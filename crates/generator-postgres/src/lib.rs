use std::{
	collections::{HashMap, HashSet},
	ops::{Deref, DerefMut},
};

use itertools::Itertools;
use schema::{
	ids::{DbIdent, Ident},
	index::Check,
	mk_change_list,
	names::{ConstraintKind, DbColumn, DbConstraint, DbEnumItem, DbIndex, DbItem, DbTable, DbType},
	renamelist::RenameOp,
	root::Schema,
	scalar::{EnumItem, ScalarAnnotation},
	sql::Sql,
	uid::{RenameExt, RenameMap},
	w, wl, ChangeList, ColumnDiff, Diff, EnumDiff, HasDefaultDbName, HasIdent, HasUid,
	IsCompatible, IsIsomorph, SchemaComposite, SchemaDiff, SchemaEnum, SchemaItem, SchemaScalar,
	SchemaSql, SchemaTable, TableCheck, TableColumn, TableDiff, TableForeignKey, TableIndex,
	TablePrimaryKey, TableSql, TableUniqueConstraint,
};

pub mod validate;

#[derive(Clone, Debug)]
pub struct Pg<T>(pub T);
impl<T> Deref for Pg<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}
impl<T> DerefMut for Pg<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}
impl<T> HasUid for Pg<T>
where
	T: HasUid,
{
	fn uid(&self) -> schema::uid::Uid {
		self.0.uid()
	}
}
impl<T> HasDefaultDbName for Pg<T>
where
	T: HasDefaultDbName,
{
	type Kind = T::Kind;

	fn default_db(&self) -> Option<DbIdent<Self::Kind>> {
		self.0.default_db()
	}
}

#[derive(Debug)]
pub struct Alternation {
	groupable_up: bool,
	groupable_down: bool,
	alt: String,
}
macro_rules! alt_group {
    ($($tt:tt)*) => {
        Alternation {
				groupable_up: true,
				groupable_down: true,
				alt: format!($($tt)*),
		  }
    };
}
macro_rules! alt_ungroup {
    ($($tt:tt)*) => {
        Alternation {
				groupable_up: false,
				groupable_down: false,
				alt: format!($($tt)*),
		  }
    };
}
macro_rules! alt_ungroup_up {
    ($($tt:tt)*) => {
        Alternation {
				groupable_up: false,
				groupable_down: true,
				alt: format!($($tt)*),
		  }
    };
}

impl Pg<TableColumn<'_>> {
	pub fn create_inline(&self, sql: &mut String, rn: &RenameMap) {
		self.create_inline_inner(sql, rn, false);
	}
	fn create_inline_inner(&self, sql: &mut String, rn: &RenameMap, force_nullable: bool) {
		let name = &self.db(rn);
		let db_type = self.db_type(rn);
		let nullability = if self.nullable || force_nullable {
			""
		} else {
			" NOT NULL"
		};
		let defaultability = self.default().map_or_else(String::new, |default| {
			let sql = Pg(self.table).format_sql(default, rn);
			format!(" DEFAULT ({sql})")
		});
		w!(sql, "{name} {db_type}{nullability}{defaultability}");
	}
	pub fn drop_alter(&self, out: &mut Vec<Alternation>, rn: &RenameMap) {
		let name = &self.db(rn);
		out.push(alt_group!("DROP COLUMN {name}"));
	}
	pub fn create_alter(&self, out: &mut Vec<Alternation>, rn: &RenameMap) {
		let mut inl = String::new();
		self.create_inline_inner(&mut inl, rn, self.initialize_as().is_some());
		out.push(alt_group!("ADD COLUMN {inl}"));
		if let Some(initialize_as) = self.initialize_as() {
			let name = &self.db(rn);
			let db_type = self.db_type(rn);
			let sql = format_sql(
				initialize_as,
				self.table.schema,
				SchemaItem::Table(self.table),
				rn,
			);
			// Technically groupable, yet postgres bails with error: column "column" of relation "tests" does not exist,
			// if appears with the same statement ad ADD COLUMN.
			out.push(alt_ungroup_up!(
				"ALTER COLUMN {name} SET DATA TYPE {db_type} USING {sql}"
			));
			if !self.nullable {
				out.push(alt_group!("ALTER COLUMN {name} SET NOT NULL"));
			}
		}
	}
	/// Column renaming doesn't support grouping multiple renames in one ALTER TABLE.
	pub fn rename_alter(&self, to: DbColumn, out: &mut Vec<Alternation>, rn: &mut RenameMap) {
		let name = self.db(rn);
		out.push(alt_ungroup!("RENAME COLUMN {name} TO {to}"));
		self.set_db(rn, to);
	}
}
impl Pg<TableSql<'_>> {
	pub fn print(&self, sql: &mut String, rn: &RenameMap) {
		let o = format_sql(
			&self.0,
			self.table.schema,
			SchemaItem::Table(self.0.table),
			rn,
		);
		sql.push_str(&o);
	}
}

impl Pg<SchemaTable<'_>> {
	fn constraints(&self) -> Vec<PgTableConstraint<'_>> {
		let mut out = vec![];
		if let Some(pk) = self.primary_key() {
			out.push(PgTableConstraint::PrimaryKey(pk));
		}
		for check in self.checks() {
			out.push(PgTableConstraint::Check(check));
		}
		for unique in self.unique_constraints() {
			out.push(PgTableConstraint::Unique(unique));
		}
		for fk in self.foreign_keys() {
			out.push(PgTableConstraint::ForeignKey(fk));
		}
		out
	}
	fn format_sql(&self, sql: &Sql, rn: &RenameMap) -> String {
		let mut out = String::new();
		Pg(self.sql(sql)).print(&mut out, rn);
		out
	}
	pub fn create(&self, sql: &mut String, rn: &RenameMap) {
		let table_name = &self.db(rn);
		wl!(sql, "CREATE TABLE {table_name} (");
		let mut had = false;
		for v in self.columns() {
			if had {
				w!(sql, ",");
			} else {
				had = true;
			};
			w!(sql, "\t");
			Pg(v).create_inline(sql, rn);
			// assert!(
			// 	v.initialize_as().is_none(),
			// 	"@initialize_as may only appear when field is added, not when the table is created"
			// );
			wl!(sql, "");
		}
		let constraints = self.constraints();
		for constraint in constraints {
			let Some(inline) = constraint.create_inline_non_fk(rn) else {
				continue;
			};
			if had {
				w!(sql, ",");
			} else {
				had = true;
			};
			wl!(sql, "\t{inline}");
		}
		wl!(sql, ");");
		for idx in self.indexes() {
			Pg(idx).create(sql, rn);
		}
	}
	pub fn drop(&self, sql: &mut String, rn: &RenameMap) {
		let table_name = &self.db(rn);
		wl!(sql, "DROP TABLE {table_name};");
	}
	pub fn rename(&self, to: DbTable, sql: &mut String, rn: &mut RenameMap) {
		self.print_alternations(&[alt_ungroup!("RENAME TO {to}")], sql, rn);
		self.set_db(rn, to);
	}

	pub fn print_alternations(&self, mut out: &[Alternation], sql: &mut String, rn: &RenameMap) {
		fn print_group(name: &DbTable, list: &[Alternation], sql: &mut String) {
			if list.len() > 1 {
				w!(sql, "ALTER TABLE {name}\n");
				for (i, alt) in list.iter().enumerate() {
					if i != 0 {
						w!(sql, ",");
					};
					wl!(sql, "\t{}", alt.alt);
				}
				wl!(sql, ";");
			} else {
				let alt = &list[0];
				w!(sql, "ALTER TABLE {name} {};\n", alt.alt);
			}
		}
		let name = &self.db(rn);
		while !out.is_empty() {
			let mut count = 1;
			loop {
				if !out[count - 1].groupable_down || out.len() == count {
					break;
				}
				if !out[count].groupable_up {
					break;
				}
				count += 1;
			}
			print_group(name, &out[..count], sql);
			out = &out[count..];
		}
	}
}

impl Pg<ColumnDiff<'_>> {
	pub fn print_alter(&self, out: &mut Vec<Alternation>, rn: &RenameMap) {
		let name = &self.new.db(rn);
		let new_ty = self.new.db_type(rn);
		if self.old.db_type(rn) != new_ty {
			out.push(alt_group!("ALTER COLUMN {name} SET DATA TYPE {new_ty}"));
		}
		let new_nullable = self.new.nullable;
		if self.old.nullable != new_nullable {
			if new_nullable {
				out.push(alt_group!("ALTER COLUMN {name} DROP NOT NULL"));
			} else {
				out.push(alt_group!("ALTER COLUMN {name} SET NOT NULL"));
			}
		}

		let old_default = self.old.default();
		let new_default = self.new.default();
		match (old_default, new_default) {
			(None, Some(new_default)) => out.push(alt_group!(
				"ALTER COLUMN {name} SET DEFAULT {}",
				Pg(self.new.table).format_sql(new_default, rn)
			)),
			(Some(_), None) => out.push(alt_group!("ALTER COLUMN {name} DROP DEFAULT")),
			(Some(old_default), Some(new_default)) => {
				let old_default = Pg(self.old.table).format_sql(old_default, rn);
				let new_default = Pg(self.new.table).format_sql(new_default, rn);
				if new_default != old_default {
					out.push(alt_group!("ALTER COLUMN {name} SET DEFAULT {new_default}",));
				}
			}
			(None, None) => {}
		}

		// #[allow(clippy::match_same_arms)]
		// match (self.old.initialize_as(), self.new.initialize_as()) {
		// 	(Some(_), Some(_)) => panic!("@initialize_as may not be left on migration"),
		// 	(None, Some(_)) => {
		// 		// TODO: Disallow when generating up migration.
		// 		//panic!("@initialize_as may only be dropped")
		// 	}
		// 	(Some(_) | None, None) => {}
		// }
	}
}
impl Pg<TableDiff<'_>> {
	pub fn print_stage1(
		&self,
		sql: &mut String,
		rn: &mut RenameMap,
	) -> ChangeList<TableColumn<'_>> {
		let mut out = Vec::new();

		let old_columns = self.old.columns().collect::<Vec<_>>();
		let new_columns = self.new.columns().collect::<Vec<_>>();
		let column_changes = mk_change_list(rn, &old_columns, &new_columns);
		// Rename/moveaway columns
		{
			let mut updated = HashMap::new();
			for ele in &column_changes.renamed {
				match ele {
					RenameOp::Rename(i, r) => {
						Pg(*i).rename_alter(DbIdent::unchecked_from(r.clone()), &mut out, rn);
					}
					RenameOp::Store(i, t) | RenameOp::Moveaway(i, t) => {
						Pg(*i).rename_alter(t.db(), &mut out, rn);
						updated.insert(t, i);
					}
					RenameOp::Restore(t, n) => {
						let table = updated.remove(&t).expect("stored");
						Pg(*table).rename_alter(DbIdent::unchecked_from(n.clone()), &mut out, rn);
					}
				};
			}
		}

		Pg(self.new).print_alternations(&out, sql, rn);
		column_changes
	}

	pub fn print_stage2(
		&self,
		sql: &mut String,
		rn: &mut RenameMap,
		column_changes: ChangeList<TableColumn<'_>>,
	) -> Vec<Alternation> {
		let mut out = Vec::new();

		let oldpg = Pg(self.old);
		let newpg = Pg(self.new);
		let old_constraints = oldpg.constraints();
		let new_constraints = newpg.constraints();
		let constraint_changes = mk_change_list(rn, &old_constraints, &new_constraints);

		// Drop/rename constraints
		for constraint in constraint_changes.dropped {
			constraint.drop_alter(&mut out, rn);
		}
		for ele in constraint_changes.renamed {
			let mut stored = HashMap::new();
			match ele {
				RenameOp::Rename(a, b) => {
					a.rename_alter(b, &mut out, rn);
				}
				RenameOp::Store(a, b) => {
					a.rename_alter(b.db(), &mut out, rn);
					stored.insert(b, a);
				}
				RenameOp::Restore(r, t) => {
					stored
						.remove(&r)
						.expect("stored")
						.rename_alter(t, &mut out, rn);
				}
				RenameOp::Moveaway(_, _) => {
					// All moveaways were dropped
				}
			}
		}

		// Drop old indexes
		let old_indexes = self.old.indexes().map(Pg).collect_vec();
		let new_indexes = self.new.indexes().map(Pg).collect_vec();
		let index_changes = mk_change_list(rn, &old_indexes, &new_indexes);
		for index in index_changes.dropped {
			index.drop(sql, rn);
		}

		// Create new columns
		for ele in column_changes.created {
			Pg(ele).create_alter(&mut out, rn);
		}

		// Update columns
		for ele in column_changes.updated {
			Pg(ele).print_alter(&mut out, rn);
		}

		// Update/create constraints except for foreign keys
		let mut fks = vec![];
		for constraint in constraint_changes.created {
			constraint.create_alter_non_fk(&mut out, rn);
			constraint.create_alter_fk(&mut fks, rn);
		}

		{
			Pg(self.new).print_alternations(&out, sql, rn);
			out = vec![];
		}

		// Create/update indexes
		for added in index_changes.created {
			added.create(sql, rn);
		}
		for updated in index_changes.updated {
			// There is nothing updateable in indexes except for names, `old` should be equal to `new`
			// Indexes should be always dropped and recreated
			let _old_name = updated.old.db(rn);
			let new_name = updated.new.db(rn);
			updated.old.rename(new_name, sql, rn);
		}

		// Drop old columns
		for column in column_changes.dropped {
			Pg(column).drop_alter(&mut out, rn);
		}
		Pg(self.new).print_alternations(&out, sql, rn);

		fks
	}
}

impl Pg<TableForeignKey<'_>> {
	pub fn create_alter(&self, out: &mut Vec<Alternation>, rn: &RenameMap) {
		let mut alt = String::new();

		let name = self.db(rn);
		w!(alt, "ADD CONSTRAINT {name} FOREIGN KEY(");
		let source_columns = self.source_columns();
		self.table
			.print_column_list(&mut alt, source_columns.into_iter(), rn);
		w!(alt, ") REFERENCES ");
		let target_table = self.target_table();
		let target_columns = self.target_columns();
		let target_table_name = target_table.db(rn);
		w!(alt, "{target_table_name}(");
		target_table.print_column_list(&mut alt, target_columns.into_iter(), rn);
		w!(alt, ")");
		if let Some(on_delete) = self.on_delete.sql() {
			w!(alt, " ON DELETE {on_delete}");
		}

		out.push(Alternation {
			groupable_up: true,
			groupable_down: true,
			alt,
		});
	}
}
impl IsIsomorph for Pg<TableIndex<'_>> {
	fn is_isomorph(&self, other: &Self, rn: &RenameMap) -> bool {
		let old_fields = self.db_columns(rn).collect_vec();
		let new_fields = other.db_columns(rn).collect_vec();
		// TODO: Is it allowed to change column types, if there is an active index exists?
		// If not - then type equality should also be checked
		old_fields == new_fields
	}
}
impl IsCompatible for Pg<TableIndex<'_>> {
	fn is_compatible(&self, new: &Self, _rn: &RenameMap) -> bool {
		// We can't update unique flag, so they are not compatible and need to be recreated
		self.unique == new.unique
	}
}
impl Pg<TableIndex<'_>> {
	pub fn create(&self, sql: &mut String, rn: &RenameMap) {
		let name = self.db(rn);
		let table_name = &self.table.db(rn);

		w!(sql, "CREATE ");
		if self.unique {
			w!(sql, "UNIQUE ");
		}
		w!(sql, "INDEX {name} ON {table_name}(\n");
		for (i, c) in self.db_columns(rn).enumerate() {
			if i != 0 {
				w!(sql, ",");
			}
			w!(sql, "\t{c}\n");
		}
		w!(sql, ");\n");
	}
	pub fn drop(&self, sql: &mut String, rn: &RenameMap) {
		let name = self.db(rn);
		w!(sql, "DROP INDEX {name};\n");
	}
	pub fn rename(&self, new_name: DbIndex, sql: &mut String, rn: &RenameMap) {
		let name = self.db(rn);
		if name == new_name {
			return;
		}
		w!(sql, "ALTER INDEX {name} RENAME TO {new_name};\n");
	}
}
impl Pg<SchemaDiff<'_>> {
	#[allow(clippy::too_many_lines)]
	pub(crate) fn print(&self, sql: &mut String, rn: &mut RenameMap) {
		let changelist = self.changelist(rn);

		// Rename/moveaway everything
		for ele in changelist.renamed {
			let mut stored = HashMap::new();
			match ele {
				RenameOp::Store(i, t) | RenameOp::Moveaway(i, t) => {
					stored.insert(t, i);
					Pg(i).rename(t.db(), sql, rn);
				}
				RenameOp::Restore(t, n) => {
					Pg(stored.remove(&t).expect("stored")).rename(n, sql, rn);
				}
				RenameOp::Rename(v, n) => Pg(v).rename(n, sql, rn),
			}
		}

		// Enums: print_renamed_added
		for ele in changelist
			.updated
			.iter()
			.filter(|d| matches!(d.old, SchemaItem::Enum(_)))
			.map(|ele| {
				let old = ele.old.as_enum().expect("enum");
				let new = ele.new.as_enum().expect("enum");
				Diff { old, new }
			}) {
			Pg(ele).print_renamed_added(sql, rn);
		}

		// Create new enums/scalars
		let mut initialized_tys = HashSet::new();
		for ele in changelist.created.iter().filter_map(SchemaItem::as_enum) {
			initialized_tys.insert(ele.id());
			Pg(ele).create(sql, rn);
		}
		for ele in changelist.created.iter().filter_map(SchemaItem::as_scalar) {
			initialized_tys.insert(ele.id());
			Pg(ele).create(sql, rn);
		}
		for ele in &changelist.updated {
			initialized_tys.insert(Ident::unchecked_cast(ele.new.id()));
		}

		// Inlined scalars are always initialized
		for ele in self
			.new
			.items()
			.iter()
			.filter_map(SchemaItem::as_scalar)
			.filter(|s| s.inlined())
		{
			initialized_tys.insert(Ident::unchecked_cast(ele.id()));
		}

		// Create new composites, in toposorted order
		{
			let mut remaining_composites = changelist
				.created
				.iter()
				.filter_map(SchemaItem::as_composite)
				.collect_vec();
			if !remaining_composites.is_empty() {
				loop {
					let (ready, pending) = remaining_composites
						.iter()
						.partition::<Vec<SchemaComposite<'_>>, _>(|c| {
							c.fields().all(|f| initialized_tys.contains(&f.ty))
						});
					assert!(
						!ready.is_empty(),
						"i'm stuck (circular composite dependency?) with {} left: {pending:?}",
						pending.len()
					);
					for ele in ready {
						Pg(ele).create(sql, rn);
						initialized_tys.insert(ele.id());
					}
					if pending.is_empty() {
						break;
					}
					remaining_composites = pending;
				}
			}
		}

		for ele in changelist
			.updated
			.iter()
			.filter(|d| matches!(d.old, SchemaItem::Scalar(_)))
			.map(|ele| {
				let old = ele.old.as_scalar().expect("scalar");
				let new = ele.new.as_scalar().expect("scalar");
				Pg(Diff { old, new })
			}) {
			Pg(ele).print(sql, rn);
		}

		// Create new tables
		for ele in changelist.created.iter().filter_map(SchemaItem::as_table) {
			Pg(ele).create(sql, rn);
		}

		// Update tables
		let diffs = changelist
			.updated
			.iter()
			.filter(|d| matches!(d.old, SchemaItem::Table(_)))
			.map(|ele| {
				let old = ele.old.as_table().expect("table");
				let new = ele.new.as_table().expect("table");
				Pg(Diff { old, new })
			})
			.collect_vec();
		let mut changes = vec![];
		for diff in &diffs {
			let changelist = diff.print_stage1(sql, rn);
			changes.push(changelist);
		}
		let mut fkss = vec![];
		for (diff, column_changes) in diffs.iter().zip(changes) {
			let fks = Pg(diff).print_stage2(sql, rn, column_changes);
			fkss.push(fks);
		}

		// Create new foreign keys
		assert_eq!(diffs.len(), fkss.len());
		for (diff, fks) in diffs.iter().zip(fkss) {
			Pg(diff.new).print_alternations(&fks, sql, rn);
		}
		for ele in changelist.created.iter().filter_map(SchemaItem::as_table) {
			let mut out = Vec::new();
			let ele = Pg(ele);
			let constraints = ele.constraints();
			for constraint in constraints {
				constraint.create_alter_fk(&mut out, rn);
			}
			ele.print_alternations(&out, sql, rn);
		}

		// Drop foreign keys, both ends of which reference dropped tables
		for a in changelist.dropped.iter().filter_map(SchemaItem::as_table) {
			let mut out = Vec::new();
			'fk: for fk in a.foreign_keys() {
				for b in changelist.dropped.iter().filter_map(SchemaItem::as_table) {
					if fk.target == b.id() {
						Pg(PgTableConstraint::ForeignKey(fk)).drop_alter(&mut out, rn);
						continue 'fk;
					}
				}
			}
			Pg(a).print_alternations(&out, sql, rn);
		}

		// Drop old tables
		for ele in changelist.dropped.iter().filter_map(SchemaItem::as_table) {
			Pg(ele).drop(sql, rn);
		}

		// Drop old composites, in toposorted order
		{
			let mut remaining_composites = changelist
				.dropped
				.iter()
				.filter_map(SchemaItem::as_composite)
				.collect_vec();
			if !remaining_composites.is_empty() {
				let mut dependencies = remaining_composites
					.iter()
					.map(|c| c.id())
					.collect::<HashSet<_>>();
				loop {
					let (ready, pending) = remaining_composites
						.iter()
						.partition::<Vec<SchemaComposite<'_>>, _>(|c| {
							c.fields().all(|f| !dependencies.contains(&f.ty))
						});
					assert!(
						!ready.is_empty(),
						"i'm stuck (circular composite dependency?) with {} left: {pending:?}",
						pending.len()
					);
					for ele in ready {
						Pg(ele).drop(sql, rn);
						dependencies.remove(&ele.id());
					}
					if pending.is_empty() {
						break;
					}
					remaining_composites = pending;
				}
			}
		};

		// Drop old enums/scalars
		for ele in changelist.dropped.iter().filter_map(SchemaItem::as_enum) {
			Pg(ele).drop(sql, rn);
		}
		for ele in changelist.dropped.iter().filter_map(SchemaItem::as_scalar) {
			Pg(ele).drop(sql, rn);
		}
	}
}
impl Pg<&Schema> {
	pub fn diff(&self, target: &Self, sql: &mut String, rn: &mut RenameMap) {
		Pg(SchemaDiff {
			old: target,
			new: self,
		})
		.print(sql, rn);
	}
	pub fn create(&self, sql: &mut String, rn: &mut RenameMap) {
		self.diff(&Pg(&Schema::default()), sql, rn);
	}
	pub fn drop(&self, sql: &mut String, rn: &mut RenameMap) {
		Pg(&Schema::default()).diff(self, sql, rn);
	}
}
impl Pg<SchemaEnum<'_>> {
	pub fn rename(&self, db: DbType, sql: &mut String, rn: &mut RenameMap) {
		self.print_alternations(&[format!("RENAME TO {db}")], sql, rn);
		self.set_db(rn, db);
	}
	pub fn create(&self, sql: &mut String, rn: &RenameMap) {
		let db_name = &self.db(rn);
		w!(sql, "CREATE TYPE {db_name} AS ENUM (\n");
		for (i, v) in self.items.iter().enumerate() {
			if i != 0 {
				w!(sql, ",");
			}
			w!(sql, "\t'{}'\n", v.db(rn));
		}
		wl!(sql, ");");
		wl!(sql,);
	}
	pub fn drop(&self, sql: &mut String, rn: &RenameMap) {
		let db_name = &self.db(rn);
		w!(sql, "DROP TYPE {db_name};\n");
	}
	pub fn print_alternations(&self, out: &[String], sql: &mut String, rn: &RenameMap) {
		if out.is_empty() {
			return;
		}
		let name = &self.db(rn);
		w!(sql, "ALTER TYPE {name}\n");
		for (i, alt) in out.iter().enumerate() {
			if i != 0 {
				w!(sql, ",");
			}
			wl!(sql, "\t{alt}");
		}
		wl!(sql, ";");
	}
}
impl Pg<EnumItem> {
	pub fn rename_alter(&self, to: DbEnumItem, rn: &mut RenameMap) -> String {
		let out = format!("RENAME VALUE '{}' TO {to}", self.db(rn));
		self.set_db(rn, to);
		out
	}
}
impl Pg<EnumDiff<'_>> {
	pub fn print_renamed_added(&self, sql: &mut String, rn: &mut RenameMap) {
		let changelist =
			schema::mk_change_list(rn, &self.0.old.items.clone(), &self.0.new.items.clone());
		let mut changes = vec![];
		for el in changelist.renamed.iter().cloned() {
			let mut stored = HashMap::new();
			match el {
				RenameOp::Store(i, t) | RenameOp::Moveaway(i, t) => {
					stored.insert(t, i.clone());
					changes.push(Pg(i).rename_alter(t.db(), rn));
				}
				RenameOp::Rename(v, n) => {
					changes.push(Pg(v).rename_alter(n, rn));
				}
				RenameOp::Restore(r, n) => {
					let stored = stored.remove(&r).expect("was not stored");
					Pg(stored).rename_alter(n, rn);
				}
			}
		}

		for added in &changelist.created {
			changes.push(format!("ADD VALUE '{}'", added.db(rn)));
		}

		Pg(self.old).print_alternations(&changes, sql, rn);
		assert!(
			changelist.dropped.is_empty(),
			"enums with dropped elements are not compatible"
		);
	}
}
impl Pg<SchemaComposite<'_>> {
	pub fn rename(&self, db: DbType, sql: &mut String, rn: &mut RenameMap) {
		self.print_alternations(&[format!("RENAME TO {db}")], sql, rn);
		self.set_db(rn, db);
	}
	pub fn create(&self, sql: &mut String, rn: &RenameMap) {
		let db_name = &self.db(rn);
		w!(sql, "CREATE TYPE {db_name} AS (\n");
		for (i, v) in self.fields().enumerate() {
			if i != 0 {
				w!(sql, ",");
			}
			let db_name = v.db(rn);
			let db_type = v.db_type(rn);
			w!(sql, "\t{db_name} {db_type}\n");
		}
		wl!(sql, ");");
	}
	pub fn drop(&self, sql: &mut String, rn: &RenameMap) {
		let db_name = &self.db(rn);
		w!(sql, "DROP TYPE {db_name};\n");
	}
	pub fn print_alternations(&self, out: &[String], sql: &mut String, rn: &RenameMap) {
		if out.is_empty() {
			return;
		}
		let name = &self.db(rn);
		w!(sql, "ALTER TYPE {name}\n");
		for (i, alt) in out.iter().enumerate() {
			if i != 0 {
				w!(sql, ",");
			}
			wl!(sql, "\t{alt}");
		}
		wl!(sql, ";");
	}
}
impl Pg<SchemaScalar<'_>> {
	pub fn rename(&self, to: DbType, sql: &mut String, rn: &mut RenameMap) {
		self.print_alternations(&[format!("RENAME TO {to}")], sql, rn);
		self.scalar.set_db(rn, to);
	}
	pub fn print_alternations(&self, out: &[String], sql: &mut String, rn: &RenameMap) {
		if out.is_empty() {
			return;
		}
		let name = &self.db(rn);
		w!(sql, "ALTER DOMAIN {name}\n");
		for (i, alt) in out.iter().enumerate() {
			if i != 0 {
				w!(sql, ",");
			}
			wl!(sql, "\t{alt}");
		}
		wl!(sql, ";");
	}
	pub fn create(&self, sql: &mut String, rn: &RenameMap) {
		let name = &self.db(rn);
		let ty = self.scalar.inner_type();
		w!(sql, "CREATE DOMAIN {name} AS {ty}");
		for ele in &self.annotations {
			match ele {
				ScalarAnnotation::Default(d) => {
					w!(sql, "\n\tDEFAULT ");
					let formatted = Pg(self.schema).format_sql(d, SchemaItem::Scalar(self.0), rn);
					w!(sql, "{formatted}");
				}
				ScalarAnnotation::Check(check) => {
					let name = check.db(rn);
					w!(sql, "\n\tCONSTRAINT {name} CHECK (");
					let formatted =
						Pg(self.schema).format_sql(&check.check, SchemaItem::Scalar(self.0), rn);
					w!(sql, "{formatted})");
				}
				ScalarAnnotation::Inline => {
					unreachable!("non-material scalars are not created")
				}
				ScalarAnnotation::Index(_)
				| ScalarAnnotation::Unique(_)
				| ScalarAnnotation::PrimaryKey(_) => {
					unreachable!("only check constrains are allowed on domain scalars")
				}
			}
		}
		wl!(sql, ";");
	}
	pub fn drop(&self, sql: &mut String, rn: &RenameMap) {
		let name = &self.db(rn);
		wl!(sql, "DROP DOMAIN {name};");
	}
}
impl Pg<&Check> {
	fn rename_alter(&self, to: DbConstraint, out: &mut Vec<String>, rn: &mut RenameMap) {
		let db = self.db(rn);
		if db == to {
			return;
		}
		out.push(format!("ALTER CONSTRAINT {db} RENAME TO {to}"));
		self.set_db(rn, to);
	}
}
impl Pg<Diff<SchemaScalar<'_>>> {
	pub fn print(&self, sql: &mut String, rn: &mut RenameMap) {
		let mut new = self
			.new
			.annotations
			.iter()
			.filter_map(ScalarAnnotation::as_check)
			.map(|c| {
				let mut sql = String::new();
				Pg(self.new.schema.sql(&c.check)).print(&mut sql, SchemaItem::Scalar(self.new), rn);
				(c, sql)
			})
			.collect::<Vec<_>>();
		let old = self
			.old
			.annotations
			.iter()
			.filter_map(ScalarAnnotation::as_check)
			.collect::<Vec<_>>();
		let mut out = Vec::new();
		for ann in &old {
			let mut sql = String::new();
			Pg(self.old.schema.sql(&ann.check)).print(&mut sql, SchemaItem::Scalar(self.old), rn);

			if let Some((i, (c, _))) = new.iter().find_position(|(_, nsql)| nsql == &sql) {
				Pg(*ann).rename_alter(c.db(rn), &mut out, rn);
				new.remove(i);
			} else {
				let db = ann.db(rn);
				out.push(format!("DROP CONSTRAINT {db}"));
			}
		}

		for (check, _) in &new {
			let db = check.db(rn);
			let mut sql = format!("ADD CONSTRAINT {db} CHECK (");
			Pg(self.new.schema.sql(&check.check)).print(&mut sql, SchemaItem::Scalar(self.new), rn);
			w!(sql, ")");
			out.push(sql);
		}

		Pg(self.old).print_alternations(&out, sql, rn);
	}
}

impl Pg<SchemaItem<'_>> {
	pub fn rename(&self, to: DbItem, sql: &mut String, rn: &mut RenameMap) {
		match self.0 {
			SchemaItem::Table(t) => Pg(t).rename(DbTable::unchecked_from(to), sql, rn),
			SchemaItem::Enum(e) => Pg(e).rename(DbType::unchecked_from(to), sql, rn),
			SchemaItem::Scalar(s) => Pg(s).rename(DbType::unchecked_from(to), sql, rn),
			SchemaItem::Composite(c) => Pg(c).rename(DbType::unchecked_from(to), sql, rn),
		}
	}
	pub fn create(&self, sql: &mut String, rn: &RenameMap) {
		match self.0 {
			SchemaItem::Table(t) => Pg(t).create(sql, rn),
			SchemaItem::Enum(e) => Pg(e).create(sql, rn),
			SchemaItem::Scalar(s) => Pg(s).create(sql, rn),
			SchemaItem::Composite(c) => Pg(c).create(sql, rn),
		}
	}
	pub fn drop(&self, sql: &mut String, rn: &RenameMap) {
		match self.0 {
			SchemaItem::Table(t) => Pg(t).drop(sql, rn),
			SchemaItem::Enum(e) => Pg(e).drop(sql, rn),
			SchemaItem::Scalar(s) => Pg(s).drop(sql, rn),
			SchemaItem::Composite(c) => Pg(c).drop(sql, rn),
		}
	}
}

fn sql_needs_parens(sql: &Sql) -> bool {
	match sql {
		Sql::Cast(_, _)
		| Sql::Call(_, _)
		| Sql::String(_)
		| Sql::Number(_)
		| Sql::Ident(_)
		| Sql::Parened(_)
		| Sql::Boolean(_)
		| Sql::GetField(_, _)
		| Sql::Placeholder
		| Sql::Tuple(_)
		| Sql::Null => false,

		Sql::UnOp(_, _) | Sql::BinOp(_, _, _) | Sql::If(_, _, _) => true,
	}
}
fn format_sql(sql: &Sql, schema: &Schema, context: SchemaItem<'_>, rn: &RenameMap) -> String {
	let mut out = String::new();
	match sql {
		Sql::Cast(expr, ty) => {
			let expr = format_sql(expr, schema, context, rn);
			let native_ty = schema.native_type(ty, rn);
			w!(out, "({expr})::{native_ty}");
		}
		Sql::Call(proc, args) => {
			w!(out, "{proc}(");
			for (i, arg) in args.iter().enumerate() {
				if i != 0 {
					w!(out, ", ");
				}
				let arg = format_sql(arg, schema, context, rn);
				w!(out, "{arg}");
			}
			w!(out, ")");
		}
		Sql::String(s) => {
			w!(out, "'{s}'");
		}
		Sql::Number(n) => {
			w!(out, "{n}");
		}
		Sql::Ident(_) => {
			let native_name = sql.ident_name(&context, rn);
			w!(out, "{native_name}");
		}
		Sql::UnOp(op, expr) => {
			let op = op.format();
			let expr = format_sql(expr, schema, context, rn);
			w!(out, "{op}({expr})");
		}
		Sql::BinOp(a, op, b) => {
			let op = op.format();
			let va = format_sql(a, schema, context, rn);
			let vb = format_sql(b, schema, context, rn);
			if sql_needs_parens(a) {
				w!(out, "({va})");
			} else {
				w!(out, "{va}");
			}
			w!(out, " {op} ");
			if sql_needs_parens(b) {
				w!(out, "({vb})");
			} else {
				w!(out, "{vb}");
			}
		}
		Sql::Parened(a) => {
			let va = format_sql(a, schema, context, rn);
			if sql_needs_parens(a) {
				w!(out, "({va})");
			} else {
				w!(out, "{va}");
			}
		}
		Sql::Boolean(b) => {
			if *b {
				w!(out, "TRUE");
			} else {
				w!(out, "FALSE");
			}
		}
		Sql::Placeholder => {
			match context {
				SchemaItem::Table(_) => panic!("placeholder should be replaced on this point"),
				SchemaItem::Enum(_) => panic!("enums have no sql items"),
				SchemaItem::Scalar(_) => w!(out, "VALUE"),
				SchemaItem::Composite(_) => panic!("composite checks should be inlined"),
			};
		}
		Sql::Null => w!(out, "NULL"),
		Sql::GetField(f, c) => {
			let va = format_sql(f, schema, context, rn);
			let name = f.field_name(&context, *c, rn);
			w!(out, "({va}).{name}");
		}
		Sql::Tuple(t) => {
			w!(out, "ROW(");
			for (i, v) in t.iter().enumerate() {
				if i != 0 {
					w!(out, ", ");
				}
				let va = format_sql(v, schema, context, rn);
				w!(out, "{va}");
			}
			w!(out, ")");
		}
		Sql::If(cond, then, else_) => {
			let cond = format_sql(cond, schema, context, rn);
			let then = format_sql(then, schema, context, rn);
			let else_ = format_sql(else_, schema, context, rn);
			w!(out, "CASE WHEN ({cond}) THEN ({then}) ELSE ({else_}) END");
		}
	}
	out
}

impl Pg<SchemaSql<'_>> {
	pub fn print(&self, sql: &mut String, context: SchemaItem<'_>, rn: &RenameMap) {
		let o = format_sql(&self.0, self.schema, context, rn);
		sql.push_str(&o);
	}
}
impl Pg<&Schema> {
	pub fn format_sql(&self, sql: &Sql, context: SchemaItem<'_>, rn: &RenameMap) -> String {
		let mut out = String::new();
		Pg(self.sql(sql)).print(&mut out, context, rn);
		out
	}
}

#[derive(Clone, Copy, Debug)]
enum PgTableConstraint<'s> {
	PrimaryKey(TablePrimaryKey<'s>),
	Unique(TableUniqueConstraint<'s>),
	Check(TableCheck<'s>),
	ForeignKey(TableForeignKey<'s>),
}
impl HasUid for PgTableConstraint<'_> {
	fn uid(&self) -> schema::uid::Uid {
		match self {
			PgTableConstraint::PrimaryKey(p) => p.uid(),
			PgTableConstraint::Unique(u) => u.uid(),
			PgTableConstraint::Check(c) => c.uid(),
			PgTableConstraint::ForeignKey(f) => f.uid(),
		}
	}
}
impl HasDefaultDbName for PgTableConstraint<'_> {
	type Kind = ConstraintKind;

	fn default_db(&self) -> Option<DbIdent<Self::Kind>> {
		match self {
			PgTableConstraint::PrimaryKey(p) => p.default_db(),
			PgTableConstraint::Unique(u) => u.default_db(),
			PgTableConstraint::Check(c) => c.default_db(),
			PgTableConstraint::ForeignKey(f) => f.default_db().map(DbIdent::unchecked_from),
		}
	}
}
impl IsIsomorph for PgTableConstraint<'_> {
	fn is_isomorph(&self, other: &Self, rn: &RenameMap) -> bool {
		match (self, other) {
			(PgTableConstraint::PrimaryKey(a), PgTableConstraint::PrimaryKey(b)) => {
				// There is only one pk per table, its just makes sense
				a.db(rn) == b.db(rn)
			}
			(PgTableConstraint::Unique(a), PgTableConstraint::Unique(b)) => {
				let mut a_columns = a.table.db_names(a.columns.clone(), rn);
				a_columns.sort();
				let mut b_columns = b.table.db_names(b.columns.clone(), rn);
				b_columns.sort();
				// It makes little sense to have multiple unique constraints with the same set of columns, except if it used as implicit index.
				// Anyway, lets try to recreate/rename that.
				a.db(rn) == b.db(rn) || a_columns == b_columns
			}
			(PgTableConstraint::Check(a), PgTableConstraint::Check(b)) => {
				let mut sql_a = String::new();
				Pg(a.table.sql(&a.check)).print(&mut sql_a, rn);
				let mut sql_b = String::new();
				Pg(b.table.sql(&b.check)).print(&mut sql_b, rn);
				a.db(rn) == b.db(rn) || sql_a == sql_b
			}
			(PgTableConstraint::ForeignKey(a), PgTableConstraint::ForeignKey(b)) => {
				if a.db(rn) == b.db(rn) {
					return true;
				}
				if a.target_table().db(rn) != b.target_table().db(rn) {
					return false;
				}
				let mut a_source_columns = a.source_db_columns(rn);
				let mut a_target_columns = a.target_db_columns(rn);
				let mut b_source_columns = b.source_db_columns(rn);
				let mut b_target_columns = b.target_db_columns(rn);
				let mut a_perm_by_source = permutation::sort(&a_source_columns);
				a_perm_by_source.apply_slice_in_place(&mut a_source_columns);
				a_perm_by_source.apply_slice_in_place(&mut a_target_columns);
				let mut b_perm_by_source = permutation::sort(&b_source_columns);
				b_perm_by_source.apply_slice_in_place(&mut b_source_columns);
				b_perm_by_source.apply_slice_in_place(&mut b_target_columns);
				if a_source_columns == b_source_columns && a_target_columns == b_target_columns {
					return true;
				}

				let mut a_perm_by_target = permutation::sort(&a_target_columns);
				a_perm_by_target.apply_slice_in_place(&mut a_source_columns);
				a_perm_by_target.apply_slice_in_place(&mut a_target_columns);
				let mut b_perm_by_target = permutation::sort(&b_target_columns);
				b_perm_by_target.apply_slice_in_place(&mut b_source_columns);
				b_perm_by_target.apply_slice_in_place(&mut b_target_columns);
				if a_source_columns == b_source_columns && a_target_columns == b_target_columns {
					return true;
				}

				false
			}
			_ => false,
		}
	}
}
// Constraints are not updateable, except for foreign keys... But they are kinda special.
impl IsCompatible for PgTableConstraint<'_> {
	fn is_compatible(&self, new: &Self, rn: &RenameMap) -> bool {
		match (self, new) {
			(PgTableConstraint::PrimaryKey(a), PgTableConstraint::PrimaryKey(b)) => {
				let a_columns = a.table.db_names(a.columns.clone(), rn);
				let b_columns = b.table.db_names(b.columns.clone(), rn);
				a_columns == b_columns
			}
			(PgTableConstraint::Unique(a), PgTableConstraint::Unique(b)) => {
				let a_columns = a.table.db_names(a.columns.clone(), rn);
				let b_columns = b.table.db_names(b.columns.clone(), rn);
				a_columns == b_columns
			}
			(PgTableConstraint::Check(a), PgTableConstraint::Check(b)) => {
				let mut sql_a = String::new();
				Pg(a.table.sql(&a.check)).print(&mut sql_a, rn);
				let mut sql_b = String::new();
				Pg(b.table.sql(&b.check)).print(&mut sql_b, rn);
				sql_a == sql_b
			}
			(PgTableConstraint::ForeignKey(a), PgTableConstraint::ForeignKey(b)) => {
				assert_eq!(
					a.target_table().db(rn),
					b.target_table().db(rn),
					"rejected by isomorp test"
				);
				let a_source_columns = a.source_db_columns(rn);
				let b_source_columns = b.source_db_columns(rn);
				let a_target_columns = a.target_db_columns(rn);
				let b_target_columns = b.target_db_columns(rn);
				a_source_columns == b_source_columns && a_target_columns == b_target_columns
			}
			_ => unreachable!("non-isomorphs are rejected"),
		}
	}
}
impl PgTableConstraint<'_> {
	fn drop_alter(&self, out: &mut Vec<Alternation>, rn: &RenameMap) {
		let name = self.db(rn);
		out.push(alt_group!("DROP CONSTRAINT {name}"));
	}
	pub fn rename_alter(
		&self,
		new_name: DbConstraint,
		out: &mut Vec<Alternation>,
		rn: &mut RenameMap,
	) {
		let name = self.db(rn);
		if name == new_name {
			return;
		}
		out.push(alt_ungroup!("RENAME CONSTRAINT {name} TO {new_name}"));
		self.set_db(rn, new_name);
	}
	pub fn create_alter_non_fk(&self, out: &mut Vec<Alternation>, rn: &RenameMap) {
		let text = match self {
			PgTableConstraint::PrimaryKey(p) => Pg(*p).create_inline(rn),
			PgTableConstraint::Unique(u) => Pg(*u).create_inline(rn),
			PgTableConstraint::Check(c) => Pg(*c).create_inline(rn),
			PgTableConstraint::ForeignKey(_) => return,
		};
		out.push(alt_group!("ADD {text}"));
	}
	pub fn create_alter_fk(&self, out: &mut Vec<Alternation>, rn: &RenameMap) {
		if let PgTableConstraint::ForeignKey(f) = self {
			Pg(*f).create_alter(out, rn);
		};
	}
	pub fn create_inline_non_fk(&self, rn: &RenameMap) -> Option<String> {
		Some(match self {
			PgTableConstraint::PrimaryKey(pk) => Pg(*pk).create_inline(rn),
			PgTableConstraint::Unique(u) => Pg(*u).create_inline(rn),
			PgTableConstraint::Check(c) => Pg(*c).create_inline(rn),
			PgTableConstraint::ForeignKey(_) => return None,
		})
	}
}

impl Pg<TablePrimaryKey<'_>> {
	pub fn create_inline(&self, rn: &RenameMap) -> String {
		let mut sql = String::new();
		w!(sql, "CONSTRAINT {} PRIMARY KEY(", self.db(rn));
		self.table
			.print_column_list(&mut sql, self.columns.iter().copied(), rn);
		w!(sql, ")");
		sql
	}
	pub fn drop_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		out.push(format!("DROP CONSTRAINT {}", self.db(rn)));
	}
	pub fn rename_alter(&self, new_name: DbConstraint, out: &mut Vec<String>, rn: &mut RenameMap) {
		out.push(format!("RENAME CONSTRAINT {} TO {}", self.db(rn), new_name));
		self.set_db(rn, new_name);
	}
}
impl Pg<TableUniqueConstraint<'_>> {
	pub fn create_inline(&self, rn: &RenameMap) -> String {
		let mut sql = String::new();
		w!(sql, "CONSTRAINT {} UNIQUE(", self.db(rn));
		self.table
			.print_column_list(&mut sql, self.columns.iter().copied(), rn);
		wl!(sql, ")");
		sql
	}
	pub fn drop_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		out.push(format!("DROP CONSTRAINT {}", self.db(rn)));
	}
}
impl Pg<TableCheck<'_>> {
	pub fn create_inline(&self, rn: &RenameMap) -> String {
		let mut sql = String::new();
		w!(sql, "CONSTRAINT {} CHECK (", self.db(rn));
		Pg(self.table.sql(&self.check)).print(&mut sql, rn);
		w!(sql, ")");
		sql
	}
	pub fn drop_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		out.push(format!("DROP CONSTRAINT {}", self.db(rn)));
	}
}

#[cfg(test)]
mod tests {
	use std::{fs, io::Write, path::PathBuf};

	use schema::{
		parser::parse, process::NamingConvention, root::SchemaProcessOptions, uid::RenameMap, wl,
		Diff,
	};
	use tempfile::NamedTempFile;
	use tracing_test::traced_test;

	use crate::Pg;

	pub fn default_options() -> SchemaProcessOptions {
		SchemaProcessOptions {
			generator_supports_domain: true,
			naming_convention: NamingConvention::Postgres,
		}
	}

	fn test_example(name: &str) {
		#[derive(Debug)]
		struct Update {
			description: String,
			schema: String,
		}

		let mut data = fs::read_to_string(name).expect("example read");
		let result_offset = data.find("\n!!!RESULT\n").unwrap_or(data.len());
		data.truncate(result_offset);
		let (defaults, parts) = data.split_once("!!!TEST").unwrap_or(("", &data));

		let defaults = defaults.strip_prefix("!!!SETUP\n").unwrap_or(defaults);

		let mut examples = parts
			.split("\n!!!UPDATE")
			.map(|s| {
				let (description, text) = s.split_once('\n').unwrap_or((s, ""));
				Update {
					description: description.trim().to_string(),
					schema: text.to_string(),
				}
			})
			.collect::<Vec<_>>();
		examples.push(Update {
			description: "cleanup schema changes".to_owned(),
			schema: String::new(),
		});
		if !defaults.is_empty() {
			for example in &mut examples {
				if !example.schema.is_empty() {
					example.schema.insert(0, '\n');
				}
				example.schema.insert_str(0, defaults);
			}
			examples.insert(
				0,
				Update {
					description: "setup".to_owned(),
					schema: defaults.to_owned(),
				},
			);
		}
		// Init defaults
		examples.insert(
			0,
			Update {
				description: "in the beginning there was nothing (doesn't exist in output)"
					.to_owned(),
				schema: String::new(),
			},
		);
		// Drop defaults
		if !defaults.is_empty() {
			examples.push(Update {
				description: "cleanup setup".to_owned(),
				schema: String::new(),
			});
		}
		let mut rn = RenameMap::default();
		let examples = examples
			.iter()
			.map(|example| {
				let schema = match parse(example.schema.as_str(), &default_options(), &mut rn) {
					Ok(s) => s,
					Err(e) => {
						for e in &e {
							match e {
								schema::parser::ParsingError::Peg(e) => {
									eprintln!(
										"buffer start: {}",
										&example.schema.as_str()[e.location.offset..]
									);
								}
							}
						}
						panic!("failed to parse schema:\n{}\n\n{e:#?}", example.schema);
					}
				};
				(example.description.clone(), schema)
			})
			.collect::<Vec<_>>();
		let mut out = String::new();
		for ele in examples.windows(2) {
			let description = &ele[1].0;
			if description.is_empty() {
				wl!(out, "\n-- updated --");
			} else {
				wl!(out, "\n-- updated: {description} --");
			}
			let mut out_tmp = String::new();
			Pg(Diff {
				old: &ele[0].1,
				new: &ele[1].1,
			})
			.print(&mut out_tmp, &mut rn);
			out.push_str(out_tmp.trim());
		}
		let output = format!("{}\n!!!RESULT\n{}", data.trim(), out.trim());
		let mut named_file =
			NamedTempFile::new_in(PathBuf::from(name).parent().expect("parent")).expect("new temp");
		named_file.write_all(output.as_bytes()).expect("write");
		named_file.persist(name).expect("persist");
	}
	include!(concat!(env!("OUT_DIR"), "/example_tests.rs"));
}
