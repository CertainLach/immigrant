use std::{
	cmp::Reverse,
	collections::{HashMap, HashSet},
	ops::{Deref, DerefMut},
};

use itertools::Itertools;
use rand::distributions::DistString;
use schema::{
	ids::DbIdent,
	index::Check,
	mk_change_list, mk_change_list_by_isomorph,
	names::{
		ConstraintKind, DbColumn, DbConstraint, DbEnumItem, DbForeignKey, DbIndex, DbItem, DbTable,
		DbType,
	},
	renamelist::RenameOp,
	root::Schema,
	scalar::{EnumItem, ScalarAnnotation},
	sql::Sql,
	uid::{RenameExt, RenameMap},
	w, wl, ChangeList, ColumnDiff, Diff, EnumDiff, HasDefaultDbName, HasIdent, HasUid,
	IsCompatible, IsIsomorph, SchemaDiff, SchemaEnum, SchemaItem, SchemaScalar, SchemaSql,
	SchemaTable, TableCheck, TableColumn, TableDiff, TableForeignKey, TableIndex, TablePrimaryKey,
	TableSql, TableUniqueConstraint,
};

mod process;
// mod validate;

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

impl Pg<TableColumn<'_>> {
	pub fn create_inline(&self, out: &mut String, rn: &RenameMap) {
		let name = &self.db(rn);
		let db_type = self.db_type(rn);
		let nullability = if self.nullable { "" } else { " NOT NULL" };
		let defaultability = if let Some(default) = self.default() {
			let sql = Pg(self.table).format_sql(default, rn);
			format!(" DEFAULT ({sql})")
		} else {
			"".to_owned()
		};
		w!(out, "{name} {db_type}{nullability}{defaultability}")
	}
	pub fn drop_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		let name = &self.db(rn);
		out.push(format!("DROP COLUMN {name}"))
	}
	pub fn create_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		let mut inl = String::new();
		self.create_inline(&mut inl, rn);
		out.push(format!("ADD COLUMN {inl}"))
	}
	pub fn rename_alter(&self, to: DbColumn, out: &mut Vec<String>, rn: &mut RenameMap) {
		let name = self.db(rn);
		out.push(format!("ALTER COLUMN {name} RENAME TO {to}"));
		self.set_db(rn, to);
	}
}
impl Pg<TableSql<'_>> {
	pub fn print(&self, out: &mut String, rn: &RenameMap) {
		let o = format_sql(&*self.0, &self.table.schema, Some(self.0.table), rn);
		out.push_str(&o);
	}
}

impl Pg<SchemaTable<'_>> {
	fn constraints(&self) -> Vec<PgTableConstraint> {
		let mut out = vec![];
		if let Some(pk) = self.primary_key() {
			out.push(PgTableConstraint::PrimaryKey(pk))
		}
		for check in self.checks() {
			out.push(PgTableConstraint::Check(check))
		}
		for unique in self.unique_constraints() {
			out.push(PgTableConstraint::Unique(unique))
		}
		out
	}
	fn format_sql(&self, sql: &Sql, rn: &RenameMap) -> String {
		let mut out = String::new();
		Pg(self.sql(sql)).print(&mut out, rn);
		out
	}
	pub fn create(&self, out: &mut String, rn: &RenameMap) {
		let table_name = &self.db(rn);
		wl!(out, "CREATE TABLE {table_name} (");
		let mut had = false;
		for v in self.columns() {
			if !had {
				had = true;
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			Pg(v).create_inline(out, rn);
			wl!(out, "");
		}
		if let Some(pk) = self.primary_key() {
			if !had {
				had = true
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			Pg(pk).create_inline(out, rn);
			wl!(out);
		}
		for check in self.checks() {
			if !had {
				had = true
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			Pg(check).create_inline(out, rn);
			wl!(out);
		}
		for unique in self.unique_constraints() {
			if !had {
				had = true
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			Pg(unique).create_inline(out, rn);
			wl!(out);
		}
		wl!(out, ");");
		for idx in self.indexes() {
			Pg(idx).create(out, rn);
		}
	}
	pub fn drop(&self, out: &mut String, rn: &RenameMap) {
		let table_name = &self.db(rn);
		wl!(out, "DROP TABLE {table_name};");
	}
	pub fn rename(&self, to: DbTable, out: &mut String, rn: &mut RenameMap) {
		self.print_alternations(&[format!("RENAME TO {to}")], out, rn);
		self.set_db(rn, to);
	}

	pub fn create_fks(&self, out: &mut String, rn: &RenameMap) {
		let mut alternations = Vec::new();
		for fk in self.foreign_keys() {
			alternations.push(Pg(fk).create_alter(rn));
		}
		self.print_alternations(&alternations, out, rn)
	}
	pub fn print_alternations(&self, alternations: &[String], out: &mut String, rn: &RenameMap) {
		if alternations.is_empty() {
			return;
		}
		let name = &self.db(rn);
		w!(out, "ALTER TABLE {name}\n");
		for (i, alt) in alternations.iter().enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			wl!(out, "\t{alt}");
		}
		wl!(out, ";");
	}
	// fn constraint_isomorphic_to(&self, other: TableConstraint<'_>) -> Option<TableConstraint<'_>> {
	// 	self.constraints()
	// 		.find(|i| Pg(*i).isomorphic_to(&Pg(other)))
	// }
}

impl Pg<ColumnDiff<'_>> {
	pub fn print_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		let name = &self.new.db(rn);
		let new_ty = self.new.db_type(rn);
		if self.old.db_type(rn) != new_ty {
			out.push(format!("ALTER COLUMN {name} TYPE {new_ty}"));
		}
		let new_nullable = self.new.nullable;
		if self.old.nullable != new_nullable {
			if new_nullable {
				out.push(format!("ALTER COLUMN {name} DROP NOT NULL"));
			} else {
				out.push(format!("ALTER COLUMN {name} SET NOT NULL"));
			}
		}

		let old_default = self.old.default();
		let new_default = self.new.default();
		match (old_default, new_default) {
			(None, Some(new_default)) => out.push(format!(
				"ALTER COLUMN {name} SET DEFAULT {}",
				Pg(self.new.table).format_sql(new_default, rn)
			)),
			(Some(_), None) => out.push(format!("ALTER COLUMN {name} DROP DEFAULT")),
			(Some(old_default), Some(new_default)) => {
				let old_default = Pg(self.old.table).format_sql(old_default, rn);
				let new_default = Pg(self.new.table).format_sql(new_default, rn);
				if new_default != old_default {
					out.push(format!("ALTER COLUMN {name} SET DEFAULT {new_default}",));
				}
			}
			(None, None) => {}
		}
	}
}
impl Pg<TableDiff<'_>> {
	pub fn print_stage1(
		&self,
		out: &mut String,
		rn: &mut RenameMap,
	) -> ChangeList<TableColumn<'_>> {
		let mut alternations = Vec::new();

		let old_columns = self.old.columns().collect::<Vec<_>>();
		let new_columns = self.new.columns().collect::<Vec<_>>();
		let column_changes = mk_change_list(rn, &old_columns, &new_columns);
		// Rename/moveaway columns
		{
			let mut updated = HashMap::new();
			for ele in column_changes.renamed.iter() {
				match ele {
					RenameOp::Rename(i, r) => {
						Pg(*i).rename_alter(
							DbIdent::unchecked_from(r.clone()),
							&mut alternations,
							rn,
						);
					}
					RenameOp::Store(i, t) | RenameOp::Moveaway(i, t) => {
						Pg(*i).rename_alter(t.db(), &mut alternations, rn);
						updated.insert(t, i);
					}
					RenameOp::Restore(t, n) => {
						let table = updated.remove(&t).expect("stored");
						Pg(*table).rename_alter(
							DbIdent::unchecked_from(n.clone()),
							&mut alternations,
							rn,
						);
					}
				}
			}
		}

		Pg(self.new).print_alternations(&alternations, out, rn);
		column_changes
	}

	pub fn print_stage2(
		&self,
		out: &mut String,
		rn: &mut RenameMap,
		column_changes: ChangeList<TableColumn<'_>>,
	) -> Vec<String> {
		let mut alternations = Vec::new();

		let oldpg = Pg(self.old);
		let newpg = Pg(self.new);
		let old_constraints = oldpg.constraints();
		let new_constraints = newpg.constraints();
		let constraint_changes = mk_change_list_by_isomorph(rn, &old_constraints, &new_constraints);

		// Drop/rename constraints
		for (constraint, _) in constraint_changes.dropped {
			constraint.drop_alter(&mut alternations, rn);
		}
		for ele in constraint_changes.renamed {
			let mut stored = HashMap::new();
			match ele {
				RenameOp::Rename(a, b) => {
					a.rename_alter(b, &mut alternations, rn);
				}
				RenameOp::Store(a, b) => {
					a.rename_alter(b.db(), &mut alternations, rn);
					stored.insert(b, a);
				}
				RenameOp::Restore(r, t) => {
					stored
						.remove(&r)
						.expect("stored")
						.rename_alter(t, &mut alternations, rn);
				}
				RenameOp::Moveaway(_, _) => {
					// All moveaways were dropped
				}
			}
		}

		// Drop old indexes
		let old_indexes = self.old.indexes().map(Pg).collect_vec();
		let new_indexes = self.new.indexes().map(Pg).collect_vec();
		let index_changes = mk_change_list_by_isomorph(rn, &old_indexes, &new_indexes);
		for (index, _) in index_changes.dropped {
			index.drop(out, rn);
		}

		// Create new columns
		for ele in column_changes.created {
			Pg(ele).create_alter(&mut alternations, rn);
		}

		// Update columns
		for ele in column_changes.updated {
			Pg(ele).print_alter(&mut alternations, rn);
		}

		// Update/create constraints except for foreign keys
		let mut fks = vec![];
		for constraint in constraint_changes.created {
			constraint.create_alter_non_fk(&mut alternations, rn);
			constraint.create_alter_fk(&mut fks, rn);
		}

		{
			Pg(self.new).print_alternations(&alternations, out, rn);
			alternations = vec![];
		}

		// Create/update indexes
		for added in index_changes.created {
			added.create(out, rn)
		}
		for updated in index_changes.updated {
			// There is nothing updateable in indexes except for names, `old` should be equal to `new`
			// Indexes should be always dropped and recreated
			let old_name = updated.old.db(rn);
			let new_name = updated.new.db(rn);
			updated.old.rename(new_name, out, rn)
		}

		// Drop old columns
		for (column, _) in column_changes.dropped {
			Pg(column).drop_alter(&mut alternations, rn);
		}
		Pg(self.new).print_alternations(&alternations, out, rn);

		fks
	}
}

impl Pg<TableForeignKey<'_>> {
	pub fn create_alter(&self, rn: &RenameMap) -> String {
		let mut out = String::new();

		let name = self.db(rn);
		w!(out, "ADD CONSTRAINT {name} FOREIGN KEY(");
		let source_columns = self.source_columns();
		self.table
			.print_column_list(&mut out, source_columns.into_iter(), rn);
		w!(out, ") REFERENCES ");
		let target_table = self.target_table();
		let target_columns = self.target_columns();
		let target_table_name = target_table.db(rn);
		w!(out, "{target_table_name}(");
		target_table.print_column_list(&mut out, target_columns.into_iter(), rn);
		w!(out, ")");
		if let Some(on_delete) = self.on_delete.sql() {
			w!(out, " ON DELETE {on_delete}");
		}

		out
	}
}
// impl Pg<TableConstraint<'_>> {
// 	pub fn isomorphic_to(&self, other: &Self) -> bool {
// 		match (&self.kind, &other.kind) {
// 			(ConstraintTy::PrimaryKey(a), ConstraintTy::PrimaryKey(b)) => {
// 				self.table.db_names(a.iter().copied()) == other.table.db_names(b.iter().copied())
// 			}
// 			(ConstraintTy::Unique { columns: a }, ConstraintTy::Unique { columns: b }) => {
// 				self.table.db_names(a.iter().copied()) == other.table.db_names(b.iter().copied())
// 			}
// 			(ConstraintTy::Check { sql: a }, ConstraintTy::Check { sql: b }) => {
// 				Pg(self.table).format_sql(a) == Pg(other.table).format_sql(b)
// 			}
// 			_ => false,
// 		}
// 	}
// 	pub fn create_inline(&self, out: &mut String) {
// 		use ConstraintTy::*;
// 		let name = self.assigned_name();
// 		w!(out, "CONSTRAINT {name} ");
// 		match &self.kind {
// 			PrimaryKey(columns) => {
// 				w!(out, "PRIMARY KEY(");
// 				self.table.print_column_list(out, columns.iter().copied());
// 				w!(out, ")");
// 			}
// 			Unique { columns } => {
// 				w!(out, "UNIQUE(");
// 				self.table.print_column_list(out, columns.iter().copied());
// 				w!(out, ")");
// 			}
// 			Check { sql } => {
// 				w!(out, "CHECK(");
// 				Pg(self.table.sql(sql)).print(out);
// 				w!(out, ")");
// 			}
// 		}
// 	}
// 	pub fn create_alter(&self, out: &mut Vec<String>) {
// 		let mut s = String::new();
// 		w!(s, "ADD ");
// 		self.create_inline(&mut s);
// 		out.push(s);
// 	}
// 	pub fn rename_alter(&self, new_name: DbConstraint, out: &mut Vec<String>) {
// 		let name = self.assigned_name();
// 		if name == new_name {
// 			return;
// 		}
// 		out.push(format!("RENAME CONSTRAINT {name} TO {new_name}"))
// 	}
// 	pub fn drop_alter(&self, out: &mut Vec<String>) {
// 		let name = self.name.as_ref().expect("assigned");
// 		out.push(format!("DROP CONSTRAINT {name}"))
// 	}
// }
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
	fn is_compatible(&self, new: &Self, rn: &RenameMap) -> bool {
		// We can't update unique flag, so they are not compatible and need to be recreated
		self.unique == new.unique
	}
}
impl Pg<TableIndex<'_>> {
	pub fn create(&self, out: &mut String, rn: &RenameMap) {
		let name = self.db(rn);
		let table_name = &self.table.db(rn);

		w!(out, "CREATE ");
		if self.unique {
			w!(out, "UNIQUE ");
		}
		w!(out, "INDEX {name} ON {table_name}(\n");
		for (i, c) in self.db_columns(rn).enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			w!(out, "\t{c}\n");
		}
		w!(out, ");\n");
	}
	pub fn drop(&self, out: &mut String, rn: &RenameMap) {
		let name = self.db(rn);
		w!(out, "DROP INDEX {name};\n")
	}
	pub fn rename(&self, new_name: DbIndex, out: &mut String, rn: &RenameMap) {
		let name = self.db(rn);
		if name == new_name {
			return;
		}
		w!(out, "ALTER INDEX {name} RENAME TO {new_name};\n")
	}
}
impl Pg<SchemaDiff<'_>> {
	pub fn print(&self, out: &mut String, rn: &mut RenameMap) {
		let changelist = self.changelist(rn);

		// Rename/moveaway everything
		for ele in changelist.renamed {
			let mut stored = HashMap::new();
			match ele {
				RenameOp::Store(i, t) | RenameOp::Moveaway(i, t) => {
					stored.insert(t, i);
					Pg(i).rename(t.db(), out, rn)
				}
				RenameOp::Restore(t, n) => {
					Pg(stored.remove(&t).expect("stored")).rename(n, out, rn)
				}
				RenameOp::Rename(v, n) => Pg(v).rename(n, out, rn),
			}
		}

		// Enums: print_renamed_added
		let mut dropped_enum_entires = vec![];
		for ele in changelist
			.updated
			.iter()
			.filter(|d| matches!(d.old, SchemaItem::Enum(_)))
			.map(|ele| {
				let old = ele.old.as_enum().expect("enum");
				let new = ele.new.as_enum().expect("enum");
				Diff { old, new }
			}) {
			let dropped = Pg(ele).print_renamed_added(out, rn);
			dropped_enum_entires.push((ele, dropped));
		}

		// Create new enums/scalars
		for ele in changelist.created.iter().filter_map(|i| i.as_enum()) {
			Pg(ele).create(out, rn);
		}
		for ele in changelist.created.iter().filter_map(|i| i.as_scalar()) {
			Pg(ele).create(out, rn);
		}

		// Create new tables
		for ele in changelist.created.iter().filter_map(|i| i.as_table()) {
			Pg(ele).create(out, rn);
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
			let changelist = diff.print_stage1(out, rn);
			changes.push(changelist);
		}
		let mut fkss = vec![];
		for (diff, column_changes) in diffs.iter().zip(changes.into_iter()) {
			let fks = Pg(diff).print_stage2(out, rn, column_changes);
			fkss.push(fks);
		}

		// Create new foreign keys
		for (diff, fks) in diffs.iter().zip(fkss.into_iter()) {
			Pg(diff.new).print_alternations(&fks, out, rn);
		}

		// Drop old tables
		for ele in changelist.dropped.iter().filter_map(|(i, _)| i.as_table()) {
			Pg(ele).drop(out, rn)
		}

		// Remove enum entries
		for (en, dropped) in dropped_enum_entires {
			Pg(en).print_removed(dropped, out, rn);
		}

		// Drop old enums/scalars
		for ele in changelist.dropped.iter().filter_map(|(i, _)| i.as_enum()) {
			Pg(ele).drop(out, rn);
		}
		for ele in changelist.dropped.iter().filter_map(|(i, _)| i.as_scalar()) {
			Pg(ele).drop(out, rn);
		}

		// #[derive(PartialOrd, Ord, PartialEq, Eq)]
		// enum SortOrder {
		// 	Type,
		// 	Table,
		// }
		// impl SortOrder {
		// 	fn get_for(i: &SchemaItem<'_>) -> Self {
		// 		match i {
		// 			SchemaItem::Table(_) => Self::Table,
		// 			_ => Self::Type,
		// 		}
		// 	}
		// }
		// changelist.created.sort_by_key(|c| SortOrder::get_for(c));
		//
		// #[derive(PartialOrd, Ord, PartialEq, Eq)]
		// enum UpdateSortOrder {
		// 	// Update table first, because they may have defaults, which may require removed enum entries
		// 	Table,
		// 	// May refer enum
		// 	Scalar,
		// 	Enum,
		// }
		// changelist.updated.sort_by_key(|c| match (c.old, c.new) {
		// 	(SchemaItem::Table(_), SchemaItem::Table(_)) => UpdateSortOrder::Table,
		// 	(SchemaItem::Enum(_), SchemaItem::Enum(_)) => UpdateSortOrder::Scalar,
		// 	(SchemaItem::Scalar(_), SchemaItem::Scalar(_)) => UpdateSortOrder::Enum,
		// 	(a, b) => unreachable!("invalid update: {a:?} => {b:?}"),
		// });
		//
		// changelist
		// 	.dropped
		// 	.sort_by_key(|c| Reverse(SortOrder::get_for(&c.0)));
		//
		// for ele in changelist.created {
		// 	Pg(ele).create(out, rn);
		// }
		//
		// let mut enum_changes = vec![];
		//
		// for ele in changelist.updated.iter().copied() {
		// 	match (ele.old, ele.new) {
		// 		(SchemaItem::Enum(old), SchemaItem::Enum(new)) => {
		// 			let diff = Pg(Diff { old, new });
		// 			let removed = diff.print_renamed_added(out, rn);
		// 			// FIXME: Why did I tough removals should be deferred?..
		// 			enum_changes.push((diff, removed));
		// 		}
		// 		(SchemaItem::Scalar(_), SchemaItem::Scalar(_))
		// 		| (SchemaItem::Table(_), SchemaItem::Table(_)) => {
		// 			// Updated later
		// 		}
		// 		_ => unreachable!("will fail on sort"),
		// 	}
		// }
		//
		// for ele in changelist.updated.iter().copied() {
		// 	match (ele.old, ele.new) {
		// 		(SchemaItem::Table(old), SchemaItem::Table(new)) => {
		// 			dbg!(old, new);
		// 			Pg(Diff { old, new }).print(out, rn)
		// 		}
		// 		(SchemaItem::Scalar(old), SchemaItem::Scalar(new)) => {
		// 			Pg(Diff { old, new }).print(out, rn)
		// 		}
		// 		(SchemaItem::Enum(_), SchemaItem::Enum(_)) => {}
		// 		_ => unreachable!("will fail on sort"),
		// 	}
		// }
		//
		// for (en, removed) in enum_changes {
		// 	en.print_removed(removed, out, rn);
		// }
		//
		// for (ele, _) in changelist.dropped {
		// 	Pg(ele).drop(out, rn);
		// }
	}
}
impl Pg<&Schema> {
	pub fn diff(&self, target: &Self, out: &mut String, rn: &mut RenameMap) {
		Pg(SchemaDiff {
			old: target,
			new: self,
		})
		.print(out, rn)
	}
	pub fn create(&self, out: &mut String, rn: &mut RenameMap) {
		self.diff(&Pg(&Schema::default()), out, rn)
	}
	pub fn drop(&self, out: &mut String, rn: &mut RenameMap) {
		Pg(&Schema::default()).diff(self, out, rn)
	}
}
impl Pg<SchemaEnum<'_>> {
	pub fn rename(&self, db: DbType, out: &mut String, rn: &mut RenameMap) {
		self.print_alternations(&[format!("RENAME TO {db}")], out, rn);
		self.set_db(rn, db)
	}
	pub fn create(&self, out: &mut String, rn: &RenameMap) {
		let db_name = &self.db(rn);
		w!(out, "CREATE TYPE {db_name} AS ENUM (\n");
		for (i, v) in self.items.iter().enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			w!(out, "\t'{}'\n", v.db(rn));
		}
		wl!(out, ");");
		wl!(out,);
	}
	pub fn drop(&self, out: &mut String, rn: &RenameMap) {
		let db_name = &self.db(rn);
		w!(out, "DROP TYPE {db_name};\n");
	}
	pub fn print_alternations(&self, alternations: &[String], out: &mut String, rn: &RenameMap) {
		if alternations.is_empty() {
			return;
		}
		let name = &self.db(rn);
		w!(out, "ALTER TYPE {name}\n");
		for (i, alt) in alternations.iter().enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			wl!(out, "\t{alt}");
		}
		wl!(out, ";");
	}
}
impl Pg<&EnumItem> {
	pub fn rename_alter(&self, to: DbEnumItem, rn: &mut RenameMap) -> String {
		let out = format!("RENAME VALUE '{}' TO {to}", self.db(rn));
		self.set_db(rn, to);
		out
	}
}
impl Pg<EnumDiff<'_>> {
	pub fn print_renamed_added(&self, out: &mut String, rn: &mut RenameMap) -> Vec<String> {
		let changelist = schema::mk_change_list(
			rn,
			&self.0.old.items.iter().collect::<Vec<_>>(),
			&self.0.new.items.iter().collect::<Vec<_>>(),
		);
		let mut changes = vec![];
		for el in changelist.renamed.iter().cloned() {
			let mut stored = HashMap::new();
			match el {
				RenameOp::Store(i, t) | RenameOp::Moveaway(i, t) => {
					stored.insert(t, i);
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

		for added in changelist.created.iter() {
			changes.push(format!("ADD VALUE '{}'", added.db(rn)))
		}

		Pg(self.old).print_alternations(&changes, out, rn);

		let mut changes_post = vec![];
		for (removed, _) in changelist.dropped.iter() {
			// It is not possible to remove enum element in postgres in transaction, so it is being renamed to
			// `name_removed_randomid` here, and resulting value may be removed later by non-transactional code
			let name = rand::distributions::Alphanumeric.sample_string(&mut rand::thread_rng(), 5);
			changes_post.push(format!(
				"RENAME VALUE '{}' TO '{0}_removed_{name}'",
				removed.db(rn)
			))
		}
		changes_post
	}
	// pub fn print_added(&self, out: &mut String, rn:&RenameMap) {
	// 	let added = self.added_items(rn);
	// 	let db_name = &self.new.name();
	// 	for added in added {
	// 		wl!(out, "ALTER TYPE {db_name} ADD VALUE '{added}';");
	// 	}
	// }
	//
	pub fn print_removed(&self, removed: Vec<String>, out: &mut String, rn: &RenameMap) {
		Pg(self.old).print_alternations(&removed, out, rn)
	}
}
impl Pg<SchemaScalar<'_>> {
	pub fn rename(&self, to: DbType, out: &mut String, rn: &mut RenameMap) {
		self.print_alternations(&[format!("RENAME TO {to}")], out, rn);
		self.scalar.set_db(rn, to);
	}
	pub fn print_alternations(&self, alternations: &[String], out: &mut String, rn: &RenameMap) {
		if alternations.is_empty() {
			return;
		}
		let name = &self.db(rn);
		w!(out, "ALTER DOMAIN {name}\n");
		for (i, alt) in alternations.iter().enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			wl!(out, "\t{alt}");
		}
		wl!(out, ";");
	}
	pub fn create(&self, out: &mut String, rn: &RenameMap) {
		let name = &self.db(rn);
		let ty = self.scalar.inner_type();
		w!(out, "CREATE DOMAIN {name} AS {ty}");
		for ele in &self.annotations {
			match ele {
				ScalarAnnotation::Default(d) => {
					w!(out, "\n\tDEFAULT ");
					let formatted = Pg(self.schema).format_sql(d, rn);
					w!(out, "{formatted}");
				}
				ScalarAnnotation::Check(sql) => {
					let name = sql.db(rn);
					w!(out, "\n\tCONSTRAINT {name} CHECK (");
					let formatted = Pg(self.schema).format_sql(&sql.check, rn);
					w!(out, "{formatted})")
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
		wl!(out, ";")
	}
	pub fn drop(&self, out: &mut String, rn: &RenameMap) {
		let name = &self.db(rn);
		wl!(out, "DROP DOMAIN {name};");
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
	pub fn print(&self, out: &mut String, rn: &mut RenameMap) {
		let mut new = self
			.new
			.annotations
			.iter()
			.filter_map(ScalarAnnotation::as_check)
			.map(|c| {
				let mut sql = String::new();
				Pg(self.new.schema.sql(&c.check)).print(&mut sql, rn);
				(c, sql)
			})
			.collect::<Vec<_>>();
		let old = self
			.old
			.annotations
			.iter()
			.filter_map(ScalarAnnotation::as_check)
			.collect::<Vec<_>>();
		dbg!(&old, &new);
		let mut alternations = Vec::new();
		for ann in old.iter() {
			let mut sql = String::new();
			Pg(self.old.schema.sql(&ann.check)).print(&mut sql, rn);

			if let Some((i, (c, _))) = new.iter().find_position(|(_, nsql)| nsql == &sql) {
				Pg(*ann).rename_alter(c.db(rn), &mut alternations, rn);
				new.remove(i);
			} else {
				let db = ann.db(rn);
				alternations.push(format!("DROP CONSTRAINT {db}"));
			}
		}

		for (check, _) in new.iter() {
			let db = check.db(rn);
			let mut out = format!("ADD CONSTRAINT {db} CHECK (");
			Pg(self.new.schema.sql(&check.check)).print(&mut out, rn);
			w!(out, ")");
			alternations.push(out)
		}

		Pg(self.old).print_alternations(&alternations, out, rn)
	}
}

impl Pg<SchemaItem<'_>> {
	pub fn rename(&self, to: DbItem, out: &mut String, rn: &mut RenameMap) {
		match self.0 {
			SchemaItem::Table(t) => Pg(t).rename(DbTable::unchecked_from(to), out, rn),
			SchemaItem::Enum(e) => Pg(e).rename(DbType::unchecked_from(to), out, rn),
			SchemaItem::Scalar(s) => Pg(s).rename(DbType::unchecked_from(to), out, rn),
		}
	}
	pub fn create(&self, out: &mut String, rn: &RenameMap) {
		match self.0 {
			SchemaItem::Table(t) => Pg(t).create(out, rn),
			SchemaItem::Enum(e) => Pg(e).create(out, rn),
			SchemaItem::Scalar(s) => Pg(s).create(out, rn),
		}
	}
	pub fn drop(&self, out: &mut String, rn: &RenameMap) {
		match self.0 {
			SchemaItem::Table(t) => Pg(t).drop(out, rn),
			SchemaItem::Enum(e) => Pg(e).drop(out, rn),
			SchemaItem::Scalar(s) => Pg(s).drop(out, rn),
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
		| Sql::Placeholder
		| Sql::Null => false,

		Sql::UnOp(_, _) | Sql::BinOp(_, _, _) => true,
	}
}
fn format_sql(
	sql: &Sql,
	schema: &Schema,
	table: Option<SchemaTable<'_>>,
	rn: &RenameMap,
) -> String {
	let mut out = String::new();
	match sql {
		Sql::Cast(expr, ty) => {
			let expr = format_sql(expr, schema, table, rn);
			let native_ty = schema.native_type(ty, rn);
			w!(out, "({expr})::{native_ty}");
		}
		Sql::Call(proc, args) => {
			w!(out, "{proc}(");
			for (i, arg) in args.iter().enumerate() {
				if i != 0 {
					w!(out, ", ");
				}
				let arg = format_sql(arg, schema, table, rn);
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
		Sql::Ident(c) => {
			let table = table
				.as_ref()
				.expect("only placeholder supported in schema sql");
			let native_name = table.db_name(c, rn);
			w!(out, "{native_name}");
		}
		Sql::UnOp(op, expr) => {
			let op = op.format();
			let expr = format_sql(expr, schema, table, rn);
			w!(out, "{op}({expr})");
		}
		Sql::BinOp(a, op, b) => {
			dbg!(op);
			let op = op.format();
			let va = format_sql(a, schema, table, rn);
			let vb = format_sql(b, schema, table, rn);
			if sql_needs_parens(a) {
				w!(out, "({va})")
			} else {
				w!(out, "{va}")
			}
			w!(out, " {op} ");
			if sql_needs_parens(b) {
				w!(out, "({vb})")
			} else {
				w!(out, "{vb}")
			}
		}
		Sql::Parened(a) => {
			let va = format_sql(a, schema, table, rn);
			if sql_needs_parens(a) {
				w!(out, "({va})")
			} else {
				w!(out, "{va}")
			}
		}
		Sql::Boolean(b) => {
			if *b {
				w!(out, "TRUE")
			} else {
				w!(out, "FALSE")
			}
		}
		Sql::Placeholder => {
			assert!(
				table.is_none(),
				"placeholder should be replaced on this point"
			);
			w!(out, "VALUE")
		}
		Sql::Null => w!(out, "NULL"),
	}
	out
}

impl Pg<SchemaSql<'_>> {
	pub fn print(&self, out: &mut String, rn: &RenameMap) {
		let o = format_sql(&*self.0, &self.schema, None, rn);
		out.push_str(&o);
	}
}
impl Pg<&Schema> {
	pub fn format_sql(&self, sql: &Sql, rn: &RenameMap) -> String {
		let mut out = String::new();
		Pg(self.sql(sql)).print(&mut out, rn);
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
	fn drop_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		let name = self.db(rn);
		out.push(format!("DROP CONSTRAINT {name}"));
	}
	pub fn rename_alter(&self, new_name: DbConstraint, out: &mut Vec<String>, rn: &mut RenameMap) {
		let name = self.db(rn);
		if name == new_name {
			return;
		}
		out.push(format!("RENAME CONSTRAINT {name} TO {new_name}"));
		self.set_db(rn, new_name);
	}
	pub fn create_alter_non_fk(&self, out: &mut Vec<String>, rn: &RenameMap) {
		let mut text = String::new();
		match self {
			PgTableConstraint::PrimaryKey(p) => Pg(*p).create_inline(&mut text, rn),
			PgTableConstraint::Unique(u) => Pg(*u).create_inline(&mut text, rn),
			PgTableConstraint::Check(c) => Pg(*c).create_inline(&mut text, rn),
			PgTableConstraint::ForeignKey(_) => return,
		}
		let name = self.db(rn);
		out.push(format!("ADD CONSTRAINT {name} {text}"))
	}
	pub fn create_alter_fk(&self, out: &mut Vec<String>, rn: &RenameMap) {
		let alter = match self {
			PgTableConstraint::ForeignKey(f) => Pg(*f).create_alter(rn),
			_ => return,
		};
		out.push(alter)
	}
}

impl Pg<TablePrimaryKey<'_>> {
	pub fn create_inline(&self, out: &mut String, rn: &RenameMap) {
		w!(out, "CONSTRAINT {} PRIMARY KEY(", self.db(rn));
		self.table
			.print_column_list(out, self.columns.iter().cloned(), rn);
		w!(out, ")");
	}
	pub fn drop_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		out.push(format!("DROP CONSTRAINT {}", self.db(rn)));
	}
	pub fn create_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		let mut inl = String::new();
		self.create_inline(&mut inl, rn);
		out.push(format!("ADD {inl}"));
	}
	pub fn rename_alter(&self, new_name: DbConstraint, out: &mut Vec<String>, rn: &mut RenameMap) {
		out.push(format!("RENAME CONSTRAINT {} TO {}", self.db(rn), new_name));
		self.set_db(rn, new_name)
	}
}
impl Pg<TableUniqueConstraint<'_>> {
	pub fn create_inline(&self, out: &mut String, rn: &RenameMap) {
		w!(out, "CONSTRAINT {} UNIQUE(", self.db(rn));
		self.table
			.print_column_list(out, self.columns.iter().cloned(), rn);
		wl!(out, ")");
	}
	pub fn drop_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		out.push(format!("DROP CONSTRAINT {}", self.db(rn)));
	}
}
impl Pg<TableCheck<'_>> {
	pub fn create_inline(&self, out: &mut String, rn: &RenameMap) {
		w!(out, "CONSTRAINT {} CHECK (", self.db(rn));
		Pg(self.table.sql(&self.check)).print(out, rn);
		w!(out, ")");
	}
	pub fn drop_alter(&self, out: &mut Vec<String>, rn: &RenameMap) {
		out.push(format!("DROP CONSTRAINT {}", self.db(rn)));
	}
}

impl Pg<Diff<TablePrimaryKey<'_>>> {
	fn alter_column_list(&self, out: &mut Vec<String>, rn: &mut RenameMap) {
		if self.old.db(rn) != self.new.db(rn) {
			Pg(self.old).rename_alter(self.new.db(rn), out, rn);
		}
	}
}

#[cfg(test)]
mod tests {
	use std::{fs, io::Write, path::PathBuf};

	use schema::{parser::parse, uid::RenameMap, wl, Diff};
	use tempfile::NamedTempFile;
	use tracing_test::traced_test;

	use crate::{process::default_options, Pg};

	fn test_example(name: &str) {
		let mut data = fs::read_to_string(name).expect("example read");
		let result_offset = data.find("\n!!!RESULT\n").unwrap_or(data.len());
		data.truncate(result_offset);
		let (defaults, parts) = data.split_once("!!!TEST\n").unwrap_or(("", &data));

		let defaults = defaults.strip_prefix("!!!SETUP\n").unwrap_or(defaults);

		let mut examples = parts
			.split("\n!!!UPDATE\n")
			.map(|s| s.to_owned())
			.collect::<Vec<_>>();
		examples.push(String::new());
		if !defaults.is_empty() {
			for example in examples.iter_mut() {
				if !example.is_empty() {
					example.insert(0, '\n');
				}
				example.insert_str(0, defaults);
			}
			examples.insert(0, defaults.to_owned());
		}
		// Init defaults
		examples.insert(0, String::new());
		// Drop defaults
		if !defaults.is_empty() {
			examples.push(String::new());
		}
		let mut rn = RenameMap::default();
		let examples = examples
			.iter()
			.map(|example| {
				let mut schema = match parse(example.as_str(), &default_options(), &mut rn) {
					Ok(s) => s,
					Err(e) => {
						panic!("failed to parse schema:\n{example}\n\n{e:#?}");
					}
				};
				schema
			})
			.collect::<Vec<_>>();
		let mut out = String::new();
		for (i, ele) in examples.windows(2).enumerate() {
			if i != 0 {
				wl!(out, "\n-- updated --");
			}
			let mut out_tmp = String::new();
			Pg(Diff {
				old: &ele[0],
				new: &ele[1],
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
