use std::{
	cmp::Reverse,
	collections::HashMap,
	ops::{Deref, DerefMut},
};

use rand::distributions::DistString;
use schema::{
	names::{DbConstraint, DbEnumItem, DbForeignKey, DbIndex, DbItem, DbTable, DbType},
	renamelist::RenameOp,
	root::Schema,
	scalar::{EnumItem, ScalarAnnotation},
	sql::Sql,
	w, wl, ColumnDiff, Diff, EnumDiff, SchemaDiff, SchemaEnum, SchemaItem, SchemaScalar, SchemaSql,
	SchemaTable, TableCheck, TableColumn, TableDiff, TableForeignKey, TableIndex, TablePrimaryKey,
	TableSql, TableUniqueConstraint,
};

mod process;
mod validate;

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

impl Pg<TableColumn<'_>> {
	pub fn create_inline(&self, out: &mut String) {
		let name = &self.name;
		let db_type = self.db_type();
		let nullability = if self.nullable { "" } else { " NOT NULL" };
		let defaultability = if let Some(default) = self.default() {
			let sql = Pg(self.table).format_sql(default);
			format!(" DEFAULT ({sql})")
		} else {
			"".to_owned()
		};
		w!(out, "{name} {db_type}{nullability}{defaultability}")
	}
	pub fn drop_alter(&self, out: &mut Vec<String>) {
		let name = &self.name;
		out.push(format!("DROP COLUMN {name}"))
	}
	pub fn create_alter(&self, out: &mut Vec<String>) {
		let mut inl = String::new();
		self.create_inline(&mut inl);
		out.push(format!("ADD COLUMN {inl}"))
	}
}
impl Pg<TableSql<'_>> {
	pub fn print(&self, out: &mut String) {
		match &*self.0 {
			Sql::Cast(expr, ty) => {
				let expr = Pg(self.table).format_sql(expr);
				let native_ty = self.table.schema.native_type(ty);
				w!(out, "({expr})::{native_ty}");
			}
			Sql::Call(proc, args) => {
				w!(out, "{proc}(");
				for (i, arg) in args.iter().enumerate() {
					if i != 0 {
						w!(out, ", ");
					}
					let arg = Pg(self.table).format_sql(arg);
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
				let native_name = self.table.db_name(c);
				w!(out, "{native_name}");
			}
			Sql::UnOp(op, expr) => {
				let op = op.format();
				let expr = Pg(self.table).format_sql(expr);
				w!(out, "{op}({expr})");
			}
			Sql::BinOp(a, op, b) => {
				let op = op.format();
				let a = Pg(self.table).format_sql(a);
				let b = Pg(self.table).format_sql(b);
				w!(out, "({a}) {op} ({b})")
			}
			Sql::Parened(a) => {
				let a = Pg(self.table).format_sql(a);
				w!(out, "({a})")
			}
			Sql::Boolean(b) => {
				if *b {
					w!(out, "TRUE")
				} else {
					w!(out, "FALSE")
				}
			}
			Sql::Placeholder => unreachable!("placeholder should be replaced on this point"),
			Sql::Null => w!(out, "NULL"),
		}
	}
}

impl Pg<SchemaTable<'_>> {
	fn format_sql(&self, sql: &Sql) -> String {
		let mut out = String::new();
		Pg(self.sql(sql)).print(&mut out);
		out
	}
	pub fn create(&self, out: &mut String) {
		let table_name = &self.name();
		wl!(out, "CREATE TABLE {table_name} (");
		let mut had = false;
		for v in self.columns() {
			if !had {
				had = true;
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			Pg(v).create_inline(out);
			wl!(out, "");
		}
		if let Some(pk) = self.primary_key() {
			if !had {
				had = true
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			Pg(pk).create_inline(out);
			wl!(out);
		}
		for check in self.checks() {
			if !had {
				had = true
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			Pg(check).create_inline(out);
			wl!(out);
		}
		for unique in self.unique_constraints() {
			if !had {
				had = true
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			Pg(unique).create_inline(out);
			wl!(out);
		}
		wl!(out, ");");
		for idx in self.indexes() {
			Pg(idx).create(out);
		}
		w!(out, "\n");
	}
	pub fn drop(&self, out: &mut String) {
		let table_name = &self.name();
		wl!(out, "DROP TABLE {table_name};");
	}
	pub fn rename(&self, to: DbTable, out: &mut String) {
		self.print_alternations(&[format!("RENAME TO {to}")], out);
		self.set_db(to);
	}

	pub fn print_drop_foreign_keys(&self, out: &mut String) {
		let mut alternations = Vec::new();
		for fk in self.foreign_keys() {
			alternations.push(Pg(fk).drop_alter());
		}
		self.print_alternations(&alternations, out);
	}
	pub fn create_fks(&self, out: &mut String) {
		let mut alternations = Vec::new();
		for fk in self.foreign_keys() {
			alternations.push(Pg(fk).create_alter());
		}
		self.print_alternations(&alternations, out)
	}
	pub fn print_alternations(&self, alternations: &[String], out: &mut String) {
		if alternations.is_empty() {
			return;
		}
		let name = &self.name();
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
	pub fn print_alter(&self, out: &mut Vec<String>) {
		let name = &self.new.name;
		let new_ty = self.new.db_type();
		if self.old.db_type() != new_ty {
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
				Pg(self.new.table).format_sql(new_default)
			)),
			(Some(_), None) => out.push(format!("ALTER COLUMN {name} DROP DEFAULT")),
			(Some(old_default), Some(new_default)) => {
				let old_default = Pg(self.old.table).format_sql(old_default);
				let new_default = Pg(self.new.table).format_sql(new_default);
				if new_default != old_default {
					out.push(format!("ALTER COLUMN {name} SET DEFAULT {new_default}",));
				}
			}
			(None, None) => {}
		}
	}
}
impl Pg<TableDiff<'_>> {
	pub fn print_renamed_or_fropped_foreign_keys(&self, out: &mut String) {
		let mut alternations = Vec::new();
		for old_fk in self.old.foreign_keys() {
			if let Some(new_fk) = Pg(self.new).foreign_key_isomophic_to(old_fk) {
				if let Some(ren) = Pg(old_fk).rename_alter(Pg(new_fk).db_name()) {
					alternations.push(ren);
				}
			} else {
				alternations.push(Pg(old_fk).drop_alter())
			}
		}
		Pg(self.new).print_alternations(&alternations, out)
	}
	pub fn print_added_foreign_keys(&self, out: &mut String) {
		let mut alternations = Vec::new();
		for new_fk in self.new.foreign_keys() {
			if self.old.foreign_key_isomophic_to(new_fk).is_none() {
				alternations.push(Pg(new_fk).create_alter())
			}
		}
		Pg(self.new).print_alternations(&alternations, out)
	}
	pub fn print(&self, out: &mut String) {
		for old_idx in self.old.indexes() {
			if let Some(new_idx) = self.new.index_isomophic_to(old_idx) {
				Pg(old_idx).rename(new_idx.assigned_name(), out)
			} else {
				Pg(old_idx).drop(out)
			}
		}
		let mut alternations = Vec::new();
		match (self.old.primary_key(), self.new.primary_key()) {
			(Some(pk), None) => Pg(pk).drop_alter(&mut alternations),
			(Some(old), Some(new)) => {
				if old.assigned_name() != new.assigned_name() {
					Pg(old).rename_alter(new.assigned_name(), &mut alternations)
				}
			}
			(None, None | Some(_)) => {}
		}
		// for old_constraint in self.old.constraints() {
		// 	if let Some(new_constraint) = Pg(self.new).constraint_isomorphic_to(old_constraint) {
		// 		Pg(old_constraint).rename_alter(new_constraint.assigned_name(), &mut alternations);
		// 	} else {
		// 		Pg(old_constraint).drop_alter(&mut alternations);
		// 	}
		// }
		for old_column in self.old.columns() {
			if let Some(new_column) = self.new.column(&old_column.name.db()) {
				Pg(ColumnDiff {
					old: old_column,
					new: new_column,
				})
				.print_alter(&mut alternations);
			} else {
				Pg(old_column).drop_alter(&mut alternations);
			}
		}
		for new_column in self.new.columns() {
			if self.old.column(&new_column.name.db()).is_none() {
				Pg(new_column).create_alter(&mut alternations)
			}
		}
		match (self.old.primary_key(), self.new.primary_key()) {
			(None, Some(v)) => Pg(v).create_alter(&mut alternations),
			(Some(old), Some(new)) => Pg(Diff { old, new }).alter(&mut alternations),
			(Some(_) | None, None) => {}
		};
		// for new_constraint in self.new.constraints() {
		// 	if Pg(self.old)
		// 		.constraint_isomorphic_to(new_constraint)
		// 		.is_none()
		// 	{
		// 		Pg(new_constraint).create_alter(&mut alternations);
		// 	}
		// }
		Pg(self.new).print_alternations(&alternations, out);
		for new_idx in self.new.indexes() {
			if self.old.index_isomophic_to(new_idx).is_none() {
				Pg(new_idx).create(out)
			}
		}
	}
}

impl Pg<TableForeignKey<'_>> {
	pub fn partial_name(&self) -> String {
		if let Some(name) = &self.name {
			return name.to_owned();
		}
		let mut out = String::new();
		for column in self.source_db_columns() {
			w!(out, "{column}_");
		}
		w!(out, "fk");
		out
	}
	pub fn db_name(&self) -> DbForeignKey {
		let partial_name = self.partial_name();
		let table_name = &self.table.name();
		DbForeignKey::new(&format!("{table_name}_{partial_name}"))
	}
	pub fn create_alter(&self) -> String {
		let mut out = String::new();

		let name = self.db_name();
		w!(out, "ADD CONSTRAINT {name} FOREIGN KEY(");
		let source_columns = self.source_columns();
		self.table
			.print_column_list(&mut out, source_columns.into_iter());
		w!(out, ") REFERENCES ");
		let target_table = self.target_table();
		let target_columns = self.target_columns();
		let target_table_name = target_table.name().db();
		w!(out, "{target_table_name}(");
		target_table.print_column_list(&mut out, target_columns.into_iter());
		w!(out, ")");
		if let Some(on_delete) = self.on_delete.sql() {
			w!(out, " ON DELETE {on_delete}");
		}

		out
	}
	pub fn drop_alter(&self) -> String {
		let name = self.db_name();
		format!("DROP CONSTRAINT {name}")
	}
	pub fn rename_alter(&self, new_name: DbForeignKey) -> Option<String> {
		let name = self.db_name();
		if name == new_name {
			return None;
		}
		Some(format!("RENAME CONSTRAINT {name} TO {new_name}"))
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
impl Pg<TableIndex<'_>> {
	pub fn create(&self, out: &mut String) {
		let name = self.assigned_name();
		let table_name = &self.table.name();

		w!(out, "CREATE ");
		if self.unique {
			w!(out, "UNIQUE ");
		}
		w!(out, "INDEX {name} ON {table_name}(\n");
		for (i, c) in self.db_columns().enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			w!(out, "\t{c}\n");
		}
		w!(out, ");\n");
	}
	pub fn drop(&self, out: &mut String) {
		let name = self.assigned_name();
		w!(out, "DROP INDEX {name};\n")
	}
	pub fn rename(&self, new_name: DbIndex, out: &mut String) {
		let name = self.assigned_name();
		if name == new_name {
			return;
		}
		w!(out, "ALTER INDEX {name} RENAME TO {new_name};\n")
	}
}
impl Pg<SchemaDiff<'_>> {
	pub fn print(&self, out: &mut String) {
		let mut changelist = self.changelist();

		#[derive(PartialOrd, Ord, PartialEq, Eq)]
		enum SortOrder {
			Type,
			Table,
		}
		impl SortOrder {
			fn get_for(i: &SchemaItem<'_>) -> Self {
				match i {
					SchemaItem::Table(_) => Self::Table,
					_ => Self::Type,
				}
			}
		}
		changelist.created.sort_by_key(|c| SortOrder::get_for(c));

		#[derive(PartialOrd, Ord, PartialEq, Eq)]
		enum UpdateSortOrder {
			// Update table first, because they may have defaults, which may require removed enum entries
			Table,
			// May refer enum
			Scalar,
			Enum,
		}
		changelist.updated.sort_by_key(|c| match (c.old, c.new) {
			(SchemaItem::Table(_), SchemaItem::Table(_)) => UpdateSortOrder::Table,
			(SchemaItem::Enum(_), SchemaItem::Enum(_)) => UpdateSortOrder::Scalar,
			(SchemaItem::Scalar(_), SchemaItem::Scalar(_)) => UpdateSortOrder::Enum,
			(a, b) => unreachable!("invalid update: {a:?} => {b:?}"),
		});

		changelist
			.dropped
			.sort_by_key(|c| Reverse(SortOrder::get_for(&c.0)));
		for ele in changelist.renamed {
			let mut stored = HashMap::new();
			match ele {
				RenameOp::Store(i, t) => {
					stored.insert(t, i);
					Pg(i).rename(t.db(), out)
				}
				RenameOp::Restore(t, n) => Pg(stored.remove(&t).expect("stored")).rename(n, out),
				RenameOp::Rename(v, n) => Pg(v).rename(n, out),
			}
		}

		for ele in changelist.created {
			Pg(ele).create(out);
		}

		let mut enum_changes = vec![];

		for ele in changelist.updated.iter().copied() {
			match (ele.old, ele.new) {
				(SchemaItem::Table(_), SchemaItem::Table(_)) => {}
				(SchemaItem::Enum(old), SchemaItem::Enum(new)) => {
					let diff = Pg(Diff { old, new });
					let removed = diff.print_renamed_added(out);
					enum_changes.push((diff, removed));
				}
				(SchemaItem::Scalar(_), SchemaItem::Scalar(_)) => {}
				_ => unreachable!("will fail on sort"),
			}
		}

		for ele in changelist.updated.iter().copied() {
			match (ele.old, ele.new) {
				(SchemaItem::Table(old), SchemaItem::Table(new)) => {
					Pg(Diff { old, new }).print(out)
				}
				(SchemaItem::Enum(_), SchemaItem::Enum(_)) => {}
				(SchemaItem::Scalar(old), SchemaItem::Scalar(new)) => {
					Pg(Diff { old, new }).print(out)
				}
				_ => unreachable!("will fail on sort"),
			}
		}

		for (en, removed) in enum_changes {
			en.print_removed(removed, out);
		}

		for (ele, _) in changelist.dropped {
			Pg(ele).drop(out);
		}
	}
}
impl Pg<&Schema> {
	pub fn diff(&self, target: &Self, out: &mut String) {
		Pg(SchemaDiff {
			old: target,
			new: self,
		})
		.print(out)
	}
	pub fn create(&self, out: &mut String) {
		self.diff(&Pg(&Schema::default()), out)
	}
	pub fn drop(&self, out: &mut String) {
		Pg(&Schema::default()).diff(self, out)
	}
}
impl Pg<SchemaEnum<'_>> {
	pub fn rename(&self, db: DbType, out: &mut String) {
		self.print_alternations(&[format!("RENAME TO {db}")], out);
		self.set_db(db)
	}
	pub fn create(&self, out: &mut String) {
		let db_name = &self.name();
		w!(out, "CREATE TYPE {db_name} AS ENUM (\n");
		for (i, v) in self.items.iter().enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			w!(out, "\t'{}'\n", v.db());
		}
		wl!(out, ");");
		wl!(out,);
	}
	pub fn drop(&self, out: &mut String) {
		let db_name = &self.name();
		w!(out, "DROP TYPE {db_name};\n");
	}
	pub fn print_alternations(&self, alternations: &[String], out: &mut String) {
		if alternations.is_empty() {
			return;
		}
		let name = &self.name();
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
	pub fn rename_alter(&self, to: DbEnumItem) -> String {
		let out = format!("RENAME VALUE '{}' TO {to}", self.db());
		self.set_db(to);
		out
	}
}
impl Pg<EnumDiff<'_>> {
	pub fn print_renamed_added(&self, out: &mut String) -> Vec<String> {
		let changelist = schema::mk_change_list(
			&self.0.old.items.iter().collect::<Vec<_>>(),
			&self.0.new.items.iter().collect::<Vec<_>>(),
		);
		let mut changes = vec![];
		for el in changelist.renamed.iter().cloned() {
			let mut stored = HashMap::new();
			match el {
				RenameOp::Store(i, t) => {
					stored.insert(t, i);
					changes.push(Pg(i).rename_alter(t.db()));
				}
				RenameOp::Rename(v, n) => {
					changes.push(Pg(v).rename_alter(n));
				}
				RenameOp::Restore(r, n) => {
					let stored = stored.remove(&r).expect("was not stored");
					Pg(stored).rename_alter(n);
				}
			}
		}

		for added in changelist.created.iter() {
			changes.push(format!("ADD VALUE '{}'", added.db()))
		}

		Pg(self.old).print_alternations(&changes, out);

		let mut changes_post = vec![];
		for (removed, _) in changelist.dropped.iter() {
			// It is not possible to remove enum element in postgres in transaction, so it is being renamed to
			// `name_removed_randomid` here, and resulting value may be removed later by non-transactional code
			let name = rand::distributions::Alphanumeric.sample_string(&mut rand::thread_rng(), 5);
			changes_post.push(format!(
				"RENAME VALUE '{}' TO '{0}_removed_{name}'",
				removed.db()
			))
		}
		changes_post
	}
	pub fn print_added(&self, out: &mut String) {
		let added = self.added_items();
		let db_name = &self.new.name();
		for added in added {
			wl!(out, "ALTER TYPE {db_name} ADD VALUE '{added}';");
		}
	}

	pub fn print_removed(&self, removed: Vec<String>, out: &mut String) {
		Pg(self.old).print_alternations(&removed, out)
	}
}
impl Pg<SchemaScalar<'_>> {
	pub fn rename(&self, to: DbType, out: &mut String) {
		self.print_alternations(&[format!("RENAME TO {to}")], out);
		self.scalar.set_db(to);
	}
	pub fn print_alternations(&self, alternations: &[String], out: &mut String) {
		if alternations.is_empty() {
			return;
		}
		let name = &self.name();
		w!(out, "ALTER DOMAIN {name}\n");
		for (i, alt) in alternations.iter().enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			wl!(out, "\t{alt}");
		}
		wl!(out, ";");
	}
	pub fn create(&self, out: &mut String) {
		let name = &self.name();
		let ty = self.scalar.inner_type();
		w!(out, "CREATE DOMAIN {name} AS {ty}");
		for ele in &self.annotations {
			match ele {
				ScalarAnnotation::Default(d) => {
					w!(out, "\n\tDEFAULT ");
					let formatted = Pg(self.schema).format_sql(d);
					w!(out, "{formatted}");
				}
				ScalarAnnotation::Check(sql) => {
					let name = sql.assigned_name();
					w!(out, "\n\tCONSTRAINT {name} CHECK ");
					let formatted = Pg(self.schema).format_sql(&sql.check);
					w!(out, "{formatted}")
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
	pub fn drop(&self, out: &mut String) {
		let name = &self.name();
		wl!(out, "DROP DOMAIN {name};");
	}
}
impl Pg<Diff<SchemaScalar<'_>>> {
	pub fn print(&self, out: &mut String) {
		// FIXME
	}
}

impl Pg<SchemaItem<'_>> {
	pub fn rename(&self, to: DbItem, out: &mut String) {
		match self.0 {
			SchemaItem::Table(t) => Pg(t).rename(DbTable::unchecked_from(to), out),
			SchemaItem::Enum(e) => Pg(e).rename(DbType::unchecked_from(to), out),
			SchemaItem::Scalar(s) => Pg(s).rename(DbType::unchecked_from(to), out),
		}
	}
	pub fn create(&self, out: &mut String) {
		match self.0 {
			SchemaItem::Table(t) => Pg(t).create(out),
			SchemaItem::Enum(e) => Pg(e).create(out),
			SchemaItem::Scalar(s) => Pg(s).create(out),
		}
	}
	pub fn drop(&self, out: &mut String) {
		match self.0 {
			SchemaItem::Table(t) => Pg(t).drop(out),
			SchemaItem::Enum(e) => Pg(e).drop(out),
			SchemaItem::Scalar(s) => Pg(s).drop(out),
		}
	}
}
impl Pg<SchemaSql<'_>> {
	pub fn print(&self, out: &mut String) {
		match &*self.0 {
			Sql::Cast(expr, ty) => {
				let expr = Pg(self.schema).format_sql(expr);
				let native_ty = self.schema.native_type(ty);
				w!(out, "({expr})::{native_ty}");
			}
			Sql::Call(proc, args) => {
				w!(out, "{proc}(");
				for (i, arg) in args.iter().enumerate() {
					if i != 0 {
						w!(out, ", ");
					}
					let arg = Pg(self.schema).format_sql(arg);
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
			Sql::UnOp(op, expr) => {
				let op = op.format();
				let expr = Pg(self.schema).format_sql(expr);
				w!(out, "{op}({expr})");
			}
			Sql::BinOp(a, op, b) => {
				let op = op.format();
				let a = Pg(self.schema).format_sql(a);
				let b = Pg(self.schema).format_sql(b);
				w!(out, "({a}) {op} ({b})")
			}
			Sql::Parened(a) => {
				let a = Pg(self.schema).format_sql(a);
				w!(out, "({a})")
			}
			Sql::Placeholder => {
				w!(out, "VALUE")
			}
			Sql::Ident(_) => {
				unreachable!("only placeholder is allowed for schema-bound checks")
			}
			Sql::Null => w!(out, "NULL"),
			Sql::Boolean(true) => w!(out, "TRUE"),
			Sql::Boolean(false) => w!(out, "FALSE"),
		}
	}
}
impl Pg<&Schema> {
	pub fn format_sql(&self, sql: &Sql) -> String {
		let mut out = String::new();
		Pg(self.sql(sql)).print(&mut out);
		out
	}
}

enum PgTableConstraint<'s> {
	PrimaryKey(TablePrimaryKey<'s>),
	Unique(TableUniqueConstraint<'s>),
	Check(TableCheck<'s>),
	ForeignKey(TableForeignKey<'s>),
}

impl Pg<TablePrimaryKey<'_>> {
	pub fn create_inline(&self, out: &mut String) {
		w!(out, "CONSTRAINT {} PRIMARY KEY(", self.assigned_name());
		self.table
			.print_column_list(&mut out, self.columns.into_iter());
		wl!(out, ")");
	}
	pub fn drop_alter(&self, out: &mut Vec<String>) {
		out.push(format!("DROP CONSTRAINT {}", self.assigned_name()));
	}
	pub fn create_alter(&self, out: &mut Vec<String>) {
		let mut inl = String::new();
		self.create_inline(&mut inl);
		out.push(format!("ADD {inl}"));
	}
	pub fn rename_alter(&self, new_name: DbConstraint, out: &mut Vec<String>) {
		out.push(format!(
			"RENAME CONSTRAINT {} TO {}",
			self.assigned_name(),
			new_name
		));
		self.set_db(new_name)
	}
}
impl Pg<TableUniqueConstraint<'_>> {
	pub fn create_inline(&self, out: &mut String) {
		w!(out, "CONSTRAINT {} UNIQUE(", self.assigned_name());
		self.table
			.print_column_list(&mut out, self.columns.into_iter());
		wl!(out, ")");
	}
}
impl Pg<TableCheck<'_>> {
	pub fn create_inline(&self, out: &mut String) {
		w!(out, "CONSTRAINT {} CHECK(", self.assigned_name());
		Pg(self.table.sql(&self.check)).print(out);
		wl!(out, ")");
	}
}

impl Pg<Diff<TablePrimaryKey<'_>>> {
	fn alter(&self, out: &mut Vec<String>) {
		if self.old.assigned_name() != self.new.assigned_name() {
			Pg(self.old).rename_alter(self.new.assigned_name(), out);
		}
	}
}

#[cfg(test)]
mod tests {
	use std::{fs, io::Write};

	use schema::{parser::parse, wl, Diff};
	use tempfile::NamedTempFile;
	use tracing::info;
	use tracing_test::traced_test;

	use crate::{process::default_options, Pg};

	#[traced_test]
	#[test]
	fn examples() {
		for example in fs::read_dir("examples").expect("list examples") {
			let example = example.expect("ex");
			info!("Example {:?}", example.path());
			let mut data = fs::read_to_string(&example.path()).expect("example read");
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
			let examples = examples
				.iter()
				.map(|example| {
					let mut schema = match parse(example.as_str(), &default_options()) {
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
				.print(&mut out_tmp);
				out.push_str(out_tmp.trim());
			}
			let output = format!("{}\n!!!RESULT\n{}", data.trim(), out.trim());
			let mut named_file =
				NamedTempFile::new_in(example.path().parent().expect("parent")).expect("new temp");
			named_file.write_all(output.as_bytes()).expect("write");
			named_file.persist(example.path()).expect("persist");
		}
	}
}
