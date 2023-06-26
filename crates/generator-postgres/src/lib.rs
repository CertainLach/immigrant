use std::ops::Deref;

use schema::{
	index::ConstraintTy,
	names::{DbConstraint, DbForeignKey, DbIndex},
	root::Schema,
	sql::Sql,
	w, wl, ColumnDiff, SchemaDiff, SchemaTable, TableColumn, TableConstraint, TableDiff,
	TableForeignKey, TableIndex, TableSql,
};

pub struct Pg<T>(pub T);
impl<T> Deref for Pg<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.0
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
			Sql::Placeholder => todo!(),
			Sql::Null => todo!(),
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
		let table_name = &self.name;
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
		for cons in self.constraints() {
			if !had {
				had = true;
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			Pg(cons).create_inline(out);
			wl!(out, "");
		}
		wl!(out, ");");
		for idx in self.indexes() {
			Pg(idx).create(out);
		}
		w!(out, "\n");
	}
	pub fn drop(&self, out: &mut String) {
		let table_name = &self.name;
		for idx in self.indexes() {
			Pg(idx).drop(out);
		}
		wl!(out, "DROP TABLE {table_name};");
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
		let name = &self.name;
		w!(out, "ALTER TABLE {name}\n");
		for (i, alt) in alternations.iter().enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			wl!(out, "\t{alt}");
		}
		wl!(out, ";");
	}
	fn constraint_isomorphic_to(&self, other: TableConstraint<'_>) -> Option<TableConstraint<'_>> {
		self.constraints()
			.find(|i| Pg(*i).isomorphic_to(&Pg(other)))
	}
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
				Pg(old_idx).rename(Pg(new_idx).db_name(), out)
			} else {
				Pg(old_idx).drop(out)
			}
		}
		let mut alternations = Vec::new();
		for old_constraint in self.old.constraints() {
			if let Some(new_constraint) = Pg(self.new).constraint_isomorphic_to(old_constraint) {
				Pg(old_constraint).rename_alter(Pg(new_constraint).db_name(), &mut alternations);
			} else {
				Pg(old_constraint).drop_alter(&mut alternations);
			}
		}
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
		for new_constraint in self.new.constraints() {
			if Pg(self.old)
				.constraint_isomorphic_to(new_constraint)
				.is_none()
			{
				Pg(new_constraint).create_alter(&mut alternations);
			}
		}
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
		let table_name = &self.table.name;
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
		let target_table_name = target_table.name.db();
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
impl Pg<TableConstraint<'_>> {
	pub fn isomorphic_to(&self, other: &Self) -> bool {
		match (&self.kind, &other.kind) {
			(ConstraintTy::PrimaryKey(a), ConstraintTy::PrimaryKey(b)) => {
				self.table.db_names(a.iter().copied()) == other.table.db_names(b.iter().copied())
			}
			(ConstraintTy::Unique { columns: a }, ConstraintTy::Unique { columns: b }) => {
				self.table.db_names(a.iter().copied()) == other.table.db_names(b.iter().copied())
			}
			(ConstraintTy::Check { sql: a }, ConstraintTy::Check { sql: b }) => {
				Pg(self.table).format_sql(a) == Pg(other.table).format_sql(b)
			}
			_ => false,
		}
	}
	fn partial_name(&self) -> String {
		use ConstraintTy::*;
		if let Some(name) = &self.name {
			return name.clone();
		}
		let columns = match &self.kind {
			PrimaryKey(c) => c.clone(),
			Unique { columns } => columns.clone(),
			Check { sql } => {
				let cols = sql.affected_columns();
				if cols.is_empty() {
					panic!("no columns referenced in {sql:?}")
				}
				cols
			}
		};
		let mut out = self.table.format_index_name(columns.into_iter());
		match self.kind {
			PrimaryKey(..) => w!(out, "_pkey"),
			Unique { .. } => w!(out, "_key"),
			Check { .. } => w!(out, "_check"),
		}
		out
	}
	pub fn db_name(&self) -> DbConstraint {
		let partial_name = self.partial_name();
		let table_name = &self.table.name;
		DbConstraint::new(&format!("{table_name}_{partial_name}"))
	}
	pub fn create_inline(&self, out: &mut String) {
		use ConstraintTy::*;
		let name = self.db_name();
		w!(out, "CONSTRAINT {name} ");
		match &self.kind {
			PrimaryKey(columns) => {
				w!(out, "PRIMARY KEY(");
				self.table.print_column_list(out, columns.iter().copied());
				w!(out, ")");
			}
			Unique { columns } => {
				w!(out, "UNIQUE(");
				self.table.print_column_list(out, columns.iter().copied());
				w!(out, ")");
			}
			Check { sql } => {
				w!(out, "CHECK(");
				Pg(self.table.sql(sql)).print(out);
				w!(out, ")");
			}
		}
	}
	pub fn create_alter(&self, out: &mut Vec<String>) {
		let mut s = String::new();
		w!(s, "ADD ");
		self.create_inline(&mut s);
		out.push(s);
	}
	pub fn rename_alter(&self, new_name: DbConstraint, out: &mut Vec<String>) {
		let name = self.db_name();
		if name == new_name {
			return;
		}
		out.push(format!("RENAME CONSTRAINT {name} TO {new_name}"))
	}
	pub fn drop_alter(&self, out: &mut Vec<String>) {
		let name = self.db_name();
		out.push(format!("DROP CONSTRAINT {name}"))
	}
}
impl Pg<TableIndex<'_>> {
	fn partial_name(&self) -> String {
		if let Some(name) = &self.name {
			return name.to_owned();
		}
		let mut out = String::new();
		for column in self.db_columns() {
			w!(out, "{column}_");
		}
		if self.unique {
			w!(out, "key")
		} else {
			w!(out, "idx")
		}
		out
	}
	pub fn db_name(&self) -> DbIndex {
		let partial_name = self.partial_name();
		let table_name = &self.table.name;
		DbIndex::new(&format!("{table_name}_{partial_name}"))
	}
	pub fn create(&self, out: &mut String) {
		let name = self.db_name();
		let table_name = &self.table.name;

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
		let name = self.db_name();
		w!(out, "DROP INDEX {name};\n")
	}
	pub fn rename(&self, new_name: DbIndex, out: &mut String) {
		let name = self.db_name();
		if name == new_name {
			return;
		}
		w!(out, "ALTER INDEX {name} RENAME TO {new_name};\n")
	}
}
impl Pg<SchemaDiff<'_>> {
	pub fn print(&self, out: &mut String) {
		// Enums may be used as types in altered tables
		// There is no need to remove/alter enum first, there would be no conflicts, as enums with
		// the same names are considered the same enum, and listed in altered_enums
		for r#enum in self.created_enums() {
			r#enum.create(out);
		}

		// Newly added enum values may be used in @defaults of a new tables
		for enum_diff in self.altered_enums() {
			enum_diff.print_added(out);
		}

		// Referenced fields may be deleted on alter
		for table in self.dropped_tables() {
			Pg(table).print_drop_foreign_keys(out);
		}
		for table_diff in self.altered_tables() {
			Pg(table_diff).print_renamed_or_fropped_foreign_keys(out);
		}

		for table_diff in self.altered_tables() {
			Pg(table_diff).print(out);
		}
		for table in self.dropped_tables() {
			Pg(table).drop(out);
		}

		// There should be no references to enum items left
		for enum_diff in self.altered_enums() {
			enum_diff.print_removed(out);
		}
		for r#enum in self.dropped_enums() {
			r#enum.drop(out);
		}

		// Newly created tables have no references to anything
		for table in self.created_tables() {
			Pg(table).create(out);
		}

		// To avoid toposorting creations, we just create all fks later
		for table_diff in self.altered_tables() {
			Pg(table_diff).print_added_foreign_keys(out);
		}
		for table in self.created_tables() {
			Pg(table).create_fks(out);
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
