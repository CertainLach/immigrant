use crate::names::{
	ColumnIdent, DbColumn, DbForeignKey, DbType, TableDefName, TableIdent,
	TypeIdent,
};
use crate::{
	w, wl, ColumnDiff, Index, SchemaTable, TableColumn, TableConstraint, TableDiff,
	TableForeignKey, TableIndex, TableItem,
};

use super::column::{Column, ColumnAnnotation};
use super::index::Constraint;

#[derive(Debug)]
pub struct Table {
	pub name: TableDefName,
	pub columns: Vec<Column>,
	pub annotations: Vec<TableAnnotation>,
	pub foreign_keys: Vec<ForeignKey>,
}
#[derive(Debug)]
pub enum TableAnnotation {
	Index(Index),
	Constraint(Constraint),
}

#[derive(Debug, PartialEq, Eq)]
pub enum OnDelete {
	SetNull,
	SetDefault,
	Restrict,
	Noop,
	Cascade,
}
impl OnDelete {
	fn sql(&self) -> Option<&'static str> {
		Some(match self {
			OnDelete::SetNull => "SET NULL",
			OnDelete::SetDefault => "SET DEFAULT",
			OnDelete::Restrict => "RESTRICT",
			OnDelete::Noop => return None,
			OnDelete::Cascade => "CASCADE",
		})
	}
}

#[derive(Debug)]
pub struct ForeignKey {
	pub name: Option<String>,
	pub source_fields: Option<Vec<ColumnIdent>>,
	pub target: TableIdent,
	pub target_fields: Option<Vec<ColumnIdent>>,
	pub on_delete: OnDelete,
}

impl TableAnnotation {
	fn as_index(&self) -> Option<&Index> {
		match self {
			TableAnnotation::Index(i) => Some(i),
			_ => None,
		}
	}
	fn as_constraint(&self) -> Option<&Constraint> {
		match self {
			TableAnnotation::Constraint(i) => Some(i),
			_ => None,
		}
	}
}

impl Table {
	pub fn process(&mut self) {
		for column in self.columns.iter_mut() {
			let propagated = column.propagate_annotations();
			self.annotations.extend(propagated);
		}
	}
	pub fn apply_scalar_annotations(
		&mut self,
		scalar: TypeIdent,
		annotations: &[ColumnAnnotation],
	) {
		for col in self.columns.iter_mut() {
			col.apply_scalar_annotations(scalar, annotations)
		}
	}

	pub fn db_name(&self, column: &ColumnIdent) -> DbColumn {
		for ele in self.columns.iter() {
			if &ele.name == column {
				return ele.name.db();
			}
		}
		unreachable!("unknown field: {column:?}");
	}
	pub fn format_index_name(&self, columns: impl Iterator<Item = ColumnIdent>) -> String {
		let mut out = String::new();
		for (i, column) in columns.enumerate() {
			if i != 0 {
				w!(out, "_");
			}
			let db_name = self.db_name(&column);
			w!(out, "{db_name}");
		}
		assert!(!out.is_empty(), "passed no columns");
		out
	}
	pub fn print_column_list(&self, out: &mut String, columns: impl Iterator<Item = ColumnIdent>) {
		for (i, column) in columns.enumerate() {
			if i != 0 {
				w!(out, ", ");
			}
			let db_name = self.db_name(&column);
			w!(out, "{db_name}");
		}
	}
}
impl SchemaTable<'_> {
	fn item<'a, I>(&'a self, value: &'a I) -> TableItem<'a, I> {
		TableItem::unchecked_new(*self, value)
	}
	fn column(&self, name: &DbColumn) -> Option<TableColumn<'_>> {
		self.columns().find(|c| &c.name == name)
	}
	pub fn schema_column(&self, column: ColumnIdent) -> TableColumn<'_> {
		self.columns()
			.find(|c| c.name == column)
			.expect("column not found")
	}
	fn columns(&self) -> impl Iterator<Item = TableColumn<'_>> {
		self.columns.iter().map(|i| self.item(i))
	}
	fn index_isomophic_to(&self, other: TableIndex<'_>) -> Option<TableIndex<'_>> {
		self.indexes().find(|i| i.isomorphic_to(&other))
	}
	fn foreign_key_isomophic_to(&self, other: TableForeignKey<'_>) -> Option<TableForeignKey<'_>> {
		self.foreign_keys().find(|i| i.isomorphic_to(&other))
	}
	fn indexes(&self) -> impl Iterator<Item = TableIndex<'_>> {
		self.annotations
			.iter()
			.filter_map(TableAnnotation::as_index)
			.map(|i| self.item(i))
	}
	fn constraints(&self) -> impl Iterator<Item = TableConstraint<'_>> {
		self.annotations
			.iter()
			.filter_map(TableAnnotation::as_constraint)
			.map(|i| self.item(i))
	}
	fn foreign_keys(&self) -> impl Iterator<Item = TableForeignKey<'_>> {
		self.foreign_keys.iter().map(|i| self.item(i))
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
			v.create_inline(out);
			wl!(out, "");
		}
		for cons in self.constraints() {
			if !had {
				had = true;
			} else {
				w!(out, ",");
			};
			w!(out, "\t");
			cons.create_inline(out);
			wl!(out, "");
		}
		wl!(out, ");");
		for idx in self.indexes() {
			idx.create(out);
		}
		w!(out, "\n");
	}
	pub fn drop(&self, out: &mut String) {
		let table_name = &self.name;
		for idx in self.indexes() {
			idx.drop(out);
		}
		wl!(out, "DROP TABLE {table_name};");
	}

	pub fn print_drop_foreign_keys(&self, out: &mut String) {
		let mut alternations = Vec::new();
		for fk in self.foreign_keys() {
			alternations.push(fk.drop_alter());
		}
		self.print_alternations(&alternations, out);
	}
	pub fn create_fks(&self, out: &mut String) {
		let mut alternations = Vec::new();
		for fk in self.foreign_keys() {
			alternations.push(fk.create_alter());
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
}
impl TableForeignKey<'_> {
	pub fn isomorphic_to(&self, other: &Self) -> bool {
		self.on_delete == other.on_delete
			&& self.source_db_columns() == other.source_db_columns()
			&& self.target_db_columns() == other.target_db_columns()
			&& self.db_types() == other.db_types()
	}
	pub fn source_columns(&self) -> Vec<ColumnIdent> {
		if let Some(source) = &self.source_fields {
			return source.clone();
		}
		if let Some(target) = &self.target_fields {
			return target.clone();
		};
		panic!("no fields defined")
	}
	pub fn target_columns(&self) -> Vec<ColumnIdent> {
		if let Some(target) = &self.target_fields {
			return target.clone();
		};
		if let Some(source) = &self.source_fields {
			return source.clone();
		}
		panic!("no fields defined")
	}
	pub fn target_table(&self) -> SchemaTable<'_> {
		let table = &self.target;
		self.table
			.schema
			.schema_table(table)
			.expect("target table is not defined")
	}
	pub fn source_db_columns(&self) -> Vec<DbColumn> {
		self.source_columns()
			.into_iter()
			.map(|f| self.table.db_name(&f))
			.collect()
	}
	pub fn target_db_columns(&self) -> Vec<DbColumn> {
		let target_table = self.target_table();
		self.target_columns()
			.into_iter()
			.map(|f| target_table.db_name(&f))
			.collect()
	}
	pub fn db_types(&self) -> Vec<DbType> {
		let db_types: Vec<_> = self
			.source_columns()
			.into_iter()
			.map(|f| self.table.schema_column(f).db_type())
			.collect();
		// Sanity
		let target_table = self.target_table();
		let target_db_types: Vec<_> = self
			.target_columns()
			.into_iter()
			.map(|f| target_table.schema_column(f).db_type())
			.collect();
		assert_eq!(db_types, target_db_types);
		db_types
	}
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
impl TableDiff<'_> {
	pub fn print_renamed_or_fropped_foreign_keys(&self, out: &mut String) {
		let mut alternations = Vec::new();
		for old_fk in self.old.foreign_keys() {
			if let Some(new_fk) = self.new.foreign_key_isomophic_to(old_fk) {
				if let Some(ren) = old_fk.rename_alter(new_fk.db_name()) {
					alternations.push(ren);
				}
			} else {
				alternations.push(old_fk.drop_alter())
			}
		}
		self.new.print_alternations(&alternations, out)
	}
	pub fn print_added_foreign_keys(&self, out: &mut String) {
		let mut alternations = Vec::new();
		for new_fk in self.new.foreign_keys() {
			if self.old.foreign_key_isomophic_to(new_fk).is_none() {
				alternations.push(new_fk.create_alter())
			}
		}
		self.new.print_alternations(&alternations, out)
	}
	pub fn print(&self, out: &mut String) {
		for old_idx in self.old.indexes() {
			if let Some(new_idx) = self.new.index_isomophic_to(old_idx) {
				old_idx.rename(new_idx.db_name(), out)
			} else {
				old_idx.drop(out)
			}
		}
		let mut alternations = Vec::new();
		for old_column in self.old.columns() {
			if let Some(new_column) = self.new.column(&old_column.name.db()) {
				ColumnDiff {
					old: old_column,
					new: new_column,
				}
				.print_alter(&mut alternations);
			} else {
				old_column.drop_alter(&mut alternations);
			}
		}
		for new_column in self.new.columns() {
			if self.old.column(&new_column.name.db()).is_none() {
				new_column.create_alter(&mut alternations)
			}
		}
		self.new.print_alternations(&alternations, out);
		for new_idx in self.new.indexes() {
			if self.old.index_isomophic_to(new_idx).is_none() {
				new_idx.create(out)
			}
		}
	}
}
