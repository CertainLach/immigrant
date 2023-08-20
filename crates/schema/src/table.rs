use std::collections::BTreeSet;

use super::{
	column::{Column, ColumnAnnotation},
	index::{Constraint, ConstraintTy},
};
use crate::{
	attribute::AttributeList,
	names::{
		ColumnIdent, DbColumn, DbNativeType, DbTable, DbType, TableDefName, TableIdent, TypeIdent,
		UpdateableTableDefName,
	},
	scalar::InlinedScalar,
	w, Index, SchemaTable, TableColumn, TableConstraint, TableForeignKey, TableIndex, TableItem,
};

#[derive(Debug)]
pub struct Table {
	pub docs: Vec<String>,
	pub attrlist: AttributeList,
	name: UpdateableTableDefName,
	pub columns: Vec<Column>,
	pub annotations: Vec<TableAnnotation>,
	pub foreign_keys: Vec<ForeignKey>,
}
impl Table {
	pub fn new(
		docs: Vec<String>,
		attrlist: AttributeList,
		name: TableDefName,
		columns: Vec<Column>,
		annotations: Vec<TableAnnotation>,
		foreign_keys: Vec<ForeignKey>,
	) -> Self {
		Self {
			docs,
			attrlist,
			name: UpdateableTableDefName::new(name),
			columns,
			annotations,
			foreign_keys,
		}
	}
	pub fn name(&self) -> TableDefName {
		self.name.name()
	}
	pub fn set_db(&self, db: DbTable) {
		self.name.set_db(db)
	}
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
	pub fn sql(&self) -> Option<&'static str> {
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
	pub fn as_index(&self) -> Option<&Index> {
		match self {
			TableAnnotation::Index(i) => Some(i),
			_ => None,
		}
	}
	pub fn as_constraint(&self) -> Option<&Constraint> {
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
			if let Some(fk) = column.propagate_foreign_key() {
				self.foreign_keys.push(fk);
			}
		}
	}
	pub(crate) fn apply_inlined_scalar(&mut self, scalar: TypeIdent, annotations: &InlinedScalar) {
		for col in self.columns.iter_mut() {
			col.apply_inlined_scalar(scalar, annotations)
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
	pub fn db_names(&self, columns: impl IntoIterator<Item = ColumnIdent>) -> Vec<DbColumn> {
		columns.into_iter().map(|c| self.db_name(&c)).collect()
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
	pub fn column(&self, name: &DbColumn) -> Option<TableColumn<'_>> {
		self.columns().find(|c| &c.name == name)
	}
	pub fn schema_column(&self, column: ColumnIdent) -> TableColumn<'_> {
		self.columns()
			.find(|c| c.name == column)
			.expect("column not found")
	}
	pub fn columns(&self) -> impl Iterator<Item = TableColumn<'_>> {
		self.columns.iter().map(|i| self.item(i))
	}
	pub fn index_isomophic_to(&self, other: TableIndex<'_>) -> Option<TableIndex<'_>> {
		self.indexes().find(|i| i.isomorphic_to(&other))
	}
	pub fn foreign_key_isomophic_to(
		&self,
		other: TableForeignKey<'_>,
	) -> Option<TableForeignKey<'_>> {
		self.foreign_keys().find(|i| i.isomorphic_to(&other))
	}
	pub fn indexes(&self) -> impl Iterator<Item = TableIndex<'_>> {
		self.annotations
			.iter()
			.filter_map(TableAnnotation::as_index)
			.map(|i| self.item(i))
	}
	pub fn constraints(&self) -> impl Iterator<Item = TableConstraint<'_>> {
		self.annotations
			.iter()
			.filter_map(TableAnnotation::as_constraint)
			.map(|i| self.item(i))
	}
	pub fn foreign_keys(&self) -> impl Iterator<Item = TableForeignKey<'_>> {
		self.foreign_keys.iter().map(|i| self.item(i))
	}
	pub fn cardinality(&self, columns: impl IntoIterator<Item = ColumnIdent>) -> Cardinality {
		fn is_unique_by_index(
			indexed: impl IntoIterator<Item = ColumnIdent>,
			columns: &BTreeSet<ColumnIdent>,
		) -> bool {
			let indexed = indexed.into_iter().collect::<BTreeSet<_>>();
			if indexed.difference(columns).next().is_none() {
				// Column list has full intersection with the unique index, thus
				// the request itself is unique
				//
				// I.e we have UNIQUE INDEX (a, b, c)
				// and the request is (a, b, c, d)
				//
				// Subset (a, b, c) is unique, thus d is irrelevant.
				return true;
			}
			false
		}
		let columns = columns.into_iter().collect::<BTreeSet<_>>();
		for ele in self.annotations.iter() {
			match ele {
				TableAnnotation::Index(i) => {
					if i.unique && is_unique_by_index(i.fields.iter().copied(), &columns) {
						return Cardinality::One;
					}
				}
				TableAnnotation::Constraint(constraint) => match &constraint.kind {
					ConstraintTy::PrimaryKey(constrained) => {
						if is_unique_by_index(constrained.iter().copied(), &columns) {
							return Cardinality::One;
						}
					}
					ConstraintTy::Unique {
						columns: constrained,
					} => {
						if is_unique_by_index(constrained.iter().copied(), &columns) {
							return Cardinality::One;
						}
					}
					ConstraintTy::Check { .. } => {}
				},
			}
		}
		// FIXME: Wrong assumption? When target is one, it doesn't mean the source is one too
		// for fk in self.foreign_keys() {
		// 	if !is_unique_by_index(fk.source_columns(), &columns) {
		// 		// This fk can not constraint our query
		// 		continue;
		// 	}
		// 	let target = fk.target_table();
		// 	let target_columns = fk.target_columns();
		// 	if target.cardinality(target_columns) == Cardinality::One {
		// 		return Cardinality::One;
		// 	}
		// }
		// TODO: Check inverse foreign keys too
		Cardinality::Many
	}
}

#[derive(PartialEq, Eq)]
pub enum Cardinality {
	One,
	Many,
}

impl<'s> TableForeignKey<'s> {
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
	pub fn target_table(&self) -> SchemaTable<'s> {
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
	pub fn db_types(&self) -> Vec<DbNativeType> {
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
	pub fn cardinality(&self) -> (Cardinality, Cardinality) {
		(
			self.table.cardinality(self.source_columns()),
			self.target_table().cardinality(self.target_columns()),
		)
	}
}
