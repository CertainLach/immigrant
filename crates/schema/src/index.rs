use super::sql::Sql;
use crate::{
	names::{ColumnIdent, DbColumn, DbConstraint, DbIndex, DbNativeType},
	TableIndex,
};

/// Can appear on columns, scalars, and tables.
///
/// Checks with the same name are merged using sql AND operator.
#[derive(Debug, Clone)]
pub struct Check {
	pub check: Sql,
	pub name: Option<String>,
}
impl Check {
	pub fn assigned_name(&self) -> DbConstraint {
		DbConstraint::new(self.name.as_ref().expect("check name was not assigned"))
	}
	pub fn propagate_to_table(mut self, column: ColumnIdent) -> Self {
		self.check.replace_placeholder(column);
		self
	}
}

/// Can appear on columns, scalars, and tables.
///
/// Constraints with the same name are merged, unifying columns.
/// When appears on a scalar - always inlined to column.
#[derive(Debug, Clone)]
pub struct UniqueConstraint {
	pub columns: Vec<ColumnIdent>,
	pub name: Option<String>,
}
impl UniqueConstraint {
	pub fn assigned_name(&self) -> DbConstraint {
		DbConstraint::new(self.name.as_ref().expect("check name was not assigned"))
	}
	pub fn propagate_to_table(mut self, column: ColumnIdent) -> Self {
		self.columns.insert(0, column);
		self
	}
}

/// Can appear on columns, scalars, and tables.
///
/// Only tables can define name for primary key, in other cases it will raise a validation error.
/// Always merged, if there is a name conflict - raises a validation error.
/// When appears on a scalar - always inlined to column.
#[derive(Debug, Clone)]
pub struct PrimaryKey {
	pub columns: Vec<ColumnIdent>,
	pub name: Option<String>,
}
impl PrimaryKey {
	pub fn assigned_name(&self) -> DbConstraint {
		DbConstraint::new(self.name.as_ref().expect("check name was not assigned"))
	}
	pub fn propagate_to_table(mut self, column: ColumnIdent) -> Self {
		self.columns.insert(0, column);
		self
	}
}

/// Can appear on columns, scalars, and tables.
///
/// Indexed with the same name are merged, if one index is marked as unique, and other isn't - raises a validation error.
// TODO: Index kind? BTREE/etc
#[derive(Debug, Clone)]
pub struct Index {
	pub unique: bool,
	pub fields: Vec<ColumnIdent>,
	pub name: Option<String>,
}

impl Index {
	pub fn assigned_name(&self) -> DbIndex {
		DbIndex::new(self.name.as_ref().expect("index name was not assigned"))
	}
	pub fn propagate_to_table(mut self, column: ColumnIdent) -> Self {
		self.fields.insert(0, column);
		self
	}
}
impl TableIndex<'_> {
	pub fn isomorphic_to(&self, other: &Self) -> bool {
		self.unique == other.unique
			&& self.db_columns().collect::<Vec<_>>() == other.db_columns().collect::<Vec<_>>()
			&& self.db_types().collect::<Vec<_>>() == other.db_types().collect::<Vec<_>>()
	}

	pub fn db_columns(&self) -> impl Iterator<Item = DbColumn> + '_ {
		self.fields.iter().map(|f| self.table.db_name(f))
	}
	pub fn db_types(&self) -> impl Iterator<Item = DbNativeType> + '_ {
		self.fields
			.iter()
			.map(|f| self.table.schema_column(*f).db_type())
	}
}
