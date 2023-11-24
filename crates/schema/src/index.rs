use super::sql::Sql;
use crate::{
	db_name_impls,
	ids::DbIdent,
	names::{ColumnIdent, ConstraintKind, DbColumn, DbNativeType, IndexKind},
	uid::{next_uid, RenameMap, Uid},
	TableIndex,
};

/// Can appear on columns, scalars, and tables.
///
/// Checks with the same name are merged using sql AND operator.
#[derive(Debug, Clone)]
pub struct Check {
	uid: Uid,
	name: Option<DbIdent<ConstraintKind>>,
	pub check: Sql,
}
db_name_impls!(Check, ConstraintKind);
impl Check {
	pub fn new(name: Option<DbIdent<ConstraintKind>>, check: Sql) -> Self {
		Self {
			uid: next_uid(),
			name,
			check,
		}
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
	uid: Uid,
	name: Option<DbIdent<ConstraintKind>>,
	pub columns: Vec<ColumnIdent>,
}
db_name_impls!(UniqueConstraint, ConstraintKind);
impl UniqueConstraint {
	pub fn new(name: Option<DbIdent<ConstraintKind>>, columns: Vec<ColumnIdent>) -> Self {
		Self {
			uid: next_uid(),
			name,
			columns,
		}
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
	uid: Uid,
	name: Option<DbIdent<ConstraintKind>>,
	pub columns: Vec<ColumnIdent>,
}
db_name_impls!(PrimaryKey, ConstraintKind);
impl PrimaryKey {
	pub fn new(name: Option<DbIdent<ConstraintKind>>, columns: Vec<ColumnIdent>) -> Self {
		Self {
			uid: next_uid(),
			name,
			columns,
		}
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
	uid: Uid,
	name: Option<DbIdent<IndexKind>>,
	pub unique: bool,
	pub fields: Vec<ColumnIdent>,
}
db_name_impls!(Index, IndexKind);
impl Index {
	pub fn new(name: Option<DbIdent<IndexKind>>, unique: bool, fields: Vec<ColumnIdent>) -> Self {
		Self {
			uid: next_uid(),
			name,
			unique,
			fields,
		}
	}
	pub fn propagate_to_table(mut self, column: ColumnIdent) -> Self {
		self.fields.insert(0, column);
		self
	}
}
impl TableIndex<'_> {
	pub fn db_columns<'i>(&'i self, rn: &'i RenameMap) -> impl Iterator<Item = DbColumn> + 'i {
		self.fields.iter().map(|f| self.table.db_name(f, rn))
	}
	pub fn db_types<'i>(&'i self, rn: &'i RenameMap) -> impl Iterator<Item = DbNativeType> + 'i {
		self.fields
			.iter()
			.map(|f| self.table.schema_column(*f).db_type(rn))
	}
}
