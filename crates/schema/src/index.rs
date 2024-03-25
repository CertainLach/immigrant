use super::sql::Sql;
use crate::{
	db_name_impls,
	ids::DbIdent,
	names::{ColumnIdent, ConstraintKind, DbColumn, DbNativeType, FieldIdent, IndexKind},
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
		self.uid = next_uid();
		self.check.replace_placeholder(Sql::Ident(column));
		self
	}
	pub fn propagate_to_composite(mut self, field: FieldIdent) -> Self {
		self.uid = next_uid();
		self.check
			.replace_placeholder(Sql::GetField(Box::new(Sql::Placeholder), field));
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
		self.uid = next_uid();
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
		self.uid = next_uid();
		self.columns.insert(0, column);
		self
	}
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Using(pub String);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OpClass(pub String);

/// Can appear on columns, scalars, and tables.
///
/// Indexed with the same name are merged, if one index is marked as unique, and other isn't - raises a validation error.
// TODO: Index kind? BTREE/etc
#[derive(Debug, Clone)]
pub struct Index {
	uid: Uid,
	name: Option<DbIdent<IndexKind>>,
	pub unique: bool,
	fields: Vec<(ColumnIdent, Option<OpClass>)>,
	pub using: Option<Using>,
	pub default_opclass: Option<OpClass>,
}
db_name_impls!(Index, IndexKind);
impl Index {
	pub fn new(
		name: Option<DbIdent<IndexKind>>,
		unique: bool,
		fields: Vec<(ColumnIdent, Option<OpClass>)>,
		using: Option<Using>,
		default_opclass: Option<OpClass>,
	) -> Self {
		Self {
			uid: next_uid(),
			name,
			unique,
			fields: fields
				.into_iter()
				.map(|v| (v.0, v.1.or(default_opclass.clone())))
				.collect(),
			using,
			default_opclass,
		}
	}
	pub fn propagate_to_table(mut self, column: ColumnIdent) -> Self {
		self.uid = next_uid();
		self.fields
			.insert(0, (column, self.default_opclass.clone()));
		self.uid = next_uid();
		self
	}
	pub fn fields(&self) -> &[(ColumnIdent, Option<OpClass>)] {
		&self.fields
	}
	pub fn field_idents(&self) -> impl IntoIterator<Item = ColumnIdent> + '_ {
		self.fields().iter().map(|i| i.0)
	}
}
impl TableIndex<'_> {
	pub fn db_columns<'i>(&'i self, rn: &'i RenameMap) -> impl Iterator<Item = DbColumn> + 'i {
		self.fields.iter().map(|f| self.table.db_name(&f.0, rn))
	}
	pub fn db_columns_opclass<'i>(
		&'i self,
		rn: &'i RenameMap,
	) -> impl Iterator<Item = (DbColumn, Option<OpClass>)> + 'i {
		self.fields
			.iter()
			.map(|f| (self.table.db_name(&f.0, rn), f.1.clone()))
	}
	pub fn db_types<'i>(&'i self, rn: &'i RenameMap) -> impl Iterator<Item = DbNativeType> + 'i {
		self.fields
			.iter()
			.map(|f| self.table.schema_column(f.0).db_type(rn))
	}
}
