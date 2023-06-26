use super::sql::Sql;
use crate::{
	names::{ColumnIdent, DbColumn, DbType},
	TableIndex,
};

#[derive(Debug, Clone)]
pub enum ConstraintTy {
	PrimaryKey(Vec<ColumnIdent>),
	Unique { columns: Vec<ColumnIdent> },
	Check { sql: Sql },
}
#[derive(Debug, Clone)]
pub struct Constraint {
	pub kind: ConstraintTy,
	pub name: Option<String>,
}
impl Constraint {
	pub fn propagate_to_table(&mut self, column: ColumnIdent) {
		match &mut self.kind {
			ConstraintTy::PrimaryKey(pk) => pk.insert(0, column),
			ConstraintTy::Unique { columns } => columns.insert(0, column),
			ConstraintTy::Check { sql } => sql.replace_placeholder(column),
		}
	}
}

#[derive(Debug, Clone)]
pub struct Index {
	pub unique: bool,
	pub fields: Vec<ColumnIdent>,
	pub name: Option<String>,
}

impl Index {
	pub fn propagate_to_table(&mut self, column: ColumnIdent) {
		self.fields.insert(0, column);
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
	pub fn db_types(&self) -> impl Iterator<Item = DbType> + '_ {
		self.fields
			.iter()
			.map(|f| self.table.schema_column(*f).db_type())
	}
}
