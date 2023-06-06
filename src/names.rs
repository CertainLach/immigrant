use std::fmt::{self, Debug, Display};

use crate::ids::{DbIdent, Ident, Kind};

macro_rules! def_kind {
    ($($name:ident($v:expr)),+ $(,)?) => {
        $(
            pub enum $name {}
            impl Kind for $name {
                fn id() -> u8 {
                    $v
                }
            }
        )+
    };
}

def_kind!(
	TableKind(0),
	TypeKind(1),
	ColumnKind(2),
	ProcedureKind(3),
	EnumItemKind(4),
	IndexKind(5),
	ConstraintKind(6),
	ForeignKeyKind(7),
);

#[derive(Clone)]
pub struct DefName<K> {
	code: Ident<K>,
	db: DbIdent<K>,
}
impl<K> DefName<K> {
	pub fn id(&self) -> Ident<K> {
		self.code
	}
	pub fn db(&self) -> DbIdent<K> {
		self.db.clone()
	}
}
impl<K: Kind> DefName<K> {
	pub fn alloc((code, db): (&str, Option<&str>)) -> Self {
		Self {
			code: Ident::alloc(code),
			db: DbIdent::new(db.unwrap_or(code)),
		}
	}
}
impl<K> Display for DefName<K> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.db)
	}
}
impl<K> Debug for DefName<K> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "\"{}\"", self.db)
	}
}
impl<K> PartialEq<Ident<K>> for DefName<K> {
	fn eq(&self, other: &Ident<K>) -> bool {
		&self.code == other
	}
}
impl<K: Kind> PartialEq<DbIdent<K>> for DefName<K> {
	fn eq(&self, other: &DbIdent<K>) -> bool {
		&self.db == other
	}
}

pub type TableDefName = DefName<TableKind>;
pub type ColumnDefName = DefName<ColumnKind>;
pub type TypeDefName = DefName<TypeKind>;
pub type EnumItemDefName = DefName<EnumItemKind>;

pub type TableIdent = Ident<TableKind>;
pub type ColumnIdent = Ident<ColumnKind>;
pub type TypeIdent = Ident<TypeKind>;

pub type DbTable = DbIdent<TableKind>;
pub type DbColumn = DbIdent<ColumnKind>;
pub type DbType = DbIdent<TypeKind>;
pub type DbProcedure = DbIdent<ProcedureKind>;
pub type DbIndex = DbIdent<IndexKind>;
pub type DbConstraint = DbIdent<ConstraintKind>;
pub type DbForeignKey = DbIdent<ForeignKeyKind>;
pub type DbEnumItem = DbIdent<EnumItemKind>;
