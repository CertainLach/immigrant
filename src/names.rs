use std::fmt::{self, Debug, Display};

use crate::ids::{DbIdent, Ident, Kind};

pub enum TableKind {}
impl Kind for TableKind {
    fn id() -> u8 {
        0
    }
}
pub enum TypeKind {}
impl Kind for TypeKind {
    fn id() -> u8 {
        1
    }
}
pub enum ColumnKind {}
impl Kind for ColumnKind {
    fn id() -> u8 {
        2
    }
}
pub enum ProcedureKind {}
impl Kind for ProcedureKind {
    fn id() -> u8 {
        3
    }
}
pub enum EnumItemKind {}
impl Kind for EnumItemKind {
    fn id() -> u8 {
        4
    }
}
pub enum IndexKind {}
impl Kind for IndexKind {
    fn id() -> u8 {
        5
    }
}

#[derive(Clone)]
pub struct DefName<K> {
    code: Ident<K>,
    db: DbIdent<K>,
}
impl<K> DefName<K> {
    pub fn db(&self) -> DbIdent<K> {
        self.db.clone()
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
