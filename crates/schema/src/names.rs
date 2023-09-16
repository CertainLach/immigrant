use std::{
	cell::RefCell,
	fmt::{self, Debug, Display},
};

use derivative::Derivative;

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
	ColumnKind(2),
	ProcedureKind(3),
	EnumItemKind(4),
	IndexKind(5),
	ConstraintKind(6),
	ForeignKeyKind(7),
	NativeTypeKind(8),
	ItemKind(9),
	TableKind(9),
	TypeKind(9),
);

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
	pub fn set_db(&mut self, db: DbIdent<K>) {
		self.db = db
	}
}
impl<K: Kind> DefName<K> {
	pub fn alloc((code, db): (&str, Option<&str>)) -> Self {
		Self {
			code: Ident::alloc(code),
			db: DbIdent::new(db.unwrap_or(code)),
		}
	}
	pub fn unchecked_cast<U: Kind>(v: DefName<U>) -> Self {
		Self {
			code: Ident::unchecked_cast(v.code),
			db: DbIdent::unchecked_from(v.db),
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
		write!(f, "{:?} \"{}\"", self.code, self.db)
	}
}
impl<K> PartialEq<Ident<K>> for DefName<K> {
	fn eq(&self, other: &Ident<K>) -> bool {
		&self.code == other
	}
}
impl<K> PartialEq<DbIdent<K>> for DefName<K> {
	fn eq(&self, other: &DbIdent<K>) -> bool {
		&self.db == other
	}
}

impl<K> PartialEq for DefName<K> {
	fn eq(&self, other: &Self) -> bool {
		self.code == other.code && self.db == other.db
	}
}

impl<K> Clone for DefName<K> {
	fn clone(&self) -> Self {
		Self {
			code: self.code,
			db: self.db.clone(),
		}
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
pub type DbNativeType = DbIdent<NativeTypeKind>;
pub type DbItem = DbIdent<ItemKind>;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct UpdateableDefName<K> {
	name: RefCell<DefName<K>>,
}
impl<K> UpdateableDefName<K> {
	pub(crate) fn new(name: DefName<K>) -> Self {
		Self {
			name: RefCell::new(name),
		}
	}
	pub(crate) fn name(&self) -> DefName<K> {
		self.name.borrow().clone()
	}
	pub(crate) fn set_db(&self, name: DbIdent<K>) {
		self.name.borrow_mut().set_db(name)
	}
}

pub type UpdateableTableDefName = UpdateableDefName<TableKind>;
pub type UpdateableTypeDefName = UpdateableDefName<TypeKind>;
pub type UpdateableEnumItemDefName = UpdateableDefName<EnumItemKind>;

#[derive(Derivative)]
#[derivative(
	Debug(bound = ""),
	PartialEq(bound = ""),
	Clone(bound = ""),
	Default(bound = "")
)]
pub struct UpdateableDbName<K> {
	db: RefCell<DbIdent<K>>,
}
impl<K> UpdateableDbName<K> {
	pub(crate) fn guard() -> Self {
		Self {
			db: RefCell::new(DbIdent::guard()),
		}
	}
	pub(crate) fn new(db: DbIdent<K>) -> Self {
		Self {
			db: RefCell::new(db),
		}
	}
	pub(crate) fn db(&self) -> DbIdent<K> {
		let r = self.db.borrow();
		r.assert_not_guard();
		r.clone()
	}
	pub(crate) fn db_if_assigned(&self) -> Option<DbIdent<K>> {
		if self.assigned() {
			Some(self.db())
		} else {
			None
		}
	}
	pub(crate) fn set(&self, name: DbIdent<K>) {
		*self.db.borrow_mut() = name;
	}
	pub(crate) fn assigned(&self) -> bool {
		self.db.borrow().assigned()
	}
}
