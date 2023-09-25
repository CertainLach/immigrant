use std::ops::Deref;

use derivative::Derivative;
use ids::DbIdent;

use self::{
	changelist::IsCompatible,
	column::Column,
	ids::Ident,
	index::{Check, Index, PrimaryKey, UniqueConstraint},
	root::Schema,
	scalar::{Enum, Scalar},
	sql::Sql,
	table::{ForeignKey, Table, TableAnnotation},
	uid::HasUid,
};

pub mod column;
pub mod index;
pub mod process;
pub mod root;
pub mod scalar;
pub mod sql;
pub mod table;

pub mod ids;
pub mod names;

pub mod parser;

pub mod attribute;

mod changelist;
pub mod renamelist;

pub use changelist::{mk_change_list, ChangeList};

mod span;
pub mod uid;

#[macro_export]
macro_rules! w {
    ($out:expr, $($tt:tt)*) => {{
        use std::fmt::Write;
        write!($out, $($tt)*).unwrap();
    }};
}
#[macro_export]
macro_rules! wl {
    ($out:ident, $($tt:tt)*) => {{
        use std::fmt::Write;
			writeln!($out, $($tt)*).unwrap();
    }};
    ($out:ident) => {{
        use std::fmt::Write;
			writeln!($out).unwrap();
    }};
}
#[macro_export]
macro_rules! newty_enum {
    (
        $(#[$($attr:tt)+])*
        $vis:vis enum $name:ident {$($Variant:ident = $accessor:ident),* $(,)+}
    ) => {
        $(#[$($attr)+])*
        #[derive(derivative::Derivative)]
        #[derivative(Debug)]
        $vis enum $name {
            $(
                #[derivative(Debug = "transparent")]
                $Variant($Variant),
            )*
        }
        paste::paste!{impl $name {$(
            $vis fn [<is_ $accessor>](&self) -> bool {
                matches!(self, Self::$Variant(_))
            }
            $vis fn [<as_ $accessor>](&self) -> Option<&$Variant> {
                match self {
                    Self::$Variant(value) => Some(value),
                    _ => None,
                }
            }
            $vis fn [<as_ $accessor _mut>](&mut self) -> Option<&mut $Variant> {
                match self {
                    Self::$Variant(value) => Some(value),
                    _ => None,
                }
            }
        )*}}
    };
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct TableItem<'a, I> {
	#[derivative(Debug = "ignore")]
	pub table: SchemaTable<'a>,
	value: &'a I,
}
impl<'a, I> TableItem<'a, I> {
	pub fn unchecked_new(table: SchemaTable<'a>, value: &'a I) -> Self {
		Self { table, value }
	}
}
impl<I> Deref for TableItem<'_, I> {
	type Target = I;
	fn deref(&self) -> &Self::Target {
		self.value
	}
}
impl<I> Clone for TableItem<'_, I> {
	fn clone(&self) -> Self {
		*self
	}
}
impl<I> Copy for TableItem<'_, I> {}
pub type TableIndex<'a> = TableItem<'a, Index>;
pub type TableColumn<'a> = TableItem<'a, Column>;
pub type TableForeignKey<'a> = TableItem<'a, ForeignKey>;
pub type TableSql<'a> = TableItem<'a, Sql>;
pub type TablePrimaryKey<'a> = TableItem<'a, PrimaryKey>;
pub type TableCheck<'a> = TableItem<'a, Check>;
pub type TableUniqueConstraint<'a> = TableItem<'a, UniqueConstraint>;

pub struct SchemaSql<'a> {
	pub schema: &'a Schema,
	pub sql: &'a Sql,
}
impl Deref for SchemaSql<'_> {
	type Target = Sql;
	fn deref(&self) -> &Self::Target {
		self.sql
	}
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Diff<I> {
	pub old: I,
	pub new: I,
}
pub type SchemaDiff<'a> = Diff<&'a Schema>;
pub type TableDiff<'a> = Diff<SchemaTable<'a>>;
pub type EnumDiff<'a> = Diff<SchemaEnum<'a>>;
pub type ColumnDiff<'a> = Diff<TableColumn<'a>>;

#[derive(Clone, Copy, Derivative)]
#[derivative(Debug)]
pub struct SchemaEnum<'a> {
	#[derivative(Debug = "ignore")]
	pub schema: &'a Schema,
	pub en: &'a Enum,
}
impl Deref for SchemaEnum<'_> {
	type Target = Enum;

	fn deref(&self) -> &Self::Target {
		self.en
	}
}

#[derive(Clone, Copy, Debug)]
pub enum SchemaItem<'a> {
	Table(SchemaTable<'a>),
	Enum(SchemaEnum<'a>),
	Scalar(SchemaScalar<'a>),
}
impl HasUid for SchemaItem<'_> {
	fn uid(&self) -> uid::Uid {
		match self {
			SchemaItem::Table(t) => t.uid(),
			SchemaItem::Enum(e) => e.uid(),
			SchemaItem::Scalar(s) => s.uid(),
		}
	}
}
impl IsCompatible for SchemaItem<'_> {
	fn is_compatible(&self, new: &Self) -> bool {
		matches!(
			(self, new),
			(Self::Table(_), Self::Table(_))
				| (Self::Enum(_), Self::Enum(_))
				| (Self::Scalar(_), Self::Scalar(_))
		)
	}
}

#[derive(Clone, Copy, Derivative)]
#[derivative(Debug)]
pub struct SchemaScalar<'a> {
	#[derivative(Debug = "ignore")]
	pub schema: &'a Schema,
	pub scalar: &'a Scalar,
}
impl Deref for SchemaScalar<'_> {
	type Target = Scalar;

	fn deref(&self) -> &Self::Target {
		self.scalar
	}
}

#[derive(Clone, Copy)]
pub enum SchemaEnumOrScalar<'a> {
	Enum(SchemaEnum<'a>),
	Scalar(SchemaScalar<'a>),
}

#[derive(Clone, Copy, Derivative)]
#[derivative(Debug)]
pub struct SchemaTable<'a> {
	#[derivative(Debug = "ignore")]
	pub schema: &'a Schema,
	pub table: &'a Table,
}

impl SchemaTable<'_> {
	pub fn sql<'a>(&'a self, sql: &'a Sql) -> TableSql<'a> {
		TableSql {
			table: *self,
			value: sql,
		}
	}
	pub fn primary_key(&self) -> Option<TablePrimaryKey> {
		let pk = self.table.primary_key()?;
		Some(TablePrimaryKey {
			table: *self,
			value: pk,
		})
	}
	pub fn checks(&self) -> impl Iterator<Item = TableCheck> {
		self.annotations
			.iter()
			.filter_map(TableAnnotation::as_check)
			.map(|value| TableCheck {
				table: *self,
				value,
			})
	}
	pub fn unique_constraints(&self) -> impl Iterator<Item = TableUniqueConstraint> {
		self.annotations
			.iter()
			.filter_map(TableAnnotation::as_unique_constraint)
			.map(|value| TableUniqueConstraint {
				table: *self,
				value,
			})
	}
}
impl Deref for SchemaTable<'_> {
	type Target = Table;

	fn deref(&self) -> &Self::Target {
		self.table
	}
}

pub trait HasIdent {
	type Kind;
	fn id(&self) -> Ident<Self::Kind>;
}
pub trait HasDefaultDbName {
	type Kind;
	fn default_db(&self) -> Option<DbIdent<Self::Kind>>;
}

#[macro_export]
macro_rules! def_name_impls {
	($t:ty, $k:ident) => {
		impl $crate::uid::HasUid for $t {
			fn uid(&self) -> Uid {
				self.uid
			}
		}
		impl $crate::HasIdent for $t {
			type Kind = $k;
			fn id(&self) -> $crate::ids::Ident<Self::Kind> {
				self.name.code.clone()
			}
		}
		impl $crate::HasDefaultDbName for $t {
			type Kind = $k;
			fn default_db(&self) -> Option<$crate::ids::DbIdent<Self::Kind>> {
				self.name.db.clone()
			}
		}
	};
}
#[macro_export]
macro_rules! db_name_impls {
	($t:ty, $k:ident) => {
		impl $crate::uid::HasUid for $t {
			fn uid(&self) -> Uid {
				self.uid
			}
		}
		impl $crate::HasDefaultDbName for $t {
			type Kind = $k;
			fn default_db(&self) -> Option<$crate::ids::DbIdent<Self::Kind>> {
				self.name.clone()
			}
		}
	};
}
