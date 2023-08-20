use std::ops::Deref;

use derivative::Derivative;
use ids::DbIdent;
use names::{DefName, ItemKind, TypeKind};

use self::{
	changelist::IsCompatible,
	column::Column,
	index::{Constraint, Index},
	root::Schema,
	scalar::{Enum, Scalar},
	sql::Sql,
	table::{ForeignKey, Table},
};

pub mod column;
pub mod index;
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
pub type TableConstraint<'a> = TableItem<'a, Constraint>;
pub type TableForeignKey<'a> = TableItem<'a, ForeignKey>;
pub type TableSql<'a> = TableItem<'a, Sql>;

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
impl HasName for SchemaItem<'_> {
	type Kind = ItemKind;

	fn name(&self) -> DefName<Self::Kind> {
		match self {
			SchemaItem::Table(t) => DefName::unchecked_cast(t.table.name()),
			SchemaItem::Enum(e) => DefName::unchecked_cast(e.en.name()),
			SchemaItem::Scalar(s) => DefName::unchecked_cast(s.scalar.name()),
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
}
impl Deref for SchemaTable<'_> {
	type Target = Table;

	fn deref(&self) -> &Self::Target {
		self.table
	}
}

pub trait HasName {
	type Kind;
	fn name(&self) -> DefName<Self::Kind>;
}
pub trait HasDbName {
	type Kind;
	fn db(&self) -> DbIdent<Self::Kind>;
}
impl<T> HasDbName for T
where
	T: HasName,
{
	type Kind = <T as HasName>::Kind;

	fn db(&self) -> DbIdent<Self::Kind> {
		self.name().db()
	}
}

impl HasName for SchemaEnumOrScalar<'_> {
	type Kind = TypeKind;

	fn name(&self) -> DefName<Self::Kind> {
		match self {
			SchemaEnumOrScalar::Enum(e) => e.name(),
			SchemaEnumOrScalar::Scalar(s) => s.name(),
		}
	}
}
