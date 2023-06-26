use std::ops::Deref;

use self::{
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
        $vis enum $name {
            $($Variant($Variant),)*
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

pub struct TableItem<'a, I> {
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
		Self {
			table: self.table,
			value: self.value,
		}
	}
}
impl<I> Copy for TableItem<'_, I> {}
pub type TableIndex<'a> = TableItem<'a, Index>;
pub type TableColumn<'a> = TableItem<'a, Column>;
pub type TableConstraint<'a> = TableItem<'a, Constraint>;
pub type TableForeignKey<'a> = TableItem<'a, ForeignKey>;
pub type TableSql<'a> = TableItem<'a, Sql>;

pub struct Diff<I> {
	pub old: I,
	pub new: I,
}
pub type SchemaDiff<'a> = Diff<&'a Schema>;
pub type TableDiff<'a> = Diff<SchemaTable<'a>>;
pub type EnumDiff<'a> = Diff<SchemaEnum<'a>>;
pub type ColumnDiff<'a> = Diff<TableColumn<'a>>;

#[derive(Clone, Copy)]
pub struct SchemaEnum<'a> {
	pub schema: &'a Schema,
	pub en: &'a Enum,
}
impl Deref for SchemaEnum<'_> {
	type Target = Enum;

	fn deref(&self) -> &Self::Target {
		self.en
	}
}

#[derive(Clone, Copy)]
pub struct SchemaScalar<'a> {
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
pub struct SchemaTable<'a> {
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
