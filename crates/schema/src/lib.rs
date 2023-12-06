use std::ops::Deref;

use derivative::Derivative;
use ids::DbIdent;
use itertools::Itertools;

use self::{
	attribute::AttributeList,
	column::Column,
	composite::Composite,
	ids::Ident,
	index::{Check, Index, PrimaryKey, UniqueConstraint},
	names::{ItemKind, TypeIdent},
	root::Schema,
	scalar::{Enum, Scalar, ScalarAnnotation},
	sql::Sql,
	table::{ForeignKey, Table, TableAnnotation},
	uid::RenameMap,
};

pub mod column;
pub mod composite;
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

pub use changelist::{mk_change_list, ChangeList, IsCompatible, IsIsomorph};

mod span;
pub mod uid;
pub use uid::HasUid;

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
	pub value: &'a I,
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
impl<T> IsCompatible for TableItem<'_, T>
where
	T: IsCompatible,
{
	fn is_compatible(&self, new: &Self, rn: &RenameMap) -> bool {
		self.value.is_compatible(new.value, rn)
	}
}
impl<T> IsIsomorph for TableItem<'_, T>
where
	T: IsIsomorph,
{
	fn is_isomorph(&self, other: &Self, rn: &RenameMap) -> bool {
		self.value.is_isomorph(other.value, rn)
	}
}
impl<T> HasUid for TableItem<'_, T>
where
	T: HasUid,
{
	fn uid(&self) -> uid::Uid {
		self.value.uid()
	}
}
impl<T> HasDefaultDbName for TableItem<'_, T>
where
	T: HasDefaultDbName,
{
	type Kind = T::Kind;

	fn default_db(&self) -> Option<DbIdent<Self::Kind>> {
		self.value.default_db()
	}
}
impl<T> HasIdent for TableItem<'_, T>
where
	T: HasIdent,
{
	type Kind = T::Kind;

	fn id(&self) -> Ident<Self::Kind> {
		self.value.id()
	}
}

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
	Composite(SchemaComposite<'a>),
}
derive_is_isomorph_by_id_name!(SchemaItem<'_>);
impl SchemaItem<'_> {
	pub fn as_enum(&self) -> Option<SchemaEnum> {
		match self {
			Self::Enum(e) => Some(*e),
			_ => None,
		}
	}
	pub fn as_scalar(&self) -> Option<SchemaScalar> {
		match self {
			Self::Scalar(e) => Some(*e),
			_ => None,
		}
	}
	pub fn as_composite(&self) -> Option<SchemaComposite> {
		match self {
			Self::Composite(e) => Some(*e),
			_ => None,
		}
	}
	pub fn as_table(&self) -> Option<SchemaTable> {
		match self {
			Self::Table(e) => Some(*e),
			_ => None,
		}
	}
	pub fn schema(&self) -> &Schema {
		match self {
			SchemaItem::Table(t) => t.schema,
			SchemaItem::Enum(e) => e.schema,
			SchemaItem::Scalar(s) => s.schema,
			SchemaItem::Composite(c) => c.schema,
		}
	}
}
impl HasUid for SchemaItem<'_> {
	fn uid(&self) -> uid::Uid {
		match self {
			SchemaItem::Table(t) => t.uid(),
			SchemaItem::Enum(e) => e.uid(),
			SchemaItem::Scalar(s) => s.uid(),
			SchemaItem::Composite(s) => s.uid(),
		}
	}
}
impl IsCompatible for SchemaItem<'_> {
	fn is_compatible(&self, new: &Self, rn: &RenameMap) -> bool {
		match (self, new) {
			(SchemaItem::Table(_), SchemaItem::Table(_)) => true,
			(SchemaItem::Enum(a), SchemaItem::Enum(b)) => {
				// There is no DB engine, which supports removing enum variants, so removals are incompatible, and the
				// enum should be recreated.
				mk_change_list(rn, &a.items, &b.items).dropped.is_empty()
			}
			(SchemaItem::Composite(a), SchemaItem::Composite(b)) => {
				// There is no DB engine, which supports updating structs
				let changes = mk_change_list(rn, &a.fields().collect_vec(), &b.fields().collect_vec());
				changes.dropped.is_empty() && changes.created.is_empty()
			}
			(SchemaItem::Scalar(_), SchemaItem::Scalar(_)) => true,
			_ => false,
		}
		// matches!(
		// 	(self, new),
		// 	(Self::Table(_), Self::Table(_))
		// 		| (Self::Enum(a), Self::Enum(_))
		// 		| (Self::Scalar(_), Self::Scalar(_))
		// )
	}
}
impl HasIdent for SchemaItem<'_> {
	type Kind = ItemKind;

	fn id(&self) -> Ident<Self::Kind> {
		match self {
			SchemaItem::Table(t) => Ident::unchecked_cast(t.id()),
			SchemaItem::Enum(e) => Ident::unchecked_cast(e.id()),
			SchemaItem::Scalar(s) => Ident::unchecked_cast(s.id()),
			SchemaItem::Composite(s) => Ident::unchecked_cast(s.id()),
		}
	}
}
impl HasDefaultDbName for SchemaItem<'_> {
	type Kind = ItemKind;

	fn default_db(&self) -> Option<DbIdent<Self::Kind>> {
		match self {
			SchemaItem::Table(t) => t.default_db().map(DbIdent::unchecked_from),
			SchemaItem::Enum(e) => e.default_db().map(DbIdent::unchecked_from),
			SchemaItem::Scalar(s) => s.default_db().map(DbIdent::unchecked_from),
			SchemaItem::Composite(s) => s.default_db().map(DbIdent::unchecked_from),
		}
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

#[derive(Clone, Copy, Derivative)]
#[derivative(Debug)]
pub struct SchemaComposite<'a> {
	#[derivative(Debug = "ignore")]
	pub schema: &'a Schema,
	pub composite: &'a Composite,
}
impl Deref for SchemaComposite<'_> {
	type Target = Composite;

	fn deref(&self) -> &Self::Target {
		self.composite
	}
}

#[derive(Clone, Copy)]
pub enum SchemaType<'a> {
	Enum(SchemaEnum<'a>),
	Scalar(SchemaScalar<'a>),
	Composite(SchemaComposite<'a>),
}
impl SchemaType<'_> {
	pub fn ident(&self) -> TypeIdent {
		match self {
			SchemaType::Scalar(s) => s.id(),
			SchemaType::Enum(e) => e.id(),
			SchemaType::Composite(e) => e.id(),
		}
	}
	pub fn has_default(&self) -> bool {
		match self {
			SchemaType::Scalar(s) => s
				.annotations
				.iter()
				.any(|a| matches!(a, ScalarAnnotation::Default(_))),
			SchemaType::Enum(_) => false,
			SchemaType::Composite(_) => false,
		}
	}
	pub fn attrlist(&self) -> &AttributeList {
		match self {
			SchemaType::Scalar(s) => &s.attrlist,
			SchemaType::Enum(e) => &e.attrlist,
			SchemaType::Composite(c) => &c.attrlist,
		}
	}
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
impl<T> HasIdent for &T
where
	T: HasIdent,
{
	type Kind = T::Kind;

	fn id(&self) -> Ident<Self::Kind> {
		(*self).id()
	}
}
pub trait HasDefaultDbName {
	type Kind;
	fn default_db(&self) -> Option<DbIdent<Self::Kind>>;
}
impl<T> HasDefaultDbName for &T
where
	T: HasDefaultDbName,
{
	type Kind = T::Kind;

	fn default_db(&self) -> Option<DbIdent<Self::Kind>> {
		(*self).default_db()
	}
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
macro_rules! delegate_name_impls {
	($t:ty, $k:ident) => {
		impl $crate::uid::HasUid for $t {
			fn uid(&self) -> $crate::uid::Uid {
				self.value.uid()
			}
		}
		impl $crate::HasIdent for $t {
			type Kind = $k;
			fn id(&self) -> $crate::ids::Ident<Self::Kind> {
				self.value.id()
			}
		}
		impl $crate::HasDefaultDbName for $t {
			type Kind = $k;
			fn default_db(&self) -> Option<$crate::ids::DbIdent<Self::Kind>> {
				self.value.default_db()
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
