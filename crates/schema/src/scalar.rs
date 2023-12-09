use std::mem;

use itertools::{Either, Itertools};

use super::sql::Sql;
use crate::{
	attribute::AttributeList,
	changelist::IsCompatible,
	column::ColumnAnnotation,
	composite::FieldAnnotation,
	def_name_impls, derive_is_isomorph_by_id_name,
	index::{Check, Index, PrimaryKey, UniqueConstraint},
	names::{DbEnumItem, DbNativeType, EnumItemDefName, EnumItemKind, TypeDefName, TypeKind},
	uid::{next_uid, RenameExt, RenameMap, Uid},
};

#[derive(Debug, Clone)]
pub struct EnumItem {
	uid: Uid,
	name: EnumItemDefName,
}
impl EnumItem {
	pub fn new(name: EnumItemDefName) -> Self {
		Self {
			uid: next_uid(),
			name,
		}
	}
}
def_name_impls!(EnumItem, EnumItemKind);
derive_is_isomorph_by_id_name!(EnumItem);

impl IsCompatible for EnumItem {
	fn is_compatible(&self, _new: &Self, _rn: &RenameMap) -> bool {
		true
	}
}

#[derive(Debug)]
pub struct Enum {
	uid: Uid,
	name: TypeDefName,
	pub attrlist: AttributeList,
	pub items: Vec<EnumItem>,
}
impl Enum {
	pub fn new(attrlist: AttributeList, name: TypeDefName, items: Vec<EnumItem>) -> Self {
		Self {
			uid: next_uid(),
			name,
			attrlist,
			items,
		}
	}
	pub fn db_items(&self, rn: &RenameMap) -> Vec<DbEnumItem> {
		self.items.iter().map(|i| i.db(rn)).collect()
	}
	pub fn db_type(&self, rn: &RenameMap) -> DbNativeType {
		DbNativeType::unchecked_from(self.db(rn))
	}
}
def_name_impls!(Enum, TypeKind);

#[derive(Debug, Default)]
pub(crate) struct PropagatedScalarData {
	pub annotations: Vec<ColumnAnnotation>,
	pub field_annotations: Vec<FieldAnnotation>,
}
impl PropagatedScalarData {
	/// Returns true if extended
	pub(crate) fn extend(&mut self, other: Self) {
		self.annotations.extend(other.annotations.into_iter());
		self.field_annotations
			.extend(other.field_annotations.into_iter());
	}
	pub(crate) fn is_empty(&self) -> bool {
		self.annotations.is_empty() && self.field_annotations.is_empty()
	}
}

/// Even though CREATE DOMAIN allows domains to be constrained as non-nulls, currently it is not possible to make
/// scalar non-null, you should create a constraint, and then make a column not null, because non-null domain
/// might work not as user would expect. I.e it allows value to be null in case of outer joins.
#[derive(Debug)]
pub struct Scalar {
	uid: Uid,
	name: TypeDefName,
	pub attrlist: AttributeList,
	/// TODO: There is no support for making one scalar depend on other scalar/enum.
	/// Should this support be implemented? Should there be dedicated syntax for SQL types?
	native: DbNativeType,
	pub annotations: Vec<ScalarAnnotation>,
	inlined: bool,
}
def_name_impls!(Scalar, TypeKind);
impl Scalar {
	pub fn new(
		attrlist: AttributeList,
		name: TypeDefName,
		native: DbNativeType,
		annotations: Vec<ScalarAnnotation>,
	) -> Self {
		Self {
			uid: next_uid(),
			name,
			attrlist,
			native,
			annotations,
			inlined: false,
		}
	}
	pub(crate) fn propagate(&mut self, inline: bool) -> PropagatedScalarData {
		let annotations = mem::take(&mut self.annotations);
		let field_annotations = annotations
			.iter()
			.flat_map(|a| a.propagate_to_field(inline))
			.collect_vec();
		let (annotations, retained) = annotations
			.into_iter()
			.partition_map(|a| a.propagate_to_column(inline));
		self.annotations = retained;
		if inline {
			self.inlined = true;
		}
		PropagatedScalarData {
			annotations,
			field_annotations,
		}
	}
	pub fn is_always_inline(&self) -> bool {
		self.annotations
			.iter()
			.any(|a| matches!(a, ScalarAnnotation::Inline))
	}
	pub fn inlined(&self) -> bool {
		self.inlined
	}
	pub fn inner_type(&self) -> DbNativeType {
		self.native.clone()
	}
	pub fn native(&self, rn: &RenameMap) -> DbNativeType {
		assert!(
			!self.is_always_inline() || self.inlined,
			"always-inline type was not inlined"
		);
		if self.inlined {
			assert!(
				!self
					.annotations
					.iter()
					.any(ScalarAnnotation::is_inline_target),
				"inlined scalars may not have inline target scalars"
			);
			self.native.clone()
		} else {
			DbNativeType::unchecked_from(self.db(rn))
		}
	}
}

#[derive(Debug, Clone)]
pub enum ScalarAnnotation {
	/// Moved to column if inlined.
	Default(Sql),
	/// Moved to column if inlined.
	Check(Check),
	/// Moved to column.
	PrimaryKey(PrimaryKey),
	/// Moved to column.
	Unique(UniqueConstraint),
	/// Moved to column.
	Index(Index),
	// Always convert to native types, even if database supports domain types.
	Inline,
}
impl ScalarAnnotation {
	pub fn as_check(&self) -> Option<&Check> {
		match self {
			Self::Check(c) => Some(c),
			_ => None,
		}
	}
	/// Should annotation be removed after inlining?
	fn is_inline_target(&self) -> bool {
		!matches!(self, Self::Inline)
	}
	fn propagate_to_field(&self, inline: bool) -> Option<FieldAnnotation> {
		Some(match self {
			ScalarAnnotation::Check(c) if inline => FieldAnnotation::Check(c.clone()),
			_ => return None,
		})
	}
	fn propagate_to_column(self, inline: bool) -> Either<ColumnAnnotation, Self> {
		Either::Left(match self {
			ScalarAnnotation::PrimaryKey(p) => ColumnAnnotation::PrimaryKey(p),
			ScalarAnnotation::Unique(u) => ColumnAnnotation::Unique(u),
			ScalarAnnotation::Index(i) => ColumnAnnotation::Index(i),
			ScalarAnnotation::Default(d) if inline => ColumnAnnotation::Default(d),
			ScalarAnnotation::Check(c) if inline => ColumnAnnotation::Check(c),
			_ => return Either::Right(self),
		})
	}
}
