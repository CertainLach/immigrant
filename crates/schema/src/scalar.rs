use super::{index::Constraint, sql::Sql};
use crate::{
	attribute::AttributeList,
	changelist::IsCompatible,
	column::ColumnAnnotation,
	names::{
		DbEnumItem, DbNativeType, DbType, EnumItemDefName, EnumItemKind, TypeDefName,
		UpdateableEnumItemDefName, UpdateableTypeDefName,
	},
	EnumDiff, HasName,
};

#[derive(Debug)]
pub struct EnumItem {
	name: UpdateableEnumItemDefName,
}
impl EnumItem {
	pub fn new(name: EnumItemDefName) -> Self {
		Self {
			name: UpdateableEnumItemDefName::new(name),
		}
	}
	pub fn set_db(&self, db: DbEnumItem) {
		self.name.set_db(db)
	}
	pub fn db(&self) -> DbEnumItem {
		self.name.name().db()
	}
}
impl HasName for &EnumItem {
	type Kind = EnumItemKind;

	fn name(&self) -> EnumItemDefName {
		self.name.name()
	}
}
impl IsCompatible for &EnumItem {
	fn is_compatible(&self, _new: &Self) -> bool {
		true
	}
}

#[derive(Debug)]
pub struct Enum {
	pub attrlist: AttributeList,
	name: UpdateableTypeDefName,
	pub items: Vec<EnumItem>,
}
impl Enum {
	pub fn new(attrlist: AttributeList, name: TypeDefName, items: Vec<EnumItem>) -> Self {
		Self {
			attrlist,
			name: UpdateableTypeDefName::new(name),
			items,
		}
	}
	pub fn name(&self) -> TypeDefName {
		self.name.name()
	}
	pub fn set_db(&self, db: DbType) {
		self.name.set_db(db)
	}

	pub fn db_items(&self) -> Vec<DbEnumItem> {
		self.items.iter().map(|i| i.db()).collect()
	}
	pub fn db_type(&self) -> DbNativeType {
		DbNativeType::unchecked_from(self.name().db())
	}
}

impl EnumDiff<'_> {
	pub fn added_items(&self) -> Vec<DbEnumItem> {
		let old_items = self.old.db_items();
		let new_items = self.new.db_items();

		let mut out = Vec::new();
		for new_item in new_items.iter() {
			if !old_items.contains(new_item) {
				out.push(new_item.clone())
			}
		}
		out
	}
	pub fn removed_items(&self) -> Vec<DbEnumItem> {
		let old_items = self.old.db_items();
		let new_items = self.new.db_items();

		let mut out = Vec::new();
		for old_item in old_items.iter() {
			if !new_items.contains(old_item) {
				out.push(old_item.clone())
			}
		}
		out
	}
}

#[derive(Debug)]
pub(crate) struct InlinedScalar {
	pub annotations: Vec<ColumnAnnotation>,
}

#[derive(Debug)]
pub struct Scalar {
	pub attrlist: AttributeList,
	name: UpdateableTypeDefName,
	native: DbNativeType,
	pub annotations: Vec<ScalarAnnotation>,
	inlined: bool,
}
impl Scalar {
	pub fn new(
		attrlist: AttributeList,
		name: TypeDefName,
		native: DbNativeType,
		annotations: Vec<ScalarAnnotation>,
	) -> Self {
		Self {
			attrlist,
			name: UpdateableTypeDefName::new(name),
			native,
			annotations,
			inlined: false,
		}
	}
	pub fn name(&self) -> TypeDefName {
		self.name.name()
	}
	pub fn set_db(&self, name: DbType) {
		self.name.set_db(name)
	}
	pub(crate) fn inline(&mut self) -> InlinedScalar {
		let annotations = self
			.annotations
			.iter()
			.cloned()
			.filter_map(|a| match a {
				ScalarAnnotation::Default(d) => Some(ColumnAnnotation::Default(d)),
				ScalarAnnotation::Constraint(c) => Some(ColumnAnnotation::Constraint(c)),
				ScalarAnnotation::Inline => None,
			})
			.collect();
		self.annotations
			.retain(|ann| matches!(ann, ScalarAnnotation::Inline));
		self.inlined = true;
		InlinedScalar { annotations }
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
	pub fn native(&self) -> DbNativeType {
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
			DbNativeType::unchecked_from(self.name().db())
		}
	}
}

#[derive(Debug, Clone)]
pub enum ScalarAnnotation {
	Default(Sql),
	Constraint(Constraint),
	// Always convert to native types, even if database supports domain types
	Inline,
}
impl ScalarAnnotation {
	fn is_inline_target(&self) -> bool {
		matches!(self, Self::Default(_) | Self::Constraint(_))
	}
}
