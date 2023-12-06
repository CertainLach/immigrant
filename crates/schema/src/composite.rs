use std::ops::Deref;

use crate::{
	attribute::AttributeList,
	def_name_impls, derive_is_isomorph_by_id_name,
	names::{
		CompositeItemDefName, DbNativeType, FieldIdent, FieldKind, TypeDefName, TypeIdent, TypeKind,
	},
	uid::{next_uid, RenameExt, RenameMap, Uid},
	HasIdent, IsCompatible, SchemaComposite,
};

#[derive(Debug, Clone)]
pub struct Field {
	uid: Uid,
	name: CompositeItemDefName,
	pub ty: TypeIdent,
}
def_name_impls!(Field, FieldKind);
derive_is_isomorph_by_id_name!(Field);

impl Field {
	pub fn new(name: CompositeItemDefName, ty: TypeIdent) -> Self {
		Self {
			uid: next_uid(),
			name,
			ty,
		}
	}
}

#[derive(Debug)]
pub struct Composite {
	uid: Uid,
	name: TypeDefName,
	pub attrlist: AttributeList,
	pub fields: Vec<Field>,
}
def_name_impls!(Composite, TypeKind);

impl Composite {
	pub fn new(attrlist: AttributeList, name: TypeDefName, fields: Vec<Field>) -> Self {
		Self {
			uid: next_uid(),
			name,
			attrlist,
			fields,
		}
	}
	pub fn db_type(&self, rn: &RenameMap) -> DbNativeType {
		DbNativeType::unchecked_from(self.db(rn))
	}
}

impl SchemaComposite<'_> {
	pub fn field(&self, field: FieldIdent) -> CompositeField<'_> {
		self.fields()
			.find(|c| c.id() == field)
			.expect("field not found")
	}
	pub fn fields(&self) -> impl Iterator<Item = CompositeField<'_>> {
		self.fields.iter().map(|field| CompositeField {
			composite: *self,
			field,
		})
	}
}

#[derive(Debug, Clone)]
pub struct CompositeField<'a> {
	composite: SchemaComposite<'a>,
	field: &'a Field,
}
def_name_impls!(CompositeField<'_>, FieldKind);
derive_is_isomorph_by_id_name!(CompositeField<'_>);

impl IsCompatible for CompositeField<'_> {
	fn is_compatible(&self, new: &Self, rn: &RenameMap) -> bool {
		self.name.db == new.name.db && self.db_type(rn) == new.db_type(rn)
	}
}

impl Deref for CompositeField<'_> {
	type Target = Field;

	fn deref(&self) -> &Self::Target {
		self.field
	}
}

impl CompositeField<'_> {
	pub fn db_type(&self, rn: &RenameMap) -> DbNativeType {
		self.composite.schema.native_type(&self.ty, rn)
	}
}
