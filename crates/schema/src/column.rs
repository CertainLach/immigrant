use std::mem;

use itertools::{Either, Itertools};

use super::{
	index::Index,
	sql::Sql,
	table::{ForeignKey, TableAnnotation},
};
use crate::{
	attribute::AttributeList,
	changelist::IsCompatible,
	def_name_impls, derive_is_isomorph_by_id_name,
	index::{Check, PrimaryKey, UniqueConstraint},
	names::{ColumnDefName, ColumnIdent, ColumnKind, DbNativeType, TypeIdent},
	scalar::PropagatedScalarData,
	uid::{next_uid, OwnUid, RenameMap, Uid},
	HasIdent, SchemaType, TableColumn,
};

#[derive(Debug)]
pub enum ColumnAnnotation {
	/// Moved to table.
	Check(Check),
	/// Moved to table.
	Unique(UniqueConstraint),
	/// Moved to table.
	PrimaryKey(PrimaryKey),
	/// Moved to table.
	Index(Index),
	/// Column default annotation.
	Default(Sql),
	/// If column is created, then this attribute is used to prefill it, treat it as one-time, much more powerful
	/// DEFAULT value.
	/// It can be implemented as an UPDATE statement, or with SET TYPE ... USING, but not as one-time DEFAULT, because
	/// it can reference other columns.
	InitializeAs(Sql),
}
impl ColumnAnnotation {
	fn as_default(&self) -> Option<&Sql> {
		match self {
			Self::Default(s) => Some(s),
			_ => None,
		}
	}
	fn as_initialize_as(&self) -> Option<&Sql> {
		match self {
			Self::InitializeAs(s) => Some(s),
			_ => None,
		}
	}
	fn propagate_to_table(self, column: ColumnIdent) -> Either<TableAnnotation, Self> {
		Either::Left(match self {
			ColumnAnnotation::Check(c) => TableAnnotation::Check(c.propagate_to_table(column)),
			ColumnAnnotation::Unique(u) => TableAnnotation::Unique(u.propagate_to_table(column)),
			ColumnAnnotation::PrimaryKey(p) => {
				TableAnnotation::PrimaryKey(p.propagate_to_table(column))
			}
			ColumnAnnotation::Index(i) => TableAnnotation::Index(i.propagate_to_table(column)),
			_ => return Either::Right(self),
		})
	}
	fn clone_for_propagate(&self) -> Self {
		match self {
			ColumnAnnotation::Check(c) => Self::Check(c.clone_for_propagate()),
			ColumnAnnotation::Unique(u) => Self::Unique(u.clone_for_propagate()),
			ColumnAnnotation::PrimaryKey(p) => Self::PrimaryKey(p.clone_for_propagate()),
			ColumnAnnotation::Index(i) => Self::Index(i.clone_for_propagate()),
			ColumnAnnotation::Default(d) => Self::Default(d.clone()),
			ColumnAnnotation::InitializeAs(i) => Self::InitializeAs(i.clone()),
		}
	}
}

#[derive(Debug)]
pub struct Column {
	uid: OwnUid,
	name: ColumnDefName,
	pub docs: Vec<String>,
	pub attrs: AttributeList,
	pub nullable: bool,
	pub ty: TypeIdent,
	pub annotations: Vec<ColumnAnnotation>,
	pub foreign_key: Option<PartialForeignKey>,
}
def_name_impls!(Column, ColumnKind);
derive_is_isomorph_by_id_name!(Column);
impl Column {
	pub fn new(
		name: ColumnDefName,
		docs: Vec<String>,
		attrs: AttributeList,
		nullable: bool,
		ty: TypeIdent,
		annotations: Vec<ColumnAnnotation>,
		foreign_key: Option<PartialForeignKey>,
	) -> Self {
		Self {
			uid: next_uid(),
			attrs,
			name,
			docs,
			nullable,
			ty,
			annotations,
			foreign_key,
		}
	}
}
impl IsCompatible for Column {
	fn is_compatible(&self, _new: &Self, _rn: &RenameMap) -> bool {
		true
	}
}

#[derive(Debug)]
pub struct PartialForeignKey {
	pub fk: ForeignKey,
}

impl Column {
	pub(crate) fn propagate_scalar_data(
		&mut self,
		scalar: TypeIdent,
		propagated: &PropagatedScalarData,
	) {
		if self.ty == scalar {
			self.annotations.extend(
				propagated
					.annotations
					.iter()
					.map(|v| v.clone_for_propagate()),
			);
		}
	}
	pub fn propagate_annotations(&mut self) -> Vec<TableAnnotation> {
		let (annotations, retained) = mem::take(&mut self.annotations)
			.into_iter()
			.partition_map(|a| a.propagate_to_table(self.id()));
		self.annotations = retained;
		annotations
	}
	pub fn propagate_foreign_key(&mut self) -> Option<ForeignKey> {
		let mut fk = self.foreign_key.take()?;
		fk.fk.source_fields = Some(vec![self.id()]);
		Some(fk.fk)
	}
}

impl<'a> TableColumn<'a> {
	pub fn db_type(&self, rn: &RenameMap) -> DbNativeType {
		self.table.schema.native_type(&self.ty, rn)
	}
	pub fn ty(&'a self) -> SchemaType<'a> {
		self.table.schema.schema_ty(self.ty)
	}
	/// Only returns column default, if the underlying type has default value -
	/// it needs to be handled manually.
	/// If you only want to check if default exists - use has_default.
	pub fn default(&self) -> Option<&Sql> {
		self.annotations
			.iter()
			.filter_map(|v| v.as_default())
			.at_most_one()
			.unwrap()
	}
	pub fn initialize_as(&self) -> Option<&Sql> {
		self.annotations
			.iter()
			.filter_map(|v| v.as_initialize_as())
			.at_most_one()
			.unwrap()
	}
	pub fn is_pk_part(&self) -> bool {
		let Some(pk) = self.table.pk() else {
			return false;
		};
		pk.columns.contains(&self.id())
	}
	pub fn has_default(&self) -> bool {
		self.default().is_some() || self.ty().has_default()
	}
	pub fn is_pk_full(&self) -> bool {
		let Some(pk) = self.table.pk() else {
			return false;
		};
		pk.columns == [self.id()]
	}
}
