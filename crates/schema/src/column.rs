use std::mem;

use itertools::{Either, Itertools};

use super::{
	index::Index,
	sql::Sql,
	table::{ForeignKey, TableAnnotation},
};
use crate::{
	attribute::AttributeList,
	def_name_impls,
	index::{Check, PrimaryKey, UniqueConstraint},
	names::{ColumnDefName, ColumnIdent, ColumnKind, DbNativeType, TypeIdent},
	scalar::{PropagatedScalarData, ScalarAnnotation},
	uid::{next_uid, RenameMap, Uid},
	HasIdent, SchemaEnum, SchemaScalar, TableColumn,
};

#[derive(Debug, Clone)]
pub enum ColumnAnnotation {
	/// Moved to table.
	Check(Check),
	/// Moved to table.
	Unique(UniqueConstraint),
	/// Moved to table.
	PrimaryKey(PrimaryKey),
	/// Moved to table.
	Index(Index),
	Default(Sql),
}
impl ColumnAnnotation {
	fn as_default(&self) -> Option<&Sql> {
		match self {
			Self::Default(s) => Some(s),
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
}

#[derive(Debug)]
pub struct Column {
	uid: Uid,
	name: ColumnDefName,
	pub docs: Vec<String>,
	pub attrs: AttributeList,
	pub nullable: bool,
	pub ty: TypeIdent,
	pub annotations: Vec<ColumnAnnotation>,
	pub foreign_key: Option<PartialForeignKey>,
}
def_name_impls!(Column, ColumnKind);
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
			self.annotations
				.extend(propagated.annotations.iter().cloned());
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

// FIXME: Move to Schema* defs
pub enum SchemaType<'t> {
	Scalar(SchemaScalar<'t>),
	Enum(SchemaEnum<'t>),
}
impl SchemaType<'_> {
	pub fn ident(&self) -> TypeIdent {
		match self {
			SchemaType::Scalar(s) => s.id(),
			SchemaType::Enum(e) => e.id(),
		}
	}
	pub fn has_default(&self) -> bool {
		match self {
			SchemaType::Scalar(s) => s
				.annotations
				.iter()
				.any(|a| matches!(a, ScalarAnnotation::Default(_))),
			SchemaType::Enum(_) => false,
		}
	}
	pub fn attrlist(&self) -> &AttributeList {
		match self {
			SchemaType::Scalar(s) => &s.attrlist,
			SchemaType::Enum(e) => &e.attrlist,
		}
	}
}

impl TableColumn<'_> {
	pub fn db_type(&self, rn: &RenameMap) -> DbNativeType {
		self.table.schema.native_type(&self.ty, rn)
	}
	pub fn ty(&self) -> SchemaType<'_> {
		self.table.schema.schema_ty(self.ty)
	}
	pub fn default(&self) -> Option<&Sql> {
		let res = self
			.annotations
			.iter()
			.filter_map(|v| v.as_default())
			.collect::<Vec<_>>();
		assert!(res.len() <= 1, "only one default annotation may present");
		res.into_iter().next()
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
