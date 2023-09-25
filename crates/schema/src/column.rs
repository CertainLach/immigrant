use std::mem;

use itertools::{Either, Itertools};

use super::{
	index::Index,
	sql::Sql,
	table::{ForeignKey, TableAnnotation},
};
use crate::{
	def_name_impls,
	index::{Check, PrimaryKey, UniqueConstraint},
	names::{ColumnDefName, ColumnIdent, ColumnKind, DbNativeType, TypeIdent},
	scalar::PropagatedScalarData,
	uid::Uid,
	SchemaEnum, SchemaScalar, TableColumn,
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
	pub nullable: bool,
	pub ty: TypeIdent,
	pub annotations: Vec<ColumnAnnotation>,
	pub foreign_key: Option<PartialForeignKey>,
}
def_name_impls!(Column, ColumnKind);

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
				.extend(propagated.annotations.iter().cloned())
		}
	}
	pub fn propagate_annotations(&mut self) -> Vec<TableAnnotation> {
		let (annotations, retained) = mem::take(&mut self.annotations)
			.into_iter()
			.partition_map(|a| a.propagate_to_table(self.name.id().clone()));
		self.annotations = retained;
		annotations
	}
	pub fn propagate_foreign_key(&mut self) -> Option<ForeignKey> {
		let mut fk = self.foreign_key.take()?;
		fk.fk.source_fields = Some(vec![self.name.id()]);
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
			SchemaType::Scalar(s) => s.name().id(),
			SchemaType::Enum(e) => e.name().id(),
		}
	}
}

impl TableColumn<'_> {
	pub fn db_type(&self) -> DbNativeType {
		self.table.schema.native_type(&self.ty)
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
		pk.columns.contains(&self.name.id())
	}
	pub fn is_pk_full(&self) -> bool {
		let Some(pk) = self.table.pk() else {
			return false;
		};
		pk.columns == [self.name.id().clone()]
	}
}
