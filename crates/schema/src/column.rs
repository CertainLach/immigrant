use std::mem;

use super::{
	index::{Constraint, Index},
	sql::Sql,
	table::{ForeignKey, TableAnnotation},
};
use crate::{
	index::ConstraintTy,
	names::{ColumnDefName, DbNativeType, TypeIdent},
	scalar::InlinedScalar,
	SchemaEnum, SchemaScalar, TableColumn,
};

#[derive(Debug, Clone)]
pub enum ColumnAnnotation {
	Constraint(Constraint),
	Index(Index),
	Default(Sql),
}
impl ColumnAnnotation {
	fn is_table_bound(&self) -> bool {
		matches!(self, Self::Constraint(_) | Self::Index(_))
	}
	fn as_default(&self) -> Option<&Sql> {
		match self {
			Self::Default(s) => Some(s),
			_ => None,
		}
	}
}

#[derive(Debug)]
pub struct Column {
	pub docs: Vec<String>,
	pub name: ColumnDefName,
	pub nullable: bool,
	pub ty: TypeIdent,
	pub annotations: Vec<ColumnAnnotation>,
	pub foreign_key: Option<PartialForeignKey>,
}

#[derive(Debug)]
pub struct PartialForeignKey {
	pub fk: ForeignKey,
}

impl Column {
	pub(crate) fn apply_inlined_scalar(&mut self, scalar: TypeIdent, inlined: &InlinedScalar) {
		if self.ty == scalar {
			self.annotations.extend(inlined.annotations.iter().cloned())
		}
	}
	pub fn propagate_annotations(&mut self) -> Vec<TableAnnotation> {
		let (table, column) = mem::take(&mut self.annotations)
			.into_iter()
			.partition(ColumnAnnotation::is_table_bound);
		self.annotations = column;

		let mut out = Vec::new();
		for annotation in table {
			match annotation {
				ColumnAnnotation::Index(mut i) => {
					i.propagate_to_table(self.name.id());
					out.push(TableAnnotation::Index(i));
				}
				ColumnAnnotation::Constraint(mut c) => {
					c.propagate_to_table(self.name.id());
					out.push(TableAnnotation::Constraint(c));
				}
				_ => unreachable!(),
			}
		}
		out
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
		for ele in self.table.constraints() {
			if let ConstraintTy::PrimaryKey(columns) = &ele.kind {
				if columns.contains(&self.name.id()) {
					return true;
				}
			}
		}
		false
	}
	pub fn is_pk_full(&self) -> bool {
		// FIXME: Will be broken after adding pk merging
		for ele in self.table.constraints() {
			if let ConstraintTy::PrimaryKey(columns) = &ele.kind {
				if columns.contains(&self.name.id()) && columns.len() == 1 {
					return true;
				}
			}
		}
		false
	}
}
