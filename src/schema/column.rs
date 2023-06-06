use std::mem;

use crate::names::{ColumnDefName, DbType, TypeIdent};
use crate::{w, ColumnDiff, TableColumn};

use super::index::{Constraint, Index};
use super::sql::Sql;
use super::table::{ForeignKey, TableAnnotation};

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
	pub fn apply_scalar_annotations(
		&mut self,
		scalar: TypeIdent,
		annotations: &[ColumnAnnotation],
	) {
		if self.ty == scalar {
			self.annotations.extend(annotations.iter().cloned())
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
}
impl TableColumn<'_> {
	pub fn db_type(&self) -> DbType {
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
	pub fn create_inline(&self, out: &mut String) {
		let name = &self.name;
		let db_type = self.db_type();
		let nullability = if self.nullable { "" } else { " NOT NULL" };
		let defaultability = if let Some(default) = self.default() {
			let sql = self.table.format_sql(default);
			format!(" DEFAULT ({sql})")
		} else {
			"".to_owned()
		};
		w!(out, "{name} {db_type}{nullability}{defaultability}")
	}
	pub fn drop_alter(&self, out: &mut Vec<String>) {
		let name = &self.name;
		out.push(format!("DROP COLUMN {name}"))
	}
	pub fn create_alter(&self, out: &mut Vec<String>) {
		let mut inl = String::new();
		self.create_inline(&mut inl);
		out.push(format!("ADD COLUMN {inl}"))
	}
}

impl ColumnDiff<'_> {
	pub fn print_alter(&self, out: &mut Vec<String>) {
		let name = &self.new.name;
		let new_ty = self.new.db_type();
		if self.old.db_type() != new_ty {
			out.push(format!("ALTER COLUMN {name} TYPE {new_ty}"));
		}
		let new_nullable = self.new.nullable;
		if self.old.nullable != new_nullable {
			if new_nullable {
				out.push(format!("ALTER COLUMN {name} DROP NOT NULL"));
			} else {
				out.push(format!("ALTER COLUMN {name} SET NOT NULL"));
			}
		}

		let old_default = self.old.default();
		let new_default = self.new.default();
		match (old_default, new_default) {
			(None, Some(new_default)) => out.push(format!(
				"ALTER COLUMN {name} SET DEFAULT {}",
				self.new.table.format_sql(new_default)
			)),
			(Some(_), None) => out.push(format!("ALTER COLUMN {name} DROP DEFAULT")),
			(Some(old_default), Some(new_default)) => {
				let old_default = self.old.table.format_sql(old_default);
				let new_default = self.new.table.format_sql(new_default);
				if new_default != old_default {
					out.push(format!("ALTER COLUMN {name} SET DEFAULT {new_default}",));
				}
			}
			(None, None) => {}
		}
	}
}
