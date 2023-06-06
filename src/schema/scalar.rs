use rand::distributions::DistString;
use rand::thread_rng;

use crate::names::{DbEnumItem, DbType, EnumItemDefName, TypeDefName, TypeIdent};
use crate::{w, wl, EnumDiff};

use super::column::ColumnAnnotation;
use super::index::Constraint;
use super::sql::Sql;

#[derive(Debug)]
pub struct Enum {
	pub name: TypeDefName,
	pub items: Vec<EnumItemDefName>,
}
impl Enum {
	pub fn db_items(&self) -> Vec<DbEnumItem> {
		self.items.iter().map(|i| i.db()).collect()
	}
	pub fn create(&self, out: &mut String) {
		let db_name = &self.name;
		w!(out, "CREATE TYPE {db_name} AS ENUM (\n");
		for (i, v) in self.items.iter().enumerate() {
			if i != 0 {
				w!(out, ",");
			}
			w!(out, "\t'{v}'\n");
		}
		wl!(out, ");");
		wl!(out,);
	}
	pub fn drop(&self, out: &mut String) {
		let db_name = &self.name;
		w!(out, "DROP TYPE {db_name};\n");
	}
}

impl EnumDiff<'_> {
	fn added_items(&self) -> Vec<DbEnumItem> {
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
	fn removed_items(&self) -> Vec<DbEnumItem> {
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
	pub fn print_added(&self, out: &mut String) {
		let added = self.added_items();
		let db_name = &self.new.name;
		for added in added {
			wl!(out, "ALTER TYPE {db_name} ADD VALUE '{added}';");
		}
	}
	pub fn print_removed(&self, out: &mut String) {
		let name = rand::distributions::Alphanumeric.sample_string(&mut thread_rng(), 5);
		let added = self.removed_items();
		let db_name = &self.old.name;
		for added in added {
			wl!(
				out,
				"ALTER TYPE {db_name} RENAME VALUE '{added}' TO '{added}_removed_{name}';"
			);
		}
	}
}

#[derive(Debug)]
pub struct Scalar {
	// Not TypeDefName, because they are compile-time only
	pub name: TypeIdent,
	pub native: DbType,
	pub annotations: Vec<ScalarAnnotation>,
}
impl Scalar {
	pub fn propagate_to_column(&mut self) -> Vec<ColumnAnnotation> {
		self.annotations
			.drain(..)
			.map(|a| match a {
				ScalarAnnotation::Default(d) => ColumnAnnotation::Default(d),
				ScalarAnnotation::Constraint(c) => ColumnAnnotation::Constraint(c),
			})
			.collect()
	}
}

#[derive(Debug)]
pub enum ScalarAnnotation {
	Default(Sql),
	Constraint(Constraint),
}
