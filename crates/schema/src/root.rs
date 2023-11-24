use std::collections::HashMap;

use super::{
	scalar::{Enum, Scalar},
	table::Table,
};
use crate::{
	changelist::{mk_change_list, ChangeList},
	column::SchemaType,
	names::{DbNativeType, DbTable, DbType, TableIdent, TypeIdent},
	process::{NamingConvention, Pgnc},
	sql::Sql,
	uid::{RenameExt, RenameMap},
	HasIdent, SchemaDiff, SchemaEnum, SchemaItem, SchemaScalar, SchemaSql, SchemaTable,
};

// newty_enum!(
// pub enum Item {
// 	Table = table,
// 	Enum = enum,
// 	Scalar = scalar,
// }
// );
#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub enum Item {
	#[derivative(Debug = "transparent")]
	Table(Table),
	#[derivative(Debug = "transparent")]
	Enum(Enum),
	#[derivative(Debug = "transparent")]
	Scalar(Scalar),
}
impl Item {
	pub fn is_table(&self) -> bool {
		matches!(self, Self::Table(_))
	}
	pub fn as_table(&self) -> Option<&Table> {
		match self {
			Self::Table(value) => Some(value),
			_ => None,
		}
	}
	pub fn as_table_mut(&mut self) -> Option<&mut Table> {
		match self {
			Self::Table(value) => Some(value),
			_ => None,
		}
	}
	pub fn is_enum(&self) -> bool {
		matches!(self, Self::Enum(_))
	}
	pub fn as_enum(&self) -> Option<&Enum> {
		match self {
			Self::Enum(value) => Some(value),
			_ => None,
		}
	}
	pub fn as_enum_mut(&mut self) -> Option<&mut Enum> {
		match self {
			Self::Enum(value) => Some(value),
			_ => None,
		}
	}
	pub fn is_scalar(&self) -> bool {
		matches!(self, Self::Scalar(_))
	}
	pub fn as_scalar(&self) -> Option<&Scalar> {
		match self {
			Self::Scalar(value) => Some(value),
			_ => None,
		}
	}
	pub fn as_scalar_mut(&mut self) -> Option<&mut Scalar> {
		match self {
			Self::Scalar(value) => Some(value),
			_ => None,
		}
	}
}

pub struct SchemaProcessOptions {
	/// If not - then every scalar is transformed to a regular type by inlining.
	pub generator_supports_domain: bool,
	/// How to create item names, how annotation deduplication should be handled, etc.
	pub naming_convention: NamingConvention,
}

#[derive(Debug, Default)]
pub struct Schema(pub Vec<Item>);
impl Schema {
	pub fn process(&mut self, options: &SchemaProcessOptions, rn: &mut RenameMap) {
		self.0.sort_by_key(|i| match i {
			Item::Table(_) => 1,
			Item::Enum(_) => 0,
			Item::Scalar(_) => 9999,
		});

		let mut propagated_scalars = HashMap::new();
		for s in self.0.iter_mut().filter_map(Item::as_scalar_mut) {
			let inline = s.is_always_inline() || !options.generator_supports_domain;
			let data = s.propagate(inline);
			propagated_scalars.insert(s.id(), data);
		}

		// Propagate scalar annotations to columns
		for table in self.0.iter_mut().filter_map(Item::as_table_mut) {
			for (id, data) in &propagated_scalars {
				table.propagate_scalar_data(*id, data);
			}
			table.process();
		}

		match options.naming_convention {
			NamingConvention::Postgres => (Pgnc(self)).process_naming(rn),
		};
	}
	pub fn material_items(&self) -> Vec<SchemaItem<'_>> {
		self.0
			.iter()
			.filter_map(|v| {
				Some(match v {
					Item::Table(table) => SchemaItem::Table(SchemaTable {
						schema: self,
						table,
					}),
					Item::Enum(en) => SchemaItem::Enum(SchemaEnum { schema: self, en }),
					Item::Scalar(scalar) if !scalar.is_always_inline() => {
						SchemaItem::Scalar(SchemaScalar {
							schema: self,
							scalar,
						})
					}
					_ => return None,
				})
			})
			.collect()
	}
	pub fn items(&self) -> Vec<SchemaItem<'_>> {
		self.0
			.iter()
			.map(|v| match v {
				Item::Table(table) => SchemaItem::Table(SchemaTable {
					schema: self,
					table,
				}),
				Item::Enum(en) => SchemaItem::Enum(SchemaEnum { schema: self, en }),
				Item::Scalar(scalar) => SchemaItem::Scalar(SchemaScalar {
					schema: self,
					scalar,
				}),
			})
			.collect()
	}
	pub fn schema_ty(&self, name: TypeIdent) -> SchemaType<'_> {
		if let Some(scalar) = self.scalars().find(|s| s.id() == name) {
			return SchemaType::Scalar(SchemaScalar {
				schema: self,
				scalar,
			});
		}
		if let Some(en) = self.enums().find(|s| s.id() == name) {
			return SchemaType::Enum(SchemaEnum { schema: self, en });
		}
		panic!("type not found: {name:?}")
	}
	pub fn native_type(&self, name: &TypeIdent, rn: &RenameMap) -> DbNativeType {
		for item in self.0.iter() {
			match item {
				Item::Enum(e) if &e.id() == name => return e.db_type(rn),
				Item::Scalar(v) if &v.id() == name => return v.native(rn),
				_ => continue,
			}
		}
		panic!("type not found: {name:?}")
	}
	pub fn tables(&self) -> impl Iterator<Item = &Table> {
		self.0.iter().filter_map(Item::as_table)
	}
	pub fn enums(&self) -> impl Iterator<Item = &Enum> {
		self.0.iter().filter_map(Item::as_enum)
	}
	pub fn scalars(&self) -> impl Iterator<Item = &Scalar> {
		self.0.iter().filter_map(Item::as_scalar)
	}

	pub fn schema_table(&self, name: &TableIdent) -> Option<SchemaTable<'_>> {
		self.tables()
			.find(|t| &t.id() == name)
			.map(|t| SchemaTable {
				schema: self,
				table: t,
			})
	}

	pub fn schema_scalar(&self, scalar: TypeIdent) -> SchemaScalar<'_> {
		SchemaScalar {
			schema: self,
			scalar: self
				.scalars()
				.find(|c| c.id() == scalar)
				.expect("column not found"),
		}
	}

	pub fn db_tables(&self, rn: &RenameMap) -> Vec<DbTable> {
		self.tables().map(|t| t.db(rn)).collect()
	}
	pub fn db_enums(&self, rn: &RenameMap) -> Vec<DbType> {
		self.enums().map(|t| t.db(rn)).collect()
	}
	pub fn sql<'a>(&'a self, sql: &'a Sql) -> SchemaSql<'_> {
		SchemaSql { schema: self, sql }
	}
}
impl SchemaDiff<'_> {
	pub fn changelist(&self, rn: &RenameMap) -> ChangeList<SchemaItem<'_>> {
		let old = self.old.material_items();
		let new = self.new.material_items();
		mk_change_list(rn, &old, &new)
	}
}
