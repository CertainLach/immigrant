use super::{
	scalar::{Enum, Scalar},
	table::Table,
};
use crate::{
	column::SchemaType,
	names::{DbTable, DbType, TableIdent, TypeIdent},
	newty_enum, EnumDiff, SchemaDiff, SchemaEnum, SchemaScalar, SchemaTable, TableDiff,
};

newty_enum!(
#[derive(Debug)]
pub enum Item {
	Table = table,
	Enum = enum,
	Scalar = scalar,
}
);
#[derive(Debug, Default)]
pub struct Schema(pub Vec<Item>);
impl Schema {
	pub fn process(&mut self) {
		self.0.sort_by_key(|i| match i {
			Item::Table(_) => 1,
			Item::Enum(_) => 0,
			Item::Scalar(_) => 9999,
		});

		// Propagate scalar annotations to columns
		{
			let mut annotated_scalars = Vec::new();
			for scalar in self.0.iter_mut().filter_map(Item::as_scalar_mut) {
				annotated_scalars.push((scalar.name, scalar.propagate_to_column()));
			}

			for table in self.0.iter_mut().filter_map(Item::as_table_mut) {
				for (id, annotations) in annotated_scalars.iter() {
					table.apply_scalar_annotations(*id, annotations);
				}
				table.process();
			}
		}
	}
	pub fn schema_ty(&self, name: TypeIdent) -> SchemaType<'_> {
		if let Some(scalar) = self.scalars().find(|s| s.name == name) {
			return SchemaType::Scalar(SchemaScalar {
				schema: self,
				scalar,
			});
		}
		if let Some(en) = self.enums().find(|s| s.name == name) {
			return SchemaType::Enum(SchemaEnum { schema: self, en });
		}
		panic!("type not found: {name:?}")
	}
	pub fn native_type(&self, name: &TypeIdent) -> DbType {
		for item in self.0.iter() {
			match item {
				Item::Enum(e) if &e.name == name => return e.name.db(),
				Item::Scalar(v) if &v.name == name => return v.native.clone(),
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

	pub fn table(&self, name: &DbTable) -> Option<SchemaTable<'_>> {
		self.tables()
			.find(|t| &t.name == name)
			.map(|t| SchemaTable {
				schema: self,
				table: t,
			})
	}
	pub fn schema_table(&self, name: &TableIdent) -> Option<SchemaTable<'_>> {
		self.tables()
			.find(|t| &t.name == name)
			.map(|t| SchemaTable {
				schema: self,
				table: t,
			})
	}
	pub fn r#enum(&self, name: &DbType) -> Option<SchemaEnum<'_>> {
		self.enums().find(|t| &t.name == name).map(|t| SchemaEnum {
			schema: self,
			en: t,
		})
	}

	pub fn schema_scalar(&self, scalar: TypeIdent) -> SchemaScalar<'_> {
		SchemaScalar {
			schema: self,
			scalar: self
				.scalars()
				.find(|c| c.name == scalar)
				.expect("column not found"),
		}
	}

	pub fn db_tables(&self) -> Vec<DbTable> {
		self.tables().map(|t| t.name.db()).collect()
	}
	pub fn db_enums(&self) -> Vec<DbType> {
		self.enums().map(|t| t.name.db()).collect()
	}
}
impl SchemaDiff<'_> {
	pub fn altered_tables(&self) -> Vec<TableDiff<'_>> {
		let old_tables = self.old.db_tables();
		let new_tables = self.new.db_tables();
		let mut altered_names = Vec::new();
		for table in new_tables {
			if old_tables.contains(&table) {
				altered_names.push(table)
			}
		}
		altered_names
			.into_iter()
			.map(|name| TableDiff {
				old: self.old.table(&name).expect("in both new and old"),
				new: self.new.table(&name).expect("in both new and old"),
			})
			.collect()
	}
	pub fn dropped_tables(&self) -> Vec<SchemaTable<'_>> {
		let old_tables = self.old.db_tables();
		let new_tables = self.new.db_tables();

		let mut out = Vec::new();
		for table in old_tables {
			if !new_tables.contains(&table) {
				out.push(table);
			}
		}
		out.into_iter()
			.map(|name| self.old.table(&name).expect("from old table list"))
			.collect()
	}
	pub fn created_tables(&self) -> Vec<SchemaTable<'_>> {
		let old_tables = self.old.db_tables();
		let new_tables = self.new.db_tables();

		let mut out = Vec::new();
		for table in new_tables {
			if !old_tables.contains(&table) {
				out.push(table);
			}
		}
		out.into_iter()
			.map(|name| self.new.table(&name).expect("from new table list"))
			.collect()
	}
	pub fn created_enums(&self) -> Vec<SchemaEnum<'_>> {
		let old_enums = self.old.db_enums();
		let new_enums = self.new.db_enums();

		let mut out = Vec::new();
		for r#enum in new_enums {
			if !old_enums.contains(&r#enum) {
				out.push(r#enum);
			}
		}
		out.into_iter()
			.map(|name| self.new.r#enum(&name).expect("from new enum list"))
			.collect()
	}
	pub fn dropped_enums(&self) -> Vec<SchemaEnum<'_>> {
		let old_enums = self.old.db_enums();
		let new_enums = self.new.db_enums();

		let mut out = Vec::new();
		for r#enum in old_enums {
			if !new_enums.contains(&r#enum) {
				out.push(r#enum);
			}
		}
		out.into_iter()
			.map(|name| self.old.r#enum(&name).expect("from new enum list"))
			.collect()
	}
	pub fn altered_enums(&self) -> Vec<EnumDiff<'_>> {
		let old_enums = self.old.db_enums();
		let new_enums = self.new.db_enums();

		let mut out = Vec::new();
		for r#enum in old_enums {
			if new_enums.contains(&r#enum) {
				out.push(r#enum);
			}
		}
		out.into_iter()
			.map(|name| EnumDiff {
				old: self.old.r#enum(&name).expect("from both new and old"),
				new: self.new.r#enum(&name).expect("from both new and old"),
			})
			.collect()
	}
}
