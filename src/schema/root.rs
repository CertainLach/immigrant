use crate::names::{DbTable, DbType, TypeIdent};
use crate::{SchemaDiff, SchemaTable, TableDiff};

use super::scalar::{Enum, Scalar};
use super::table::Table;

#[derive(Debug)]
pub enum Item {
    Table(Table),
    Enum(Enum),
    Scalar(Scalar),
}
impl Item {
    fn as_table(&self) -> Option<&Table> {
        match self {
            Item::Table(t) => Some(t),
            _ => None,
        }
    }
}
#[derive(Debug, Default)]
pub struct Schema(pub Vec<Item>);
impl Schema {
    pub fn process(&mut self) {
        self.0.sort_by_key(|i| match i {
            Item::Table(_) => 1,
            Item::Enum(_) => 0,
            Item::Scalar(_) => 9999,
        })
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

    pub fn table(&self, name: &DbTable) -> Option<SchemaTable<'_>> {
        self.tables()
            .find(|t| &t.name == name)
            .map(|t| SchemaTable {
                schema: self,
                table: t,
            })
    }

    pub fn db_tables(&self) -> Vec<DbTable> {
        self.tables().map(|t| t.name.db()).collect()
    }

    pub fn diff(&self, old: &Self, out: &mut String) {
        SchemaDiff { old, new: self }.print(out)
    }
    pub fn create(&self, out: &mut String) {
        self.diff(&Schema::default(), out)
    }
    pub fn drop(&self, out: &mut String) {
        Schema::default().diff(self, out)
    }
}
impl SchemaDiff<'_> {
    fn altered_tables(&self) -> Vec<TableDiff<'_>> {
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
    fn dropped_tables(&self) -> Vec<SchemaTable<'_>> {
        let old_tables = self.old.db_tables();
        let new_tables = self.new.db_tables();

        let mut out = Vec::new();
        for table in old_tables {
            if !new_tables.contains(&table) {
                out.push(table);
            }
        }
        out.into_iter().map(|name| self.old.table(&name).expect("from old table list")).collect()
    }
    fn created_tables(&self) -> Vec<SchemaTable<'_>> {
        let old_tables = self.old.db_tables();
        let new_tables = self.new.db_tables();

        let mut out = Vec::new();
        for table in new_tables {
            if !old_tables.contains(&table) {
                out.push(table);
            }
        }
        out.into_iter().map(|name| self.new.table(&name).expect("from new table list")).collect()
    }
    pub fn print(&self, out: &mut String) {
        for diff in self.altered_tables() {
            diff.print(out);
        }
        for table in self.dropped_tables() {
            table.drop(out);
        }
        for table in self.created_tables() {
            table.create(out);
        }
    }
}
