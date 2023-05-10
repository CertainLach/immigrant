use crate::names::{ColumnIdent, DbColumn, DbIndex, TableDefName};
use crate::{w, Index, SchemaTable, TableColumn, TableDiff, TableIndex, TableItem, ColumnDiff, wl};

use super::column::Column;

#[derive(Debug)]
pub struct Table {
    pub name: TableDefName,
    pub columns: Vec<Column>,
    // indexes: Vec<Index>,
}

impl Table {
    pub fn db_name(&self, column: &ColumnIdent) -> DbColumn {
        for ele in self.columns.iter() {
            if &ele.name == column {
                return ele.name.db();
            }
        }
        unreachable!("unknown field: {column:?}");
    }
    pub fn format_index_name(&self, columns: impl Iterator<Item = ColumnIdent>) -> String {
        let mut out = String::new();
        for (i, column) in columns.enumerate() {
            if i != 0 {
                w!(out, "_");
            }
            let db_name = self.db_name(&column);
            w!(out, "{db_name}");
        }
        assert!(!out.is_empty(), "no columns found");
        out
    }
    pub fn print_column_list(&self, out: &mut String, columns: impl Iterator<Item = ColumnIdent>) {
        for (i, column) in columns.enumerate() {
            if i != 0 {
                w!(out, ", ");
            }
            let db_name = self.db_name(&column);
            w!(out, "{db_name}");
        }
    }
}
impl SchemaTable<'_> {
    fn item<'a, I>(&'a self, value: &'a I) -> TableItem<'a, I> {
        TableItem::unchecked_new(*self, value)
    }
    fn column(&self, name: &DbColumn) -> Option<TableColumn<'_>> {
        self.columns().find(|c| &c.name == name)
    }
    fn columns(&self) -> impl Iterator<Item = TableColumn<'_>> {
        self.columns.iter().map(|i| self.item(i))
    }
    // fn index(&self, name: &DbIndex) -> Option<TableIndex<'_>> {
    //     self.indexes().find(|c| &c.db_name() == name)
    // }
    // fn index_isomophic_to(&self, other: TableIndex<'_>) -> Option<TableIndex<'_>> {
    //     self.indexes().find(|i| i.isomorphic_to(&other))
    // }
    // fn indexes(&self) -> impl Iterator<Item = TableIndex<'_>> {
    //     self.indexes.iter().map(|i| self.item(i))
    // }

    pub fn create(&self, out: &mut String) {
        let table_name = &self.name;
        wl!(out, "CREATE TABLE {table_name} (");
        for (i, v) in self.columns().enumerate() {
            if i != 0 {
                w!(out, ",");
            }
            w!(out, "\t");
            v.create_inline(out);
            wl!(out, "");
        }
        w!(out, ");");
        // for idx in self.indexes() {
        //     idx.create(out);
        // }
        w!(out, "\n");
    }
    pub fn drop(&self, out: &mut String) {
        let table_name = &self.name;
        // for idx in self.indexes() {
        //     idx.drop(out);
        // }
        wl!(out, "DROP TABLE {table_name};");
    }
}
impl TableDiff<'_> {
    pub fn print(&self, out: &mut String) {
        // for old_idx in self.old.indexes() {
        //     if let Some(new_idx) = self.new.index_isomophic_to(old_idx) {
        //         old_idx.rename(new_idx.db_name(), out)
        //     } else {
        //         old_idx.drop(out)
        //     }
        // }
        let mut alternations = Vec::new();
        for old_column in self.old.columns() {
            if let Some(new_column) = self.new.column(&old_column.name.db()) {
                ColumnDiff {
                    old: old_column,
                    new: new_column,
                }.print_alter(&mut alternations);
            } else {
                old_column.drop_alter(&mut alternations);
            }
        }
        for new_column in self.new.columns() {
            if self.old.column(&new_column.name.db()).is_none() {
                new_column.create_alter(&mut alternations)
            }
        }
        if !alternations.is_empty() {
            let name = &self.new.name;
            w!(out, "ALTER TABLE {name}\n");
            for (i, alt) in alternations.iter().enumerate() {
                if i != 0 {
                    w!(out, ",");
                }
                w!(out, "\t{alt}");
            }
            w!(out, ";\n");
        }
        // for new_idx in self.new.indexes() {
        //     if self.old.index_isomophic_to(new_idx).is_none() {
        //         new_idx.create(out)
        //     }
        // }
    }
}
