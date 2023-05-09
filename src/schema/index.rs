use crate::names::{ColumnIdent, DbColumn, DbIndex};
use crate::{w, TableConstraint, TableIndex};

use super::sql::Sql;

pub enum ConstraintKind {
    PrimaryKey(Vec<ColumnIdent>),
    Unique { columns: Vec<ColumnIdent> },
    Check { sql: Sql },
}
pub struct Constraint {
    kind: ConstraintKind,
    name: Option<String>,
}
impl TableConstraint<'_> {
    fn partial_name(&self) -> String {
        use ConstraintKind::*;
        if let Some(name) = &self.name {
            return name.clone();
        }
        let columns = match &self.kind {
            PrimaryKey(c) => c.clone(),
            Unique { columns } => columns.clone(),
            Check { sql } => sql.affected_columns(),
        };
        let mut out = self.table.format_index_name(columns.into_iter());
        match self.kind {
            PrimaryKey(..) => w!(out, "_pkey"),
            Unique { .. } => w!(out, "_key"),
            Check { .. } => w!(out, "_check"),
        }
        out
    }
    pub fn create_inline(&self, out: &mut String) {
        use ConstraintKind::*;
        let table_name = &self.table.name;
        let partial_name = self.partial_name();
        w!(out, "CONSTRAINT {table_name}_{partial_name} ");
        match &self.kind {
            PrimaryKey(columns) => {
                w!(out, "PRIMARY KEY(");
                self.table
                    .print_column_list(out, columns.into_iter().copied());
                w!(out, ")");
            }
            Unique { columns } => {
                w!(out, "UNIQUE(");
                self.table
                    .print_column_list(out, columns.into_iter().copied());
                w!(out, ")");
            }
            Check { sql } => {
                w!(out, "CHECK(");
                self.table.sql(&sql).print(out);
                w!(out, ")");
            }
        }
    }
    pub fn create_alter(&self, out: &mut String) {
        w!(out, "ADD ");
        self.create_inline(out);
    }
}

#[derive(Debug, Clone)]
pub struct Index {
    unique: bool,
    fields: Vec<ColumnIdent>,
    name: Option<String>,
}

impl Index {
    fn prepend_column(&mut self, column: ColumnIdent) {
        self.fields.insert(0, column);
    }
}
impl TableIndex<'_> {
    pub fn isomorphic_to(&self, other: &Self) -> bool {
        self.unique == other.unique
            && self.db_columns().collect::<Vec<_>>() == other.db_columns().collect::<Vec<_>>()
    }

    fn db_columns(&self) -> impl Iterator<Item = DbColumn> + '_ {
        self.fields.iter().map(|f| self.table.db_name(f))
    }
    fn partial_name(&self) -> String {
        if let Some(name) = &self.name {
            return name.to_owned();
        }
        let mut out = String::new();
        for column in self.db_columns() {
            w!(out, "{column}_");
        }
        if self.unique {
            w!(out, "key")
        } else {
            w!(out, "idx")
        }
        out
    }
    pub fn db_name(&self) -> DbIndex {
        let partial_name = self.partial_name();
        let table_name = &self.table.name;
        DbIndex::new(&format!("{table_name}_{partial_name}"))
    }
    pub fn create(&self, out: &mut String) {
        let name = self.db_name();
        let table_name = &self.table.name;

        w!(out, "CREATE ");
        if self.unique {
            w!(out, "UNIQUE ");
        }
        w!(out, "INDEX {name} ON {table_name}(\n");
        for (i, c) in self.db_columns().enumerate() {
            if i != 0 {
                w!(out, ",");
            }
            w!(out, "\t{c}\n");
        }
        w!(out, ");\n");
    }
    pub fn drop(&self, out: &mut String) {
        let name = self.db_name();
        w!(out, "DROP INDEX {name};\n")
    }
    pub fn rename(&self, new_name: DbIndex, out: &mut String) {
        let name = self.db_name();
        if name == new_name {
            return;
        }
        w!(out, "ALTER INDEX {name} RENAME TO {new_name};\n")
    }
}
