use crate::names::{ColumnDefName, DbType, TypeIdent};
use crate::{w, ColumnDiff, TableColumn};

use super::index::{Constraint, Index};
use super::sql::Sql;

#[derive(Debug)]
pub struct Column {
    pub name: ColumnDefName,
    pub nullable: bool,
    pub ty: TypeIdent,
    pub annotations: Vec<FieldAnnotation>,
}
impl TableColumn<'_> {
    pub fn db_type(&self) -> DbType {
        self.table.schema.native_type(&self.ty)
    }
    pub fn create_inline(&self, out: &mut String) {
        let name = &self.name;
        let db_type = self.db_type();
        let nullability = if self.nullable { "" } else { " NOT NULL" };
        w!(out, "{name} {db_type}{nullability}")
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
#[derive(Debug)]
pub enum FieldAnnotation {
    Constraint(Constraint),
    Index(Index),
    Default(Sql),
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
    }
}
