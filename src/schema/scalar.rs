use crate::names::{EnumItemDefName, TypeDefName, TypeIdent, DbType};
use crate::w;

#[derive(Debug)]
pub struct Enum {
    pub name: TypeDefName,
    pub items: Vec<EnumItemDefName>,
}
impl Enum {
    fn create(&self, out: &mut String) {
        let db_name = &self.name;
        w!(out, "CREATE TYPE {db_name} AS ENUM (\n");
        for (i, v) in self.items.iter().enumerate() {
            if i != 0 {
                w!(out, ",");
            }
            w!(out, "\t'{v}'\n");
        }
        w!(out, ");");
    }
}

#[derive(Debug)]
pub struct Scalar {
    // Not TypeDefName, because they are compile-time only
    pub name: TypeIdent,
    pub native: DbType,
}
