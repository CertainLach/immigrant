use peg::parser;
use std::fmt::Write;
use std::ops::Deref;

use self::ids::DbIdent;
use self::names::{
    ColumnDefName, ColumnIdent, DbColumn, DbProcedure, DbTable, DbType, TableDefName, TypeIdent,
};
use self::schema::column::Column;
use self::schema::index::{Constraint, Index};
use self::schema::scalar::Enum;
use self::schema::sql::Sql;
use schema::{root::Schema, table::Table};

pub mod ids;
pub mod names;
pub mod schema;

#[macro_export]
macro_rules! w {
    ($out:expr, $($tt:tt)*) => {{
        use std::fmt::Write;
        write!($out, $($tt)*).unwrap();
    }};
}
#[macro_export]
macro_rules! wl {
    ($out:expr, $($tt:tt)*) => {{
        use std::fmt::Write;
        writeln!($out, $($tt)*).unwrap();
    }};
}

pub struct TableItem<'a, I> {
    pub table: SchemaTable<'a>,
    value: &'a I,
}
impl<'a, I> TableItem<'a, I> {
    pub fn unchecked_new(table: SchemaTable<'a>, value: &'a I) -> Self {
        Self { table, value }
    }
}
impl<I> Deref for TableItem<'_, I> {
    type Target = I;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<I> Clone for TableItem<'_, I> {
    fn clone(&self) -> Self {
        Self {
            table: self.table,
            value: self.value,
        }
    }
}
impl<I> Copy for TableItem<'_, I> {}
pub type TableIndex<'a> = TableItem<'a, Index>;
pub type TableColumn<'a> = TableItem<'a, Column>;
pub type TableConstraint<'a> = TableItem<'a, Constraint>;
pub type TableSql<'a> = TableItem<'a, Sql>;

pub struct Diff<I> {
    old: I,
    new: I,
}
pub type SchemaDiff<'a> = Diff<&'a Schema>;
pub type TableDiff<'a> = Diff<SchemaTable<'a>>;
pub type ColumnDiff<'a> = Diff<TableColumn<'a>>;

#[derive(Clone, Copy)]
pub struct SchemaEnum<'a> {
    pub schema: &'a Schema,
    pub en: &'a Enum,
}
impl Deref for SchemaEnum<'_> {
    type Target = Enum;

    fn deref(&self) -> &Self::Target {
        &self.en
    }
}

#[derive(Clone, Copy)]
pub struct SchemaTable<'a> {
    pub schema: &'a Schema,
    pub table: &'a Table,
}
impl SchemaTable<'_> {
    fn sql<'a>(&'a self, sql: &'a Sql) -> TableSql<'a> {
        TableSql {
            table: *self,
            value: sql,
        }
    }
    fn format_sql(&self, sql: &Sql) -> String {
        let mut out = String::new();
        self.sql(sql).print(&mut out);
        out
    }
}
impl Deref for SchemaTable<'_> {
    type Target = Table;

    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

fn h<T>(v: T) -> Box<T> {
    Box::new(v)
}
// parser! {
// grammar schema_parser() for str {
// pub(super) rule root() -> Schema = _ t:item()**_ _ {Schema(t)}
//
// rule item() -> Item
// = t:table() {Item::Table(t)}
// / t:enum() {Item::Enum(t)}
// / t:scalar() {Item::Scalar(t)}
//
// rule table() -> Table = "table" _ name:table_ident() _ "{" _ indexes:index()**_ _ fields:table_field()++_ _ "}" {{
//     let mut indexes = indexes;
//     for (f, findexes) in fields.iter(){
//         for mut findex in findexes.iter().cloned() {
//             findex.prepend_column(f.name.schema_name.clone());
//             indexes.push(findex);
//         }
//     }
//     Table {
//         indexes,
//         name,
//         fields: fields.into_iter().map(|f|f.0).collect(),
//     }
// }};
// rule enum() -> Enum = "enum" _ name:table_ident() _ "{" _ items:enum_item()++_ _ "}" {
//     Enum {
//         name,
//         items,
//     }
// };
// rule scalar() -> Scalar = "scalar" _ name:code_ident() _ native:str() {
//     Scalar {
//         name,
//         native: native.to_string()
//     }
// }
// rule table_field() -> (Column, Vec<Index>) = name:field_ident() t:(_ ":" _ i:code_ident() {i})? n:(_ "?")? _ indexes:index()**_ ";" {
//     (Column {
//         nullable: n.is_some(),
//         ty: t.unwrap_or_else(|| {
//             name.schema_name.clone()
//         }),
//         name,
//     }, indexes)
// }
// rule enum_item() -> Ident = c:code_ident() d:(_ d:str() {d})? ";" {
//     if let Some(d) = d {
//         Ident {
//             schema_name: c.clone(),
//             db: DbIdent(d.to_owned()),
//         }
//     } else {
//        Ident {
//             schema_name: c.clone(),
//             db: DbIdent(c.0),
//        }
//     }
// }
// rule field_ident() -> Ident = c:code_ident() d:(_ d:db_ident() {d})? {
//     if let Some(d) = d {
//         Ident {
//             schema_name: c.clone(),
//             db: d,
//         }
//     } else {
//         Ident {
//             schema_name: c.clone(),
//             db: DbIdent(c.0),
//         }
//     }
// };
// rule table_ident() -> Ident = c:code_ident() d:(_ d:db_ident() {d})? {
//     if let Some(d) = d {
//         Ident {
//             schema_name: c.clone(),
//             db: d,
//         }
//     } else {
//         Ident {
//             schema_name: c.clone(),
//             db: DbIdent(c.0),
//         }
//     }
// };
// rule index() -> Index
// = "@index" _ unq:("." _ "unique" _)? name:name()? _ f:(_ i:index_fields() {i})? {Index{name, unique:unq.is_some(), fields:f.unwrap_or_default()}}
//
// rule index_fields() -> Vec<Ident> = "(" _ i:code_ident()**_ _ ")" {i}
//
// rule code_ident() -> Ident = n: $(['a'..='z' | 'A'..='Z' | '_' | '0'..='9']+) {Ident(n.to_owned())};
// rule str() -> &'input str = "\"" v:$((!['"' | '\''] [_])*) "\"" {v}
// rule name() -> String = v:str() {v.to_owned()}
//
// rule sqlexpr() -> Sql = "(" s:sql() ")" {s};
// rule sql() -> Sql
// = precedence! {
//     a:(@) _ "||" _ b:@ {Sql::BinOp(h(a), SqlOp::Or, h(b))}
//     --
//     a:(@) _ "&&" _ b:@ {Sql::BinOp(h(a), SqlOp::And, h(b))}
//     --
//     a:(@) _ "==" _ b:@ {Sql::BinOp(h(a), SqlOp::Eq, h(b))}
//     a:(@) _ "!=" _ b:@ {Sql::BinOp(h(a), SqlOp::Ne, h(b))}
//     --
//     a:(@) _ "<" _ b:@ {Sql::BinOp(h(a), SqlOp::Lt, h(b))}
//     a:(@) _ ">" _ b:@ {Sql::BinOp(h(a), SqlOp::Gt, h(b))}
//     a:(@) _ "<=" _ b:@ {Sql::BinOp(h(a), SqlOp::Le, h(b))}
//     a:(@) _ ">=" _ b:@ {Sql::BinOp(h(a), SqlOp::Ge, h(b))}
//     --
//     a:(@) _ "+" _ b:@ {Sql::BinOp(h(a), SqlOp::Plus, h(b))}
//     a:(@) _ "-" _ b:@ {Sql::BinOp(h(a), SqlOp::Minus, h(b))}
//     --
//     a:(@) _ "*" _ b:@ {Sql::BinOp(h(a), SqlOp::Mul, h(b))}
//     a:(@) _ "/" _ b:@ {Sql::BinOp(h(a), SqlOp::Div, h(b))}
//     a:(@) _ "%" _ b:@ {Sql::BinOp(h(a), SqlOp::Mod, h(b))}
//     --
//     "-" _ b:@ {Sql::UnOp(SqlUnOp::Minus, h(b))}
//     "+" _ b:@ {Sql::UnOp(SqlUnOp::Plus, h(b))}
//     "!" _ b:@ {Sql::UnOp(SqlUnOp::Not, h(b))}
//     --
//     a:(@) _ "::" _ ty:code_ident() {Sql::Cast(h(a), ty)}
//     --
//     e:sql_basic() {e}
//     "(" _ e:sql() _ ")" {Sql::Parened(h(e))}
// }
// rule sql_basic() -> Sql
// = "TODO"{todo!()}
//
// rule _() = [' ' | '\t' | '\n']*;
// }
// }

fn root(v: &str) -> Schema {
    // let mut s = schema_parser::root(v).unwrap();
    // s.process();
    // s
    todo!()
}

fn main() {
    let v = root(include_str!("test.schema"));
    let mut out = String::new();
    v.create(&mut out);
    println!("{out}");
}

// fn diff(a: &Schema, b: &Schema) -> String {
//     let mut out = String::new();
//
//     for old_ver in a.tables() {
//         if let Some(new_ver) = b.table(&old_ver.name.db) {
//             let mut alternations = Vec::new();
//             {
//                 let mut renames = Vec::new();
//                 for old_idx in old_ver.indexes.iter() {
//                     if let Some(new_idx) = new_ver.indexes.iter().find(|ni| ni.isomorphic(&old_idx))
//                     {
//                         if new_idx.name(&old_ver.name.db) != old_idx.name(&old_ver.name.db) {
//                             renames.push(format!(
//                                 "RENAME CONSTRAINT {} TO {}",
//                                 old_idx.name(&old_ver),
//                                 new_idx.name(&new_ver)
//                             ));
//                         }
//                     } else {
//                         alternations.push(format!("DROP CONSTRAINT {}", old_idx.name(&old_ver),));
//                     }
//                 }
//                 alternations.extend(renames.into_iter());
//             }
//             for old_field in &old_ver.fields {
//                 if let Some(new_field) = new_ver.field(&old_field.name.db) {
//                     let new_ty = b.native_type(&new_field.ty);
//                     if a.native_type(&old_field.ty) != new_ty {
//                         alternations.push(format!(
//                             "ALTER COLUMN {} TYPE {new_ty}",
//                             old_field.name.db_name()
//                         ))
//                     }
//                     if old_field.nullable != new_field.nullable {
//                         let name = old_field.name.db_name();
//                         if new_field.nullable {
//                             alternations.push(format!("ALTER COLUMN {name} DROP NOT NULL"))
//                         } else {
//                             alternations.push(format!("ALTER COLUMN {name} SET NOT NULL"))
//                         }
//                     }
//                 } else {
//                     alternations.push(format!("DROP COLUMN {}", old_field.name.db_name()));
//                 }
//             }
//             for new_field in &new_ver.fields {
//                 if old_ver.field(&new_field.name.db).is_some() {
//                     continue;
//                 }
//                 alternations.push(format!(
//                     "ADD COLUMN {} {}",
//                     new_field.name.db_name(),
//                     b.native_type(&new_field.ty)
//                 ));
//             }
//             {
//                 for new_idx in new_ver.indexes.iter() {
//                     if !old_ver.indexes.iter().any(|ni| ni.isomorphic(&new_idx)) {
//                         new_idx.create(&mut out, &new_ver);
//                     }
//                 }
//             }
//             if !alternations.is_empty() {
//                 writeln!(
//                     out,
//                     "ALTER TABLE {}\n\t{};",
//                     old_ver.name.db_name(),
//                     alternations.join("\n,\t")
//                 )
//                 .unwrap();
//             }
//         }
//     }
//
//     for old_ver in a.tables() {
//         if b.table(&old_ver.name.db).is_none() {
//             writeln!(out, "DROP TABLE {};\n", old_ver.name.db_name()).unwrap();
//         }
//     }
//
//     // Create before altering, as new fields may
//     for new_ver in b.tables() {
//         if a.table(&new_ver.name.db).is_none() {
//             new_ver.create(&mut out, &b);
//         }
//     }
//
//     out
// }

#[cfg(test)]
mod tests {
    fn showdiff(a: &Schema, b: &Schema) {
        println!();
        println!("// up.sql");
        print!("{}", diff(a, b));
        println!("// down.sql");
        print!("{}", diff(b, a));
    }
    use super::*;
    #[test]
    fn create_table() {
        let from = root(
            r#"
                        scalar id "INTEGER"
                        table Test {id;}
        "#,
        );
        let to = root("");
        showdiff(&from, &to);
    }
    #[test]
    fn correct_index() {
        let to = root(
            r#"
                        scalar id "INTEGER"
                        table Test {
                            a `acorrect`: id @primary_key;
                            b `bcorrect`: id @index;
                        }
        "#,
        );
        let from = root("");
        showdiff(&from, &to);
    }

    #[test]
    fn rename_index() {
        let from = root(r#"scalar id "INTEGER" table Test { id @index "test1"; }"#);
        let to = root(r#"scalar id "INTEGER" table Test { id @index; }"#);
        showdiff(&from, &to);
    }
    #[test]
    fn change_type() {
        let from = root(r#"scalar id "VARCHAR(10)" table Test { id; }"#);
        let to = root(r#"scalar id "VARCHAR(20)" table Test { id; id2: id; }"#);
        showdiff(&from, &to);
    }

    #[test]
    fn nullable() {
        let from = root(r#"scalar id "INTEGER" table Test { id?; }"#);
        let to = root(r#"scalar id "INTEGER" table Test { id; }"#);
        showdiff(&from, &to);
    }
}
