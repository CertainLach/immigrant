use peg::parser;

use crate::ids::DbIdent;
use crate::names::{DefName, DbProcedure};
use crate::schema::index::{Constraint, ConstraintKind};
use crate::{
    ids::in_allocator,
    names::{ColumnIdent, DbType, EnumItemDefName, TableDefName, TypeDefName, TypeIdent},
    schema::{
        column::{Column, FieldAnnotation},
        index::Index,
        root::{Item, Schema},
        scalar::{Enum, Scalar},
        sql::{Sql, SqlOp, SqlUnOp},
        table::Table,
    },
};

fn h<T>(v: T) -> Box<T> {
    Box::new(v)
}
parser! {
grammar schema_parser() for str {
pub(super) rule root() -> Schema = _ t:item()**_ _ {Schema(t)}

rule item() -> Item
= t:table() {Item::Table(t)}
/ t:enum() {Item::Enum(t)}
/ t:scalar() {Item::Scalar(t)}

rule table() -> Table = "table" _ name:def_name() _ "{" _ indexes:index()**_ _ fields:table_field()++_ _ "}" {{
    Table {
        name: TableDefName::alloc(name),
        columns: fields,
    }
}};
rule enum() -> Enum = "enum" _ name:def_name() _ "{" _ items:def_name()++(_ ";" _) _ ";"? _ "}" {
    Enum {
        name: TypeDefName::alloc(name),
        items: items.into_iter().map(EnumItemDefName::alloc).collect(),
    }
};
rule scalar() -> Scalar = "scalar" _ name:code_ident() _ native:str() _ annotations:field_annotation()**_ ";"{
    Scalar {
        name: TypeIdent::alloc(name),
        native: DbType::new(native)
    }
}
rule table_field() -> Column = name:def_name() t:(_ ":" _ i:code_ident() {i})? n:(_ "?")? _ annotations:field_annotation()**_ ";" {
    Column {
        nullable: n.is_some(),
        ty: TypeIdent::alloc(t.unwrap_or_else(|| {
            name.0.clone()
        })),
        name: DefName::alloc(name),
        annotations,
    }
}

rule field_annotation() -> FieldAnnotation
= i:index() {FieldAnnotation::Index(i)}
/ "@primary_key" {FieldAnnotation::Constraint(Constraint {kind: ConstraintKind::PrimaryKey(vec![]), name: None})}
/ "@default(" s:sql() ")" {FieldAnnotation::Default(s)}

rule def_name() -> (&'input str, Option<&'input str>)
= c:code_ident() d:(_ d:db_ident() {d})? {
    (c, d)
}

rule index() -> Index
= "@index" _ unq:("." _ "unique" _)? name:name()? _ f:(_ i:index_fields() {i})? {Index{name, unique:unq.is_some(), fields:f.unwrap_or_default()}}

rule index_fields() -> Vec<ColumnIdent> = "(" _ i:code_ident()**_ _ ")" {i.into_iter().map(ColumnIdent::alloc).collect()}

rule code_ident() -> &'input str = n: $(['a'..='z' | 'A'..='Z' | '_' | '0'..='9']+) {n};
rule db_ident() -> &'input str = "\"" v:$((!['"' | '\''] [_])*) "\"" {v};
rule str() -> &'input str = "\"" v:$((!['"' | '\''] [_])*) "\"" {v}
rule name() -> String = v:str() {v.to_owned()}

rule sqlexpr() -> Sql = "(" s:sql() ")" {s};
rule sql() -> Sql
= precedence! {
    a:(@) _ "||" _ b:@ {Sql::BinOp(h(a), SqlOp::Or, h(b))}
    --
    a:(@) _ "&&" _ b:@ {Sql::BinOp(h(a), SqlOp::And, h(b))}
    --
    a:(@) _ "==" _ b:@ {Sql::BinOp(h(a), SqlOp::Eq, h(b))}
    a:(@) _ "!=" _ b:@ {Sql::BinOp(h(a), SqlOp::Ne, h(b))}
    --
    a:(@) _ "<" _ b:@ {Sql::BinOp(h(a), SqlOp::Lt, h(b))}
    a:(@) _ ">" _ b:@ {Sql::BinOp(h(a), SqlOp::Gt, h(b))}
    a:(@) _ "<=" _ b:@ {Sql::BinOp(h(a), SqlOp::Le, h(b))}
    a:(@) _ ">=" _ b:@ {Sql::BinOp(h(a), SqlOp::Ge, h(b))}
    --
    a:(@) _ "+" _ b:@ {Sql::BinOp(h(a), SqlOp::Plus, h(b))}
    a:(@) _ "-" _ b:@ {Sql::BinOp(h(a), SqlOp::Minus, h(b))}
    --
    a:(@) _ "*" _ b:@ {Sql::BinOp(h(a), SqlOp::Mul, h(b))}
    a:(@) _ "/" _ b:@ {Sql::BinOp(h(a), SqlOp::Div, h(b))}
    a:(@) _ "%" _ b:@ {Sql::BinOp(h(a), SqlOp::Mod, h(b))}
    --
    "-" _ b:@ {Sql::UnOp(SqlUnOp::Minus, h(b))}
    "+" _ b:@ {Sql::UnOp(SqlUnOp::Plus, h(b))}
    "!" _ b:@ {Sql::UnOp(SqlUnOp::Not, h(b))}
    --
    a:(@) _ "::" _ ty:code_ident() {Sql::Cast(h(a), TypeIdent::alloc(ty))}
    --
    e:sql_basic() {e}
    "(" _ e:sql() _ ")" {Sql::Parened(h(e))}
}
rule sql_basic() -> Sql
= i:code_ident() _ "(" _ e:sql()**(_ "," _) _ ","? _ ")" {Sql::Call(DbProcedure::new(i), e)}

rule _() = [' ' | '\t' | '\n']*;
}
}

pub fn parse(v: &str) -> Schema {
    let mut s = in_allocator(|| schema_parser::root(v).unwrap());
    s.process();
    s
}
