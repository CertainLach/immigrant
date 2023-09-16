use std::result;

use peg::{error::ParseError, parser, str::LineCol};

use crate::{
	attribute::{Attribute, AttributeField, AttributeList, AttributeValue},
	column::{Column, ColumnAnnotation, PartialForeignKey},
	ids::{in_allocator, DbIdent},
	index::{Check, Index, PrimaryKey, UniqueConstraint},
	names::{
		ColumnIdent, DbProcedure, DefName, EnumItemDefName, TableDefName, TableIdent, TypeDefName,
		TypeIdent, UpdateableDbName,
	},
	root::{Item, Schema, SchemaProcessOptions},
	scalar::{Enum, EnumItem, Scalar, ScalarAnnotation},
	sql::{Sql, SqlOp, SqlUnOp},
	table::{ForeignKey, OnDelete, Table, TableAnnotation},
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

rule table() -> Table =
	docs:docs()
	attrlist:attribute_list() _
	"table" _ name:def_name() _ "{" _
		fields:(f:table_field() _ ";" {f})++_ _
		annotations:(a:table_annotation() _ ";" {a})**_ _
		foreign_keys:(f:foreign_key() _ ";" {f})**_ _
	"}" _ ";" {{
	Table::new(
		docs,
		attrlist,
		TableDefName::alloc(name),
		fields,
		annotations,
		foreign_keys,
	)
}};
rule enum() -> Enum = attrlist:attribute_list() _ "enum" _ name:def_name() _ "{" _ items:def_name()++(_ ";" _) _ ";"? _ "}" _ ";" {
	Enum::new(attrlist, TypeDefName::alloc(name), items.into_iter().map(EnumItemDefName::alloc).map(EnumItem::new).collect())
};
rule scalar() -> Scalar = attrlist:attribute_list() _ "scalar" _ name:def_name() _ "=" _ native:str() _ annotations:scalar_annotation()**_ ";" {
	Scalar::new(attrlist, TypeDefName::alloc(name), DbIdent::new(native), annotations)
}
rule table_field() -> Column =
	docs:docs()
	name:def_name() t:(_ ":" _ i:code_ident() {i})? n:(_ "?")? _ annotations:field_annotation()**_ foreign_key:partial_foreign_key()? {
	Column {
		docs,
		nullable: n.is_some(),
		ty: TypeIdent::alloc(t.unwrap_or(name.0)),
		name: DefName::alloc(name),
		annotations,
		foreign_key,
	}
}

rule attribute_list() -> AttributeList
= list:attribute() ** _ {AttributeList(list)}
rule attribute() -> Attribute
= "#" _ name:code_ident() fields:(_ "(" _ f:(f:attribute_field()++comma() trailing_comma() {f}) _ ")" {f})? {
	Attribute {
		name: name.to_owned(),
		fields: fields.unwrap_or_default(),
	}
}
rule attribute_field() -> AttributeField
= key:code_ident() v:(_ "=" _ value:attribute_value() {value})? {AttributeField {key: key.to_owned(), value: v.unwrap_or(AttributeValue::Set)}}
rule attribute_value() -> AttributeValue
= s:str() {AttributeValue::String(s.to_owned())}

rule scalar_annotation() -> ScalarAnnotation
= c:check() {ScalarAnnotation::Check(c)}
/ u:unique() {ScalarAnnotation::Unique(u)}
/ pk:primary_key() {ScalarAnnotation::PrimaryKey(pk)}
/ d:default() {ScalarAnnotation::Default(d)}
/ "@inline" {ScalarAnnotation::Inline}
rule field_annotation() -> ColumnAnnotation
= i:index() {ColumnAnnotation::Index(i)}
/ c:check() {ColumnAnnotation::Check(c)}
/ u:unique() {ColumnAnnotation::Unique(u)}
/ pk:primary_key() {ColumnAnnotation::PrimaryKey(pk)}
/ d:default() {ColumnAnnotation::Default(d)}
rule table_annotation() -> TableAnnotation
= c:check() {TableAnnotation::Check(c)}
/ u:unique() {TableAnnotation::Unique(u)}
/ pk:primary_key() {TableAnnotation::PrimaryKey(pk)}
/ i:index() {TableAnnotation::Index(i)}

rule def_name() -> (&'input str, Option<&'input str>)
= c:code_ident() d:(_ d:db_ident() {d})? {
	(c, d)
}

rule foreign_key() -> ForeignKey
= source_fields:index_fields()? _ pfk:partial_foreign_key() {
	let mut fk = pfk.fk;
	assert!(fk.source_fields.is_none());
	fk.source_fields = source_fields;
	fk
}

rule partial_foreign_key() -> PartialForeignKey
= name:db_ident()? _ "~" _ on_delete:("." _ a:on_delete() {a})? _ target:code_ident() _ target_fields:index_fields()? {
	PartialForeignKey {
		fk: ForeignKey {
			name: name.map(|v| v.to_owned()),
			source_fields: None,
			on_delete: on_delete.unwrap_or(OnDelete::Noop),
			target: TableIdent::alloc(target),
			target_fields,
		}
	}
}
rule on_delete() -> OnDelete
= "set_null" {OnDelete::SetNull}
/ "set_default" {OnDelete::SetDefault}
/ "restrict" {OnDelete::Restrict}
/ "noop" {OnDelete::Noop}
/ "cascade" {OnDelete::Cascade}

rule default() -> Sql
= "@default" _ "(" _ s:sql() _ ")" {s}
rule primary_key() -> PrimaryKey
= "@primary_key" _ name:db_ident()? _ columns:index_fields()? {PrimaryKey{columns: columns.unwrap_or_default(), name: name.map(|n| UpdateableDbName::new(DbIdent::new(n))).unwrap_or_default()}}
rule unique() -> UniqueConstraint
= "@unique" _ name:db_ident()? _ columns:index_fields()? {UniqueConstraint{columns: columns.unwrap_or_default(), name: name.map(|n| UpdateableDbName::new(DbIdent::new(n))).unwrap_or_default()}}
rule check() -> Check
= "@check" _ name:db_ident()? _ "(" _ check:sql() _ ")" {Check{check, name: name.map(|n| UpdateableDbName::new(DbIdent::new(n))).unwrap_or_default()}}
rule index() -> Index
= "@index" _ unq:("." _ "unique" _)? name:db_ident()? _ f:index_fields()? {Index{name: name.map(|n| UpdateableDbName::new(DbIdent::new(n))).unwrap_or_default(), unique:unq.is_some(), fields:f.unwrap_or_default()}}

rule index_fields() -> Vec<ColumnIdent> = "(" _ i:code_ident()**comma() trailing_comma() ")" {i.into_iter().map(ColumnIdent::alloc).collect()}

rule code_ident() -> &'input str = n: $(['a'..='z' | 'A'..='Z' | '_' | '0'..='9']+) {n};
rule db_ident() -> &'input str = str()
rule str() -> &'input str = "\"" v:$((!['"' | '\''] [_])*) "\"" {v}

rule sqlexpr() -> Sql = "(" s:sql() ")" {s};
rule sql() -> Sql
= precedence! {
	a:(@) _ "||" _ b:@ {Sql::BinOp(h(a), SqlOp::Or, h(b))}
	--
	a:(@) _ "&&" _ b:@ {Sql::BinOp(h(a), SqlOp::And, h(b))}
	--
	a:(@) _ "==" _ b:@ {Sql::BinOp(h(a), SqlOp::Eq, h(b))}
	a:(@) _ "!=" _ b:@ {Sql::BinOp(h(a), SqlOp::Ne, h(b))}
	a:(@) _ "~~" _ b:@ {Sql::BinOp(h(a), SqlOp::Like, h(b))}
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
= i:code_ident() _ "(" _ e:sql()**comma() trailing_comma() ")" {Sql::Call(DbProcedure::new(i), e)}
/ s:str() {Sql::String(s.to_owned())}
/ n:$(['0'..='9']+) {Sql::Number(n.parse().unwrap())}
/ "_" {Sql::Placeholder}

rule trailing_comma() = _ ","? _;
rule comma() = _ "," _;
rule _() = ([' ' | '\t' | '\n'] / ("///" (!['\n'] [_])+ ['\n']))*;
rule docs() -> Vec<String> = l:("//" s:$((!['\n'] [_])+) "\n" _ {s.to_string()})* {l}
}
}

#[derive(thiserror::Error, Debug)]
pub enum ParsingError {
	#[error("parser error: {0}")]
	Peg(#[from] ParseError<LineCol>),
}

type Result<T> = result::Result<T, Vec<ParsingError>>;
pub fn parse(v: &str, opts: &SchemaProcessOptions) -> Result<Schema> {
	let mut s = in_allocator(|| schema_parser::root(v).map_err(|e| vec![ParsingError::from(e)]))?;
	s.process(opts);
	Ok(s)
}
