use std::result;

use peg::{error::ParseError, parser, str::LineCol};

use crate::{
	attribute::{Attribute, AttributeField, AttributeList, AttributeValue},
	column::{Column, ColumnAnnotation, PartialForeignKey},
	ids::{in_allocator, DbIdent},
	index::{Check, Index, PrimaryKey, UniqueConstraint},
	names::{
		ColumnIdent, DbProcedure, DefName, EnumItemDefName, TableDefName, TableIdent, TypeDefName,
		TypeIdent,
	},
	root::{Item, Schema, SchemaProcessOptions},
	scalar::{Enum, EnumItem, Scalar, ScalarAnnotation},
	span::{register_source, SimpleSpan, SourceId},
	sql::{Sql, SqlOp, SqlUnOp},
	table::{ForeignKey, OnDelete, Table, TableAnnotation},
};

fn h<T>(v: T) -> Box<T> {
	Box::new(v)
}

type S = SourceId;
parser! {
grammar schema_parser() for str {
pub(super) rule root(s:S) -> Schema = _ t:item(s)**_ _ {Schema(t)}

rule item(s:S) -> Item
= t:table(s) {Item::Table(t)}
/ t:enum(s) {Item::Enum(t)}
/ t:scalar(s) {Item::Scalar(t)}

rule table(s:S) -> Table =
	docs:docs()
	attrlist:attribute_list() _
	"table" _ name:def_name(s) _ "{" _
		fields:(f:table_field(s) _ ";" {f})++_ _
		annotations:(a:table_annotation(s) _ ";" {a})**_ _
		foreign_keys:(f:foreign_key(s) _ ";" {f})**_ _
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
rule enum(s:S) -> Enum = attrlist:attribute_list() _ "enum" _ name:def_name(s) _ "{" _ items:def_name(s)++(_ ";" _) _ ";"? _ "}" _ ";" {
	Enum::new(attrlist, TypeDefName::alloc(name), items.into_iter().map(EnumItemDefName::alloc).map(EnumItem::new).collect())
};
rule scalar(s:S) -> Scalar = attrlist:attribute_list() _ "scalar" _ name:def_name(s) _ "=" _ native:str() _ annotations:scalar_annotation(s)**_ ";" {
	Scalar::new(attrlist, TypeDefName::alloc(name), DbIdent::new(native), annotations)
}
rule table_field(s:S) -> Column =
	docs:docs()
	name:def_name(s) t:(_ ":" _ i:code_ident(s) {i})? n:(_ "?")? _ annotations:field_annotation(s)**_ foreign_key:partial_foreign_key(s)? {
	Column {
		docs,
		nullable: n.is_some(),
		ty: TypeIdent::alloc(t.unwrap_or((name.0, name.1))),
		name: DefName::alloc(name),
		annotations,
		foreign_key,
	}
}

rule attribute_list() -> AttributeList
= list:attribute() ** _ {AttributeList(list)}
rule attribute() -> Attribute
= "#" _ name:db_ident() fields:(_ "(" _ f:(f:attribute_field()++comma() trailing_comma() {f}) _ ")" {f})? {
	Attribute {
		name: name.to_owned(),
		fields: fields.unwrap_or_default(),
	}
}
rule attribute_field() -> AttributeField
= key:db_ident() v:(_ "=" _ value:attribute_value() {value})? {AttributeField {key: key.to_owned(), value: v.unwrap_or(AttributeValue::Set)}}
rule attribute_value() -> AttributeValue
= s:str() {AttributeValue::String(s.to_owned())}

rule scalar_annotation(s:S) -> ScalarAnnotation
= c:check(s) {ScalarAnnotation::Check(c)}
/ u:unique(s) {ScalarAnnotation::Unique(u)}
/ pk:primary_key(s) {ScalarAnnotation::PrimaryKey(pk)}
/ d:default(s) {ScalarAnnotation::Default(d)}
/ "@inline" {ScalarAnnotation::Inline}
rule field_annotation(s:S) -> ColumnAnnotation
= i:index(s) {ColumnAnnotation::Index(i)}
/ c:check(s) {ColumnAnnotation::Check(c)}
/ u:unique(s) {ColumnAnnotation::Unique(u)}
/ pk:primary_key(s) {ColumnAnnotation::PrimaryKey(pk)}
/ d:default(s) {ColumnAnnotation::Default(d)}
rule table_annotation(s:S) -> TableAnnotation
= c:check(s) {TableAnnotation::Check(c)}
/ u:unique(s) {TableAnnotation::Unique(u)}
/ pk:primary_key(s) {TableAnnotation::PrimaryKey(pk)}
/ i:index(s) {TableAnnotation::Index(i)}

rule def_name(s:S) -> (SimpleSpan, &'input str, Option<&'input str>)
= c:code_ident(s) d:(_ d:db_ident() {d})? {
	(c.0, c.1, d)
}

rule foreign_key(s:S) -> ForeignKey
= source_fields:index_fields(s)? _ pfk:partial_foreign_key(s) {
	let mut fk = pfk.fk;
	assert!(fk.source_fields.is_none());
	fk.source_fields = source_fields;
	fk
}

rule partial_foreign_key(s:S) -> PartialForeignKey
= name:db_ident()? _ "~" _ on_delete:("." _ a:on_delete() {a})? _ target:code_ident(s) _ target_fields:index_fields(s)? {
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

rule default(s:S) -> Sql
= "@default" _ "(" _ s:sql(s) _ ")" {s}
rule primary_key(s:S) -> PrimaryKey
= "@primary_key" _ name:db_ident()? _ columns:index_fields(s)? {PrimaryKey::new(name.map(DbIdent::new).unwrap_or_default(), columns.unwrap_or_default())}
rule unique(s:S) -> UniqueConstraint
= "@unique" _ name:db_ident()? _ columns:index_fields(s)? {UniqueConstraint::new(name.map(DbIdent::new).unwrap_or_default(), columns.unwrap_or_default())}
rule check(s:S) -> Check
= "@check" _ name:db_ident()? _ "(" _ check:sql(s) _ ")" {Check::new(name.map(DbIdent::new).unwrap_or_default(), check)}
rule index(s:S) -> Index
= "@index" _ unq:("." _ "unique" _)? name:db_ident()? _ f:index_fields(s)? {Index::new(name.map(DbIdent::new).unwrap_or_default(), unq.is_some(), f.unwrap_or_default())}

rule index_fields(s:S) -> Vec<ColumnIdent> = "(" _ i:code_ident(s)**comma() trailing_comma() ")" {i.into_iter().map(ColumnIdent::alloc).collect()}

rule code_ident(s:S) -> (SimpleSpan, &'input str) = b:position!() n:$(['a'..='z' | 'A'..='Z' | '_' | '0'..='9']+) e:position!() {(SimpleSpan::new(s, b as u32, e as u32), n)};
rule db_ident() -> &'input str = str()
rule str() -> &'input str = "\"" v:$((!['"' | '\''] [_])*) "\"" {v}

rule sqlexpr(s:S) -> Sql = "(" s:sql(s) ")" {s};
rule sql(s:S) -> Sql
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
	a:(@) _ "::" _ ty:code_ident(s) {Sql::Cast(h(a), TypeIdent::alloc(ty))}
	--
	e:sql_basic(s) {e}
	"(" _ e:sql(s) _ ")" {Sql::Parened(h(e))}
}
rule sql_basic(s:S) -> Sql
= i:db_ident() _ "(" _ e:sql(s)**comma() trailing_comma() ")" {Sql::Call(DbProcedure::new(i), e)}
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
	let span = register_source(v.to_string());
	let mut s =
		in_allocator(|| schema_parser::root(v, span).map_err(|e| vec![ParsingError::from(e)]))?;
	s.process(opts);
	Ok(s)
}
