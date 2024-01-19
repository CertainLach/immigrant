use std::result;

use peg::{error::ParseError, parser, str::LineCol};

use crate::{
	attribute::{Attribute, AttributeField, AttributeList, AttributeValue},
	column::{Column, ColumnAnnotation, PartialForeignKey},
	composite::{Composite, CompositeAnnotation, Field, FieldAnnotation},
	ids::{in_allocator, DbIdent},
	index::{Check, Index, PrimaryKey, UniqueConstraint},
	names::{
		ColumnIdent, DbProcedure, DefName, EnumItemDefName, FieldIdent, TableDefName, TableIdent,
		TypeDefName, TypeIdent, ViewDefName,
	},
	root::{Item, Schema, SchemaProcessOptions},
	scalar::{Enum, EnumItem, Scalar, ScalarAnnotation},
	span::{register_source, SimpleSpan, SourceId},
	sql::{Sql, SqlOp, SqlUnOp},
	table::{ForeignKey, OnDelete, Table, TableAnnotation},
	uid::RenameMap,
	view::{Definition, DefinitionPart, View},
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
/ t:view(s) {Item::View(t)}
/ t:enum(s) {Item::Enum(t)}
/ t:scalar(s) {Item::Scalar(t)}
/ t:composite(s) {Item::Composite(t)}

rule table(s:S) -> Table =
	docs:docs()
	attrlist:attribute_list(s) _
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
rule definition(s:S) -> Definition = "$$"
	parts:(
		raw:$((!"$$" !"${" [_])+)
			{DefinitionPart::Raw(raw.to_string())}
	/	i:("${" _ i:code_ident(s) _ "}" {i})
			{DefinitionPart::TableRef(TableIdent::alloc(i))}
	/	i:("${" _ i:code_ident(s) _ "." _ j:code_ident(s) _ "}" {(i, j)})
			{DefinitionPart::ColumnRef(TableIdent::alloc(i.0), ColumnIdent::alloc(i.1))}
	)+
"$$" {{
	Definition(parts)
}};
rule view(s:S) -> View =
	docs:docs()
	attrlist:attribute_list(s) _
	"view" _ name:def_name(s) _ "=" _ definition:definition(s) _ ";" {{
	View::new(
		docs,
		attrlist,
		ViewDefName::alloc(name),
		definition,
	)
}};
rule enum(s:S) -> Enum = attrlist:attribute_list(s) _ "enum" _ name:def_name(s) _ "{" _ items:def_name(s)++(_ ";" _) _ ";"? _ "}" _ ";" {
	Enum::new(attrlist, TypeDefName::alloc(name), items.into_iter().map(EnumItemDefName::alloc).map(EnumItem::new).collect())
};
rule scalar(s:S) -> Scalar = attrlist:attribute_list(s) _ "scalar" _ name:def_name(s) _ "=" _ native:str() _ annotations:scalar_annotation(s)**_ ";" {
	Scalar::new(attrlist, TypeDefName::alloc(name), DbIdent::new(native), annotations)
}
rule composite(s:S) -> Composite = attrlist:attribute_list(s) _ "struct" _ name:def_name(s) _ "{" _
		fields:(f:field(s) _ ";" {f})++_ _
		annotations:(a:composite_annotation(s) _ ";" {a})**_ _
	"}" _ ";" {
	Composite::new(attrlist, TypeDefName::alloc(name), fields, annotations)
}
rule field(s:S) -> Field =
	name:def_name(s) _
	t:(":" _ i:code_ident(s) _ {i})?
	n:("?" _)?
	annotations:field_annotation(s)**_ {
	Field::new(
		DefName::alloc(name),
		n.is_some(),
		TypeIdent::alloc(t.unwrap_or((name.0, name.1))),
		annotations
	)
}

rule table_field(s:S) -> Column =
	docs:docs()
	attrlist:attribute_list(s) _
	name:def_name(s) _
	t:(":" _ i:code_ident(s) _ {i})?
	n:("?" _)?
	annotations:column_annotation(s)**_
	foreign_key:partial_foreign_key(s)? {
	Column::new(
		DefName::alloc(name),
		docs,
		attrlist,
		n.is_some(),
		TypeIdent::alloc(t.unwrap_or((name.0, name.1))),
		annotations,
		foreign_key
	)
}

rule attribute_list(s:S) -> AttributeList
= list:attribute(s) ** _ {AttributeList(list)}
rule attribute(s:S) -> Attribute
= "#" _ name:code_ident(s) fields:(_ "(" _ f:(f:attribute_field(s)++comma() trailing_comma() {f}) _ ")" {f})? {
	Attribute {
		name: name.1.to_owned(),
		fields: fields.unwrap_or_default(),
	}
}
rule attribute_field(s:S) -> AttributeField
= key:code_ident(s) v:(_ "=" _ value:attribute_value() {value})? {AttributeField {key: key.1.to_owned(), value: v.unwrap_or(AttributeValue::Set)}}
rule attribute_value() -> AttributeValue
= s:str() {AttributeValue::String(s.to_owned())}

rule scalar_annotation(s:S) -> ScalarAnnotation
= c:check(s) {ScalarAnnotation::Check(c)}
/ u:unique(s) {ScalarAnnotation::Unique(u)}
/ pk:primary_key(s) {ScalarAnnotation::PrimaryKey(pk)}
/ d:default(s) {ScalarAnnotation::Default(d)}
/ i:index(s) {ScalarAnnotation::Index(i)}
/ "@inline" {ScalarAnnotation::Inline}
/ "@external" {ScalarAnnotation::External}
rule column_annotation(s:S) -> ColumnAnnotation
= i:index(s) {ColumnAnnotation::Index(i)}
/ c:check(s) {ColumnAnnotation::Check(c)}
/ u:unique(s) {ColumnAnnotation::Unique(u)}
/ pk:primary_key(s) {ColumnAnnotation::PrimaryKey(pk)}
/ d:default(s) {ColumnAnnotation::Default(d)}
/ d:initialize_as(s) {ColumnAnnotation::InitializeAs(d)}
rule table_annotation(s:S) -> TableAnnotation
= c:check(s) {TableAnnotation::Check(c)}
/ u:unique(s) {TableAnnotation::Unique(u)}
/ pk:primary_key(s) {TableAnnotation::PrimaryKey(pk)}
/ i:index(s) {TableAnnotation::Index(i)}
/ "@external" {TableAnnotation::External}
rule composite_annotation(s:S) -> CompositeAnnotation
= c:check(s) {CompositeAnnotation::Check(c)}
rule field_annotation(s:S) -> FieldAnnotation
= c:check(s) {FieldAnnotation::Check(c)}

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
		fk: ForeignKey::new(
			name.map(DbIdent::new),
			None,
			TableIdent::alloc(target),
			target_fields,
			on_delete.unwrap_or(OnDelete::Noop),
		)
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
rule initialize_as(s:S) -> Sql
= "@initialize_as" _ "(" _ s:sql(s) _ ")" {s}
rule primary_key(s:S) -> PrimaryKey
= "@primary_key" _ name:db_ident()? _ columns:index_fields(s)? {PrimaryKey::new(name.map(DbIdent::new), columns.unwrap_or_default())}
rule unique(s:S) -> UniqueConstraint
= "@unique" _ name:db_ident()? _ columns:index_fields(s)? {UniqueConstraint::new(name.map(DbIdent::new), columns.unwrap_or_default())}
rule check(s:S) -> Check
= "@check" _ name:db_ident()? _ "(" _ check:sql(s) _ ")" {Check::new(name.map(DbIdent::new), check)}
rule index(s:S) -> Index
= "@index" _ unq:("." _ "unique" _)? name:db_ident()? _ f:index_fields(s)? {Index::new(name.map(DbIdent::new), unq.is_some(), f.unwrap_or_default())}

rule index_fields(s:S) -> Vec<ColumnIdent> = "(" _ i:code_ident(s)**comma() trailing_comma() ")" {i.into_iter().map(ColumnIdent::alloc).collect()}

rule code_ident(s:S) -> (SimpleSpan, &'input str) = b:position!() n:$(['a'..='z' | 'A'..='Z'] ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*) e:position!() {(SimpleSpan::new(s, b as u32, e as u32), n)};
rule db_ident() -> &'input str = str()
rule str() -> &'input str = "\"" v:$((!['"' | '\''] [_])*) "\"" {v}

rule sqlexpr(s:S) -> Sql = "(" s:sql(s) ")" {s};
rule sql(s:S) -> Sql
= precedence! {
	a:(@) _ "||" _ b:@ {Sql::BinOp(h(a), SqlOp::Or, h(b))}
	--
	a:(@) _ "&&" _ b:@ {Sql::BinOp(h(a), SqlOp::And, h(b))}
	--
	a:(@) _ "===" _ b:@ {Sql::BinOp(h(a), SqlOp::SEq, h(b))}
	a:(@) _ "!==" _ b:@ {Sql::BinOp(h(a), SqlOp::SNe, h(b))}
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
	a:(@) _ "." _ f:code_ident(s) {Sql::GetField(h(a), FieldIdent::alloc(f))}
	--
	e:sql_basic(s) {e}
}
rule sql_basic(s:S) -> Sql
= "(" _ e:(e:sql(s) _ "," _ {e})*<2,> _ ")" {Sql::Tuple(e)}
/ "(" _ e:sql(s) _ "," _ ")" {Sql::Tuple(vec![e])}
/ "(" _ e:sql(s) _ ")" {Sql::Parened(h(e))}
/ "(" _ ")" {Sql::Tuple(vec![])}
/ "_" {Sql::Placeholder}
/ "if" _ c:sql(s) _ "then" _ then:sql(s) _ "else" _ else_:sql(s) {Sql::If(h(c), h(then), h(else_))}
/ "null" {Sql::Null}
/ "true" {Sql::Boolean(true)}
/ "false" {Sql::Boolean(false)}
/ i:code_ident(s) _ "(" _ e:sql(s)**comma() trailing_comma() ")" {Sql::Call(DbProcedure::new(i.1), e)}
/ i:code_ident(s) {Sql::Ident(ColumnIdent::alloc(i))}
/ s:str() {Sql::String(s.to_owned())}
/ n:$(['0'..='9']+) {Sql::Number(n.parse().unwrap())}

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
pub fn parse(v: &str, opts: &SchemaProcessOptions, rn: &mut RenameMap) -> Result<Schema> {
	let span = register_source(v.to_string());
	let mut s =
		in_allocator(|| schema_parser::root(v, span).map_err(|e| vec![ParsingError::from(e)]))?;
	s.process(opts, rn);
	Ok(s)
}
