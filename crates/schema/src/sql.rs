use crate::{
	ids::{DbIdent, Ident},
	names::{ColumnIdent, DbProcedure, FieldIdent, FieldKind, TypeIdent, TypeKind},
	uid::{RenameExt, RenameMap},
	HasIdent, SchemaItem, SchemaType,
};

#[derive(Debug, Clone)]
pub enum SqlUnOp {
	Plus,
	Minus,
	Not,
}
impl SqlUnOp {
	pub fn format(&self) -> &'static str {
		match self {
			SqlUnOp::Plus => "+",
			SqlUnOp::Minus => "-",
			SqlUnOp::Not => "NOT ",
		}
	}
}
#[derive(Debug, Clone)]
pub enum SqlOp {
	Lt,
	Gt,
	Le,
	Ge,
	Eq,
	Ne,
	SEq,
	SNe,

	And,
	Or,

	Plus,
	Minus,
	Div,
	Mul,
	Mod,

	Like,
}
impl SqlOp {
	pub fn format(&self) -> &'static str {
		match self {
			SqlOp::Lt => "<",
			SqlOp::Gt => ">",
			SqlOp::Le => "<=",
			SqlOp::Ge => ">=",
			SqlOp::Eq => "=",
			SqlOp::Ne => "<>",
			SqlOp::SEq => "IS NOT DISTINCT FROM",
			SqlOp::SNe => "IS DISTINCT FROM",
			SqlOp::And => "AND",
			SqlOp::Or => "OR",
			SqlOp::Plus => "+",
			SqlOp::Minus => "-",
			SqlOp::Div => "/",
			SqlOp::Mul => "*",
			SqlOp::Mod => "%",
			SqlOp::Like => "LIKE",
		}
	}
}
#[derive(Debug, Clone)]
pub enum Sql {
	Cast(Box<Sql>, TypeIdent),
	Call(DbProcedure, Vec<Sql>),
	String(String),
	Number(i128),
	Ident(ColumnIdent),
	UnOp(SqlUnOp, Box<Sql>),
	BinOp(Box<Sql>, SqlOp, Box<Sql>),
	GetField(Box<Sql>, FieldIdent),
	Parened(Box<Sql>),
	Tuple(Vec<Sql>),
	If(Box<Sql>, Box<Sql>, Box<Sql>),
	Boolean(bool),
	Placeholder,
	Null,
}
impl Sql {
	pub fn replace_placeholder(&mut self, column: ColumnIdent) {
		struct ReplacePlaceholder {
			to: ColumnIdent,
		}
		impl SqlVisitor for ReplacePlaceholder {
			fn handle_placeholder(&mut self, placeholder: &mut Sql) {
				*placeholder = Sql::Ident(self.to)
			}
		}
		self.visit(&mut ReplacePlaceholder { to: column })
	}
	pub fn affected_columns(&self) -> Vec<ColumnIdent> {
		struct ColumnCollector {
			columns: Vec<ColumnIdent>,
		}
		impl SqlVisitor for ColumnCollector {
			fn handle_column(&mut self, column: &mut ColumnIdent) {
				if self.columns.contains(column) {
					return;
				}
				self.columns.push(*column)
			}
			fn handle_placeholder(&mut self, _placeholder: &mut Sql) {
				panic!("placeholder was not normalized")
			}
		}
		let mut collector = ColumnCollector { columns: vec![] };
		let mut copy = self.clone();
		copy.visit(&mut collector);
		collector.columns
	}
	fn visit(&mut self, v: &mut impl SqlVisitor) {
		v.handle(self);
		match self {
			Sql::Cast(s, t) => {
				s.visit(v);
				v.handle_type(t);
			}
			Sql::Call(_p, args) => {
				for arg in args {
					arg.visit(v);
				}
			}
			Sql::String(_s) => {}
			Sql::Number(_n) => {}
			Sql::Ident(i) => v.handle_column(i),
			Sql::UnOp(_o, s) => s.visit(v),
			Sql::BinOp(a, _o, b) => {
				a.visit(v);
				b.visit(v);
			}
			Sql::Parened(s) => s.visit(v),
			Sql::Placeholder => v.handle_placeholder(self),
			Sql::Boolean(_) => {}
			Sql::GetField(s, _) => s.visit(v),
			Sql::Null => {}
			Sql::Tuple(els) => {
				for ele in els.iter_mut() {
					ele.visit(v)
				}
			}
			Sql::If(a, b, c) => {
				a.visit(v);
				b.visit(v);
				c.visit(v);
			}
		}
	}
	pub fn all(s: impl IntoIterator<Item = Self>) -> Self {
		let mut s = s.into_iter();
		let mut v = s.next().unwrap_or(Self::Boolean(true));
		for i in s {
			v = Sql::BinOp(Box::new(v), SqlOp::And, Box::new(i))
		}
		v
	}
	pub fn field_name(
		&self,
		context: &SchemaItem<'_>,
		field: Ident<FieldKind>,
		rn: &RenameMap,
	) -> DbIdent<FieldKind> {
		let this = self.type_ident_of_expr(context);
		let ty = context.schema().schema_ty(this);
		match ty {
			SchemaType::Enum(_) => {
				panic!("nothing in enum... or should the enum variant be returned here?")
			}
			SchemaType::Scalar(_) => panic!("nothing in scalar: {ty:?}"),
			SchemaType::Composite(c) => c.field(field).db(rn),
		}
	}
	/// If self == Sql::Ident(name), convert name to native
	pub fn ident_name<'s>(
		&self,
		context: &'s SchemaItem<'s>,
		rn: &RenameMap,
	) -> DbIdent<FieldKind> {
		let Sql::Ident(f) = self else {
			panic!("not ident");
		};
		match context {
			SchemaItem::Table(t) => {
				let column = t.schema_column(*f);
				DbIdent::unchecked_from(column.db(rn))
			}
			SchemaItem::Enum(_) => panic!("nothing in enum"),
			SchemaItem::Scalar(_) => panic!("nothing in scalar: {context:?}"),
			SchemaItem::Composite(c) => {
				let field = c.field(Ident::unchecked_cast(*f));
				field.db(rn)
			}
		}
	}
	fn type_ident_of_expr<'s>(&self, context: &'s SchemaItem<'s>) -> Ident<TypeKind> {
		match self {
			Sql::Cast(_, t) => *t,
			Sql::UnOp(_, _)
			| Sql::BinOp(_, _, _)
			| Sql::Call(_, _)
			| Sql::String(_)
			| Sql::Number(_)
			| Sql::Boolean(_)
			| Sql::Null
			| Sql::Tuple(_)
			| Sql::If(_, _, _) => {
				panic!("cannot determine call return type")
			}
			Sql::Ident(f) => match context {
				SchemaItem::Table(t) => {
					let column = t.schema_column(*f);
					column.ty
				}
				SchemaItem::Enum(_) => panic!("nothing in enum"),
				SchemaItem::Scalar(_) => panic!("nothing in scalar: {self:?}"),
				SchemaItem::Composite(c) => {
					let field = c.field(Ident::unchecked_cast(*f));
					field.ty
				}
			},
			Sql::GetField(f, t) => {
				let ty_id = f.type_ident_of_expr(context);
				let ty = context.schema().schema_ty(ty_id);
				match ty {
					SchemaType::Enum(_) => panic!("nothing in enum"),
					SchemaType::Scalar(_) => panic!("nothing in scalar: {ty:?}"),
					SchemaType::Composite(c) => {
						let field = c.field(*t);
						field.ty
					}
				}
			}
			Sql::Parened(v) => v.type_ident_of_expr(context),
			Sql::Placeholder => match context {
				SchemaItem::Table(_) => panic!("can't refer to table fields using this notation"),
				SchemaItem::Enum(e) => e.id(),
				SchemaItem::Scalar(s) => s.id(),
				SchemaItem::Composite(c) => c.id(),
			},
		}
	}
}
#[allow(unused_variables)]
pub trait SqlVisitor {
	fn handle(&mut self, sql: &mut Sql) {}
	fn handle_type(&mut self, ty: &mut TypeIdent) {}
	fn handle_column(&mut self, column: &mut ColumnIdent) {}
	fn handle_placeholder(&mut self, placeholder: &mut Sql) {}
}
