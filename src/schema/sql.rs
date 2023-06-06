use crate::names::{ColumnIdent, DbProcedure, TypeIdent};
use crate::{w, TableSql};

#[derive(Debug, Clone)]
pub enum SqlUnOp {
	Plus,
	Minus,
	Not,
}
impl SqlUnOp {
	fn format(&self) -> &'static str {
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

	And,
	Or,

	Plus,
	Minus,
	Div,
	Mul,
	Mod,
}
impl SqlOp {
	fn format(&self) -> &'static str {
		match self {
			SqlOp::Lt => "<",
			SqlOp::Gt => ">",
			SqlOp::Le => "<=",
			SqlOp::Ge => ">=",
			SqlOp::Eq => "=",
			SqlOp::Ne => "<>",
			SqlOp::And => "AND",
			SqlOp::Or => "OR",
			SqlOp::Plus => "+",
			SqlOp::Minus => "-",
			SqlOp::Div => "/",
			SqlOp::Mul => "*",
			SqlOp::Mod => "%",
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
	Parened(Box<Sql>),
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
			Sql::Null => {}
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

impl TableSql<'_> {
	pub fn print(&self, out: &mut String) {
		match &**self {
			Sql::Cast(expr, ty) => {
				let expr = self.table.format_sql(expr);
				let native_ty = self.table.schema.native_type(ty);
				w!(out, "({expr})::{native_ty}");
			}
			Sql::Call(proc, args) => {
				w!(out, "{proc}(");
				for (i, arg) in args.iter().enumerate() {
					if i != 0 {
						w!(out, ", ");
					}
					let arg = self.table.format_sql(arg);
					w!(out, "{arg}");
				}
				w!(out, ")");
			}
			Sql::String(s) => {
				w!(out, "'{s}'");
			}
			Sql::Number(n) => {
				w!(out, "{n}");
			}
			Sql::Ident(c) => {
				let native_name = self.table.db_name(c);
				w!(out, "{native_name}");
			}
			Sql::UnOp(op, expr) => {
				let op = op.format();
				let expr = self.table.format_sql(expr);
				w!(out, "{op}({expr})");
			}
			Sql::BinOp(a, op, b) => {
				let op = op.format();
				let a = self.table.format_sql(a);
				let b = self.table.format_sql(b);
				w!(out, "({a}) {op} ({b})")
			}
			Sql::Parened(_) => todo!(),
			Sql::Placeholder => todo!(),
			Sql::Null => todo!(),
		}
	}
}
