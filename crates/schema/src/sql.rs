use crate::names::{ColumnIdent, DbProcedure, TypeIdent};

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
