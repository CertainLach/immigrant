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
#[derive(Debug, Clone)]
pub enum Sql {
    Cast(Box<Sql>, TypeIdent),
    Call(DbProcedure, Vec<Sql>),
    String(String),
    Ident(ColumnIdent),
    UnOp(SqlUnOp, Box<Sql>),
    BinOp(Box<Sql>, SqlOp, Box<Sql>),
    Parened(Box<Sql>),
    Placeholder,
    Null,
}
impl Sql {
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
            fn handle_placeholder(&mut self, placeholder: &mut Sql) {
                panic!("placeholder was not normalized")
            }
        }
        let mut collector = ColumnCollector { columns: vec![] };
        let mut copy = self.clone();
        copy.visit(&mut collector);
        collector.columns
    }
    fn visit(&mut self, v: &mut impl SqlVisitor) {
        match self {
            Sql::Cast(s, t) => {
                v.handle(s);
                v.handle_type(t);
            }
            Sql::Call(_p, args) => {
                for arg in args {
                    v.handle(arg)
                }
            }
            Sql::String(_s) => {}
            Sql::Ident(i) => v.handle_column(i),
            Sql::UnOp(_o, s) => v.handle(s),
            Sql::BinOp(a, _o, b) => {
                v.handle(a);
                v.handle(b);
            }
            Sql::Parened(s) => v.handle(s),
            Sql::Placeholder => v.handle_placeholder(self),
            Sql::Null => {}
        }
    }
}
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
                w!(out, "{s:?}");
            },
            Sql::Ident(c) => {
                let native_name = self.table.db_name(&c);
                w!(out, "{native_name}");
            },
            Sql::UnOp(op, expr) => {
                let op = op.format();
                let expr = self.table.format_sql(expr);
                w!(out, "{op}({expr})");
            },
            Sql::BinOp(_, _, _) => todo!(),
            Sql::Parened(_) => todo!(),
            Sql::Placeholder => todo!(),
            Sql::Null => todo!(),
        }
    }
}
