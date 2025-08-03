use crate::variable::Variable;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(Variable),
    Number(i64),
    StringLiteral(String),
    Boolean(bool),
    BinaryOp {
        op: String,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOp {
        op: String,
        expr: Box<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Expression::Variable(var) => format!("Variable::<{}>({})", var.var_type, var.name),
            Expression::Number(value) => format!("Number({})", value),
            Expression::StringLiteral(value) => format!("StringLiteral({})", value),
            Expression::Boolean(value) => format!("Boolean({})", value),
            Expression::BinaryOp { op, left, right } => {
                format!("BinaryOp({} {} {})", left, op, right)
            }
            Expression::UnaryOp { op, expr } => format!("UnaryOp({} {})", op, expr),
        };
        write!(f, "{}", fmtstr)
    }
}
