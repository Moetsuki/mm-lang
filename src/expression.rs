use crate::{types::Type, variable::Variable};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(Variable),
    Number(i64),
    StringLiteral(String),
    Boolean(bool),
    Cast {
        expr: Box<Expression>,
        target_type: Type,
    },
    BinaryOp {
        op: String,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOp {
        op: String,
        expr: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Expression::Variable(var) => format!("Variable::<{}>({})", var.var_type, var.name),
            Expression::Number(value) => format!("Number({})", value),
            Expression::StringLiteral(value) => format!("StringLiteral({})", value),
            Expression::Boolean(value) => format!("Boolean({})", value),
            Expression::Cast { expr, target_type } => {
                format!("Cast({}, to: {})", expr, target_type)
            }
            Expression::BinaryOp { op, left, right } => {
                format!("BinaryOp({} {} {})", left, op, right)
            }
            Expression::UnaryOp { op, expr } => format!("UnaryOp({} {})", op, expr),
            Expression::Call { callee, args } => {
                let args_str = args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join(", ");
                format!("Call({}, [{}])", callee, args_str)
            }
        };
        write!(f, "{}", fmtstr)
    }
}
