use crate::{span::Span, types::Type, variable::Variable};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Expression {
    Variable{
        var: Variable,
        span: Span,
    },
    Number {
        value: i64,
        span: Span,
    },
    StringLiteral {
        value: String,
        span: Span,
    },
    Boolean {
        value: bool,
        span: Span,
    },
    Cast {
        expr: Box<Expression>,
        target_type: Type,
        span: Span,
    },
    BinaryOp {
        op: String,
        left: Box<Expression>,
        right: Box<Expression>,
        span: Span,
    },
    UnaryOp {
        op: String,
        expr: Box<Expression>,
        span: Span,
    },
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
        span: Span,
    },
    MethodCall {
        object: Box<Expression>,
        method: String,
        args: Vec<Expression>,
        span: Span,
    },
    FieldAccess {
        object: Box<Expression>,
        field: String,
        span: Span,
    },
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Variable { span, .. } => *span,
            Expression::Number { span, .. } => *span,
            Expression::StringLiteral { span, .. } => *span,
            Expression::Boolean { span, .. } => *span,
            Expression::Cast { span, .. } => *span,
            Expression::BinaryOp { span, .. } => *span,
            Expression::UnaryOp { span, .. } => *span,
            Expression::Call { span, .. } => *span,
            Expression::MethodCall { span, .. } => *span,
            Expression::FieldAccess { span, .. } => *span,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Expression::Variable { var, .. } => format!("Variable::<{}>({})", var.var_type, var.name),
            Expression::Number{ value, .. } => format!("Number({})", value),
            Expression::StringLiteral{ value, .. } => format!("StringLiteral({})", value),
            Expression::Boolean{ value, .. } => format!("Boolean({})", value),
            Expression::Cast { expr, target_type, .. } => {
                format!("Cast({}, to: {})", expr, target_type)
            }
            Expression::BinaryOp { op, left, right, .. } => {
                format!("BinaryOp({} {} {})", left, op, right)
            }
            Expression::UnaryOp { op, expr, .. } => format!("UnaryOp({} {})", op, expr),
            Expression::Call { callee, args, .. } => {
                let args_str = args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join(", ");
                format!("Call({}, [{}])", callee, args_str)
            }
            Expression::MethodCall { object, method, args, .. } => {
                let args_str = args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join(", ");
                format!("Method({}, {}, [{}])", object, method, args_str)
            }
            Expression::FieldAccess { object, field, .. } => {
                format!("FieldAccess({}, {})", object, field)
            }
        };
        write!(f, "{}", fmtstr)
    }
}
