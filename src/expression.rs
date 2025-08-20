use crate::{span::Span, types::Type, variable::Variable};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Expression {
    Variable {
        var: Variable,
        span: Span,
    },
    Number {
        value: i64,
        span: Span,
    },
    Float {
        value: f64,
        span: Span,
    },
    StringLiteral {
        value: String,
        span: Span,
    },
    InitializerList {
        elements: Vec<Expression>,
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
    ArrayAccess {
        array: Box<Expression>,
        index: Box<Expression>,
        span: Span,
    },
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Variable { span, .. } => *span,
            Expression::Number { span, .. } => *span,
            Expression::Float { span, .. } => *span,
            Expression::StringLiteral { span, .. } => *span,
            Expression::InitializerList { span, .. } => *span,
            Expression::Boolean { span, .. } => *span,
            Expression::Cast { span, .. } => *span,
            Expression::BinaryOp { span, .. } => *span,
            Expression::UnaryOp { span, .. } => *span,
            Expression::Call { span, .. } => *span,
            Expression::MethodCall { span, .. } => *span,
            Expression::FieldAccess { span, .. } => *span,
            Expression::ArrayAccess { span, .. } => *span,
        }
    }

    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Expression::Variable { span, .. } => span,
            Expression::Number { span, .. } => span,
            Expression::Float { span, .. } => span,
            Expression::StringLiteral { span, .. } => span,
            Expression::InitializerList { span, .. } => span,
            Expression::Boolean { span, .. } => span,
            Expression::Cast { span, .. } => span,
            Expression::BinaryOp { span, .. } => span,
            Expression::UnaryOp { span, .. } => span,
            Expression::Call { span, .. } => span,
            Expression::MethodCall { span, .. } => span,
            Expression::FieldAccess { span, .. } => span,
            Expression::ArrayAccess { span, .. } => span,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Expression::Variable { var, .. } => {
                format!("Variable::<{}>({})", var.var_type, var.name)
            }
            Expression::Number { value, .. } => format!("Number({})", value),
            Expression::Float { value, .. } => format!("Float({})", value),
            Expression::StringLiteral { value, .. } => format!("StringLiteral({})", value),
            Expression::Boolean { value, .. } => format!("Boolean({})", value),
            Expression::InitializerList { elements, .. } => {
                let elements_str = elements
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("InitializerList([{}])", elements_str)
            }
            Expression::Cast {
                expr, target_type, ..
            } => {
                format!("Cast({}, to: {})", expr, target_type)
            }
            Expression::BinaryOp {
                op, left, right, ..
            } => {
                format!("BinaryOp({} {} {})", left, op, right)
            }
            Expression::UnaryOp { op, expr, .. } => format!("UnaryOp({} {})", op, expr),
            Expression::Call { callee, args, .. } => {
                let args_str = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Call({}, [{}])", callee, args_str)
            }
            Expression::MethodCall {
                object,
                method,
                args,
                ..
            } => {
                let args_str = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Method({}, {}, [{}])", object, method, args_str)
            }
            Expression::FieldAccess { object, field, .. } => {
                format!("FieldAccess({}, {})", object, field)
            }
            Expression::ArrayAccess { array, index, .. } => {
                format!("ArrayAccess({}, {})", array, index)
            }
        };
        write!(f, "{}", fmtstr)
    }
}
