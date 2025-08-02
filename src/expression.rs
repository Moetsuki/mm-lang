#[derive(Debug, Clone)]
pub enum Expression {
    Variable(String),
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

impl ToString for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Variable(name) => format!("Variable({})", name),
            Expression::Number(value) => format!("Number({})", value),
            Expression::StringLiteral(value) => format!("StringLiteral({})", value),
            Expression::Boolean(value) => format!("Boolean({})", value),
            Expression::BinaryOp { op, left, right } => {
                format!(
                    "BinaryOp({} {} {})",
                    left.to_string(),
                    op,
                    right.to_string()
                )
            }
            Expression::UnaryOp { op, expr } => {
                format!("UnaryOp({} {})", op, expr.to_string())
            }
        }
    }
}
