#![allow(dead_code)]
use crate::block::Block;
use crate::expression::Expression;
use crate::variable::Variable;
use crate::types::Type;

use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDecl {
        identifier: Variable,
        value: Expression,
    },
    Assignment {
        identifier: Expression,
        value: Expression,
    },
    Call {
        callee: Expression,
        args: Vec<Expression>,
    },
    If {
        condition: Expression,
        then_block: Block,
        else_block: Option<Block>,
    },
    Function {
        name: String,
        ret_type: Type,
        params: Vec<Variable>,
        body: Block,
    },
    Block {
        body: Block,
    },
    Return {
        value: Expression,
    },
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Statement::VariableDecl { identifier, value } => {
                format!("VariableDecl({}, {})", identifier.name, value)
            }
            Statement::Assignment { identifier, value } => {
                format!("Assignment({}, {})", identifier, value)
            }
            Statement::Call { callee, args } => {
                format!(
                    "Call({}, [{}])",
                    callee,
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Statement::If {
                condition,
                then_block,
                else_block,
            } => format!(
                "If(condition: {}, then: {}, else: {:?})",
                condition, then_block, else_block
            ),
            Statement::Function { name, ret_type, params, body } => format!(
                "Function(name: {}, ret_type: {}, params: [{}], body: {})",
                name,
                ret_type,
                params.iter()
                    .map(|p| p.name.clone())
                    .collect::<Vec<_>>()
                    .join(", "),
                body
            ),
            Statement::Block { body } => format!("Block({})", body),
            Statement::Return { value } => format!("Return({})", value),
        };
        write!(f, "{}", fmtstr)
    }
}