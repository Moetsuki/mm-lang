#![allow(dead_code)]
use crate::block::Block;
use crate::expression::Expression;
use crate::variable::Variable;

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
        params: Vec<Variable>,
        body: Block,
    },
    Block {
        body: Block,
    },
}
