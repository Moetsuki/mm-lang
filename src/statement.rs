#![allow(dead_code)]
use crate::block::Block;
use crate::expression::Expression;
use crate::variable::Variable;
use crate::types::Type;

use std::fmt::Display;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Protected,
}

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
    Class {
        name: String,
        parent: Option<String>,
        fields: Vec<(Variable, Visibility)>,
        methods: Vec<(Box<Statement>, Visibility)>,
    },
    Struct {
        id: u64,
        name: String,
        parent: Option<Box<Statement>>,
        fields: Vec<Variable>,
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
            Statement::Class { name, parent, fields, methods } => {
                let fields_str = fields.iter()
                    .map(|(var, vis)| format!("{}: {} ({:?})", var.name, var.var_type, vis))
                    .collect::<Vec<_>>()
                    .join(", ");
                let methods_str = methods.iter()
                    .map(|method| format!("{} ({:?})", method.0, method.1))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Class(name: {}, parent: {:?}, fields: [{}], methods: [{}])", name, parent, fields_str, methods_str)
            }
            Statement::Struct { id, name, parent, fields } => {
                let parent_str = if let Some(p) = parent {
                    format!(" extends {}", p)
                } else {
                    String::new()
                };
                let fields_str = fields.iter()
                    .map(|f| f.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Struct(id: {}, name: {}{}, fields: [{}])", id, name, parent_str, fields_str)
            }
        };
        write!(f, "{}", fmtstr)
    }
}

impl Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Visibility::Public => "public".to_string(),
            Visibility::Private => "private".to_string(),
            Visibility::Protected => "protected".to_string(),
        };
        write!(f, "{}", fmtstr)
    }
}

// Implement Hash for Visibility if not already derived
impl std::hash::Hash for Visibility {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
    }
}