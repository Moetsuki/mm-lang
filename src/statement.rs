#![allow(dead_code)]
use crate::block::Block;
use crate::expression::Expression;
use crate::span::Span;
use crate::variable::Variable;
use crate::types::Type;

use std::fmt::Display;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Visibility {
    Default,
    Public,
    Private,
    Protected,
}

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDecl {
        identifier: Variable,
        visibility: Visibility,
        value: Expression,
        span: Span,
    },
    Assignment {
        identifier: Expression,
        value: Expression,
        span: Span,
    },
    Call {
        callee: Expression,
        args: Vec<Expression>,
        span: Span,
    },
    If {
        condition: Expression,
        then_block: Block,
        elif: Vec<Statement>,
        else_block: Option<Block>,
        span: Span,
    },
    Function {
        name: String,
        visibility: Visibility,
        ret_type: Type,
        params: Vec<Variable>,
        body: Block,
        span: Span,
    },
    Block {
        body: Block,
        span: Span,
    },
    Return {
        value: Expression,
        span: Span,
    },
    Class {
        name: String,
        visibility: Visibility,
        parent: Option<String>,
        fields: Vec<(Variable, Visibility)>,
        methods: Vec<(Box<Statement>, Visibility)>,
        span: Span,
    },
    Struct {
        id: u64,
        name: String,
        visibility: Visibility,
        parent: Option<Box<Statement>>,
        fields: Vec<Variable>,
        span: Span,
    },
    Use {
        module_path: String,
        span: Span,
    }
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::VariableDecl { span, .. } => *span,
            Statement::Assignment { span, .. } => *span,
            Statement::Call { span, .. } => *span,
            Statement::If { span, .. } => *span,
            Statement::Function { span, .. } => *span,
            Statement::Block { span, .. } => *span,
            Statement::Return { span, .. } => *span,
            Statement::Class { span, .. } => *span,
            Statement::Struct { span, .. } => *span,
            Statement::Use { span, .. } => *span,
        }
    }
    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Statement::VariableDecl { span, .. } => span,
            Statement::Assignment { span, .. } => span,
            Statement::Call { span, .. } => span,
            Statement::If { span, .. } => span,
            Statement::Function { span, .. } => span,
            Statement::Block { span, .. } => span,
            Statement::Return { span, .. } => span,
            Statement::Class { span, .. } => span,
            Statement::Struct { span, .. } => span,
            Statement::Use { span, .. } => span,
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Statement::VariableDecl { identifier, value, visibility, .. } => {
                format!("VariableDecl({}, {}, {})", identifier.name, value, visibility)
            }
            Statement::Assignment { identifier, value, .. } => {
                format!("Assignment({}, {})", identifier, value)
            }
            Statement::Call { callee, args, .. } => {
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
                elif,
                else_block,
                ..
            } => format!(
                "If(condition: {},\nthen: {},elif: {:?}\nelse: {:?})",
                condition, then_block, elif, else_block
            ),
            Statement::Function { name, visibility, ret_type, params, body, .. } => format!(
                "Function(name: {}, {}, ret_type: {}, params: [{}], body: {})",
                name,
                visibility,
                ret_type,
                params.iter()
                    .map(|p| p.name.clone())
                    .collect::<Vec<_>>()
                    .join(", "),
                body
            ),
            Statement::Block { body, .. } => format!("Block({})", body),
            Statement::Return { value, .. } => format!("Return({})", value),
            Statement::Class { name, visibility, parent, fields, methods, .. } => {
                let fields_str = fields.iter()
                    .map(|(var, vis)| format!("{}: {} ({:?})", var.name, var.var_type, vis))
                    .collect::<Vec<_>>()
                    .join(", ");
                let methods_str = methods.iter()
                    .map(|method| format!("{} ({:?})", method.0, method.1))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Class(name: {}, {}, parent: {:?}, fields: [{}], methods: [{}])", name, visibility, parent, fields_str, methods_str)
            }
            Statement::Struct { id, name, visibility, parent, fields, .. } => {
                let parent_str = if let Some(p) = parent {
                    format!(" extends {}", p)
                } else {
                    String::new()
                };
                let fields_str = fields.iter()
                    .map(|f| f.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Struct(id: {}, name: {}{}, {}, fields: [{}])", id, name, parent_str, visibility, fields_str)
            }
            Statement::Use { module_path, .. } => {
                format!("Use({})", module_path)
            }
        };
        write!(f, "{}", fmtstr)
    }
}

impl Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Visibility::Default => "default".to_string(),
            Visibility::Public => "public".to_string(),
            Visibility::Private => "private".to_string(),
            Visibility::Protected => "protected".to_string(),
        };
        write!(f, "{}", fmtstr)
    }
}

impl From<&String> for Visibility {
    fn from(s: &String) -> Self {
        match s.as_str() {
            "default" => Visibility::Default,
            "public" => Visibility::Public,
            "private" => Visibility::Private,
            "protected" => Visibility::Protected,
            _ => panic!("Unknown visibility: {}", s),
        }
    }
}

impl From<&str> for Visibility {
    fn from(s: &str) -> Self {
        match s {
            "default" => Visibility::Default,
            "public" => Visibility::Public,
            "private" => Visibility::Private,
            "protected" => Visibility::Protected,
            _ => panic!("Unknown visibility: {}", s),
        }
    }
}

// Implement Hash for Visibility if not already derived
impl std::hash::Hash for Visibility {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
    }
}
