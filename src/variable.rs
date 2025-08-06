use crate::types::Type;

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub var_type: Type,
}