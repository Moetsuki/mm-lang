use std::{
    fmt::Display,
    hash::{Hash, Hasher},
    str::FromStr,
};

use crate::statement::Visibility;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    String,
    Void,
    Function {
        name: String,
        args: Vec<Type>,
        ret_type: Box<Type>,
        is_variadic: bool,
    },
    Class {
        name: String,
        parent: Option<Box<Type>>,
        fields: Vec<(Type, Visibility)>,
        methods: Vec<(Type, Visibility)>,
    },
    Struct {
        name: String,
        parent: Option<Box<Type>>,
        fields: Vec<Type>,
    },
    Array(Box<Type>),
    UserDefined(String, Box<Type>),
    Pointer(Box<Type>),
    ToBeEvaluated(String),
}

impl Type {
    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::F32 | Type::F64
        )
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(self, Type::U8 | Type::U16 | Type::U32 | Type::U64)
    }

    pub fn is_floating(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Type::Bool => "bool".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::String => "string".to_string(),
            Type::Void => "none".to_string(),
            Type::Function {
                name,
                args,
                ret_type,
                is_variadic,
            } => {
                let params_str = args
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                let variadic_str = if *is_variadic { ", ..." } else { "" };
                format!(
                    "function {}({}{}) -> {}",
                    name, params_str, variadic_str, ret_type
                )
            }
            Type::Class {
                name,
                parent,
                fields,
                methods,
            } => {
                let parent_str = if let Some(p) = parent {
                    format!(" extends {}", p)
                } else {
                    String::new()
                };
                let fields_str = fields
                    .iter()
                    .map(|(field_type, visibility)| format!("{}: {}", visibility, field_type))
                    .collect::<Vec<_>>()
                    .join(", ");
                let methods_str = methods
                    .iter()
                    .map(|(method, visibility)| format!("{}: {}", visibility, method))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "class {}{} {{ fields: [{}], methods: [{}] }}",
                    name, parent_str, fields_str, methods_str
                )
            }
            Type::Struct {
                name,
                parent,
                fields,
            } => {
                let parent_str = if let Some(p) = parent {
                    format!(" extends {}", p)
                } else {
                    String::new()
                };
                let fields_str = fields
                    .iter()
                    .map(|field| field.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "struct {}{} {{ fields: [{}] }}",
                    name, parent_str, fields_str
                )
            }
            Type::Array(elem_type) => format!("array<{}>", elem_type),
            Type::Pointer(inner_type) => format!("ptr<{}>", inner_type),
            Type::UserDefined(name, typ) => {
                format!("UserDefined {} <{}>", name, typ)
            }
            Type::ToBeEvaluated(tbe_str) => tbe_str.to_string(),
        };
        write!(f, "{}", fmtstr)
    }
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(Type::Bool),
            "i8" => Ok(Type::I8),
            "i16" => Ok(Type::I16),
            "i32" => Ok(Type::I32),
            "i64" => Ok(Type::I64),
            "u8" => Ok(Type::U8),
            "u16" => Ok(Type::U16),
            "u32" => Ok(Type::U32),
            "u64" => Ok(Type::U64),
            "f32" => Ok(Type::F32),
            "f64" => Ok(Type::F64),
            "string" => Ok(Type::String),
            "none" => Ok(Type::Void),
            _ => {
                if s.starts_with("ptr<") && s.ends_with('>') {
                    let inner_type_str = &s[8..s.len() - 1];
                    let inner_type = Type::from_str(inner_type_str)?;
                    Ok(Type::Pointer(Box::new(inner_type)))
                } else {
                    Err(())
                }
            }
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Bool => "bool".hash(state),
            Type::I8 => "i8".hash(state),
            Type::I16 => "i16".hash(state),
            Type::I32 => "i32".hash(state),
            Type::I64 => "i64".hash(state),
            Type::U8 => "u8".hash(state),
            Type::U16 => "u16".hash(state),
            Type::U32 => "u32".hash(state),
            Type::U64 => "u64".hash(state),
            Type::F32 => "f32".hash(state),
            Type::F64 => "f64".hash(state),
            Type::String => "string".hash(state),
            Type::Void => "none".hash(state),
            Type::Function { args, ret_type, .. } => {
                args.hash(state);
                ret_type.hash(state);
            }
            Type::Class {
                name,
                parent,
                fields,
                methods,
            } => {
                name.hash(state);
                if let Some(p) = parent {
                    p.hash(state);
                }
                fields.hash(state);
                methods.hash(state);
            }
            Type::Struct {
                name,
                parent,
                fields,
            } => {
                name.hash(state);
                if let Some(p) = parent {
                    p.hash(state);
                }
                fields.hash(state);
            }
            Type::Array(elem_type) => elem_type.hash(state),
            Type::Pointer(inner_type) => {
                "ptr<".hash(state);
                inner_type.hash(state);
                ">".hash(state);
            }
            Type::UserDefined(name, typ) => {
                name.hash(state);
                typ.hash(state);
            }
            Type::ToBeEvaluated(tbe_str) => tbe_str.hash(state),
        }
    }
}
