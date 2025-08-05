use std::{fmt::Display, str::FromStr, hash::{Hash, Hasher}};

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
    NoneType,
    Function(Vec<Type>, Box<Type>),
    Array(Box<Type>),
    UserType(String),
    Pointer(Box<Type>),
    ToBeEvaluated,
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
            Type::NoneType => "none".to_string(),
            Type::Function(params, ret) => format!(
                "function ({}) -> {}",
                params
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret.to_string()
            ),
            Type::Array(elem_type) => format!("array<{}>", elem_type.to_string()),
            Type::Pointer(inner_type) => format!("ptr<{}>", inner_type.to_string()),
            Type::UserType(name) => name.clone(),
            Type::ToBeEvaluated => "TBE".to_string(),
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
            "none" => Ok(Type::NoneType),
            _ => {
                if s.starts_with("ptr<") && s.ends_with('>') {
                    let inner_type_str = &s[8..s.len() - 1];
                    let inner_type = Type::from_str(inner_type_str)?;
                    return Ok(Type::Pointer(Box::new(inner_type)));
                } else {
                    Err(())
                }
            },
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
            Type::NoneType => "none".hash(state),
            Type::Function(params, ret) => {
                params.hash(state);
                ret.hash(state);
            }
            Type::Array(elem_type) => elem_type.hash(state),
            Type::Pointer(inner_type) => {
                "ptr<".hash(state);
                inner_type.hash(state);
                ">".hash(state);
            }
            Type::UserType(name) => name.hash(state),
            Type::ToBeEvaluated => "TBE".hash(state),
        }
    }
}
