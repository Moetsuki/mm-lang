use crate::types::Type;

pub fn type_to_llvm(t: &Type) -> String {
    match t {
        Type::Bool => "i1".to_string(),
        Type::I8 => "i8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::U8 => "i8".to_string(),
        Type::U16 => "i16".to_string(),
        Type::U32 => "i32".to_string(),
        Type::U64 => "i64".to_string(),
        Type::F32 => "float".to_string(),
        Type::F64 => "double".to_string(),
        Type::String => "ptr".to_string(),
        Type::Void => "void".to_string(),
        Type::Pointer(_) => "ptr".to_string(),
        _ => panic!("Unsupported type {} for LLVM IR", t),
    }
}