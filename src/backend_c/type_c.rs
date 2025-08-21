use crate::types::Type;

#[allow(clippy::only_used_in_recursion)]
pub fn type_to_c(t: &Type) -> String {
    match t {
        Type::Bool => "bool".into(),
        Type::I8 => "int8_t".into(),
        Type::I16 => "int16_t".into(),
        Type::I32 => "int32_t".into(),
        Type::I64 => "int64_t".into(),
        Type::U8 => "uint8_t".into(),
        Type::U16 => "uint16_t".into(),
        Type::U32 => "uint32_t".into(),
        Type::U64 => "uint64_t".into(),
        Type::F32 => "float".into(),
        Type::F64 => "double".into(),
        Type::String => "const char*".into(),
        Type::Void => "void".into(),
        Type::Pointer(inner) => format!("{}*", type_to_c(inner)),
        Type::Function { ret_type, .. } => type_to_c(ret_type),
        Type::ToBeEvaluated(name) => name.clone(),
        Type::UserDefined(_, inner) => type_to_c(inner),
        Type::Class { name, .. } => format!("struct {}", name),
        Type::Struct { name, .. } => format!("struct {}", name),
        Type::Tensor { var_type, .. } => type_to_c(var_type),
    }
}