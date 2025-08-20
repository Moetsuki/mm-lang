# Type System Module

[← Back to README](../README.md)

## Overview

The type system module (`types.rs`) defines the core type system for MM-Lang, providing static type checking, type inference, and type safety guarantees. It serves as the foundation for semantic analysis and code generation.

## Type Definition

### Core Type Enum

### Core Type Enum (current)

```rust
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Bool,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    String,
    Void,
    Function { name: String, args: Vec<Type>, ret_type: Box<Type>, is_variadic: bool },
    Class { name: String, parent: Option<Box<Type>>, fields: Vec<(Type, Visibility)>, methods: Vec<(Type, Visibility)> },
    Struct { name: String, parent: Option<Box<Type>>, fields: Vec<Type> },
    Tensor { var_type: Box<Type>, dimensions: Vec<usize> },
    UserDefined(String, Box<Type>),
    Pointer(Box<Type>),
    ToBeEvaluated(String),
}
```

## Primitive Types

### Boolean Type

```mm
let flag: bool = true;
let active: bool = false;
```

**Characteristics:**
- Two values: `true` and `false`
- Used in conditional expressions
- Result of comparison operations
- 1 byte in memory

### Integer Types

#### Signed Integers
```mm
let tiny: i8 = -128;        // 8-bit signed (-128 to 127)
let small: i16 = -32768;    // 16-bit signed (-32,768 to 32,767)
let medium: i32 = -2147483648; // 32-bit signed
let large: i64 = -9223372036854775808; // 64-bit signed
```

#### Unsigned Integers
```mm
let byte: u8 = 255;         // 8-bit unsigned (0 to 255)
let word: u16 = 65535;      // 16-bit unsigned (0 to 65,535)
let dword: u32 = 4294967295;   // 32-bit unsigned
let qword: u64 = 18446744073709551615; // 64-bit unsigned
```

**Characteristics:**
- Fixed-size integer representations
- Overflow behavior defined
- Efficient arithmetic operations
- Default integer type is `i64`

### Floating-Point Types

```mm
let precision: f32 = 3.14159;    // 32-bit IEEE 754
let high_precision: f64 = 3.141592653589793; // 64-bit IEEE 754
```

**Characteristics:**
- IEEE 754 compliance
- Standard floating-point operations
- Default float literal lowers to `f64`
- NaN and infinity support

### String Type

```mm
let message: string = "Hello, World!";
let empty: string = "";
let multiline: string = "Line 1\nLine 2";
```

**Characteristics:**
- UTF-8 encoded text
- Immutable by default
- Dynamic length
- Heap-allocated

### Void Type

```mm
function log_message(msg: string) -> none {
    print(msg);
    // Implicit return none
}
```

**Characteristics:**
- Represents absence of value
- Used for functions with no return value
- Similar to `void` in C/C++
- Zero-sized type

## Complex Types

### Function Types

```mm
function add(x: i64, y: i64) -> i64 {
    return x + y;
}

// Function type: function(i64, i64) -> i64
let calculator: function(i64, i64) -> i64 = add;
```

**Structure:**
```rust
Function { name: String, args: Vec<Type>, ret_type: Box<Type>, is_variadic: bool }
//       parameters  return_type
```

**Characteristics:**
- First-class values
- Parameter type list
- Single return type
- Support for higher-order functions

### Array Types

```mm
let numbers: tensor<i64> = {1, 2, 3, 4, 5};
let names: tensor<string> = {"Alice", "Bob", "Charlie"};
let matrix: tensor<tensor<f64>> = {{1.0, 2.0}, {3.0, 4.0}};
```

**Structure:**
```rust
Tensor { var_type: Box<Type>, dimensions: Vec<usize> }
//    element_type
```
### User‑Defined/Composite Types

- Class types carry parent linkage, field types with visibility, and method function types. Lowered to `%ClassName` with a leading vtable pointer (fields start at index 1).
- Struct types are plain aggregates with fields in declaration order. Lowered to `%StructName` without a vtable (fields start at index 0).
- `UserDefined(String, Box<Type>)` and `Pointer<T>` are used internally during parsing/codegen.

Shapes for reference:

```rust
Class { name: String, parent: Option<Box<Type>>, fields: Vec<(Type, Visibility)>, methods: Vec<(Type, Visibility)> }
Struct { name: String, parent: Option<Box<Type>>, fields: Vec<Type> }
Tensor { var_type: Box<Type>, dimensions: Vec<usize> }
UserDefined(String, Box<Type>)
Pointer(Box<Type>)
```

**Characteristics:**
- Custom type definitions
- Structural typing
- Method definitions (planned)
- Generic parameters (planned)

## Type Operations

### Type Display

```rust
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
            Type::Function { name, args, ret_type, is_variadic } => {
                let params_str = args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                let variadic_str = if *is_variadic { ", ..." } else { "" };
                format!("function {}({}{}) -> {}", name, params_str, variadic_str, ret_type)
            }
            Type::Class { name, parent, fields, methods } => {
                let parent_str = if let Some(p) = parent { format!(" extends {}", p) } else { String::new() };
                let fields_str = fields.iter().map(|(t, v)| format!("{}: {}", v, t)).collect::<Vec<_>>().join(", ");
                let methods_str = methods.iter().map(|(t, v)| format!("{}: {}", v, t)).collect::<Vec<_>>().join(", ");
                format!("class {}{} {{ fields: [{}], methods: [{}] }}", name, parent_str, fields_str, methods_str)
            }
            Type::Struct { name, parent, fields } => {
                let parent_str = if let Some(p) = parent { format!(" extends {}", p) } else { String::new() };
                let fields_str = fields.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                format!("struct {}{} {{ fields: [{}] }}", name, parent_str, fields_str)
            }
            Type::Tensor { var_type, dimensions } => format!("tensor[{:?};{:?}]", var_type, dimensions),
            Type::Pointer(inner) => format!("ptr<{}>", inner),
            Type::UserDefined(name, typ) => format!("UserDefined {} <{}>", name, typ),
            Type::ToBeEvaluated(s) => s.to_string(),
        };
        write!(f, "{}", fmtstr)
    }
}
```
## Type Parsing

```rust
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
                    // parse inner type
                    let inner = &s[4..s.len()-1];
                    let inner = Type::from_str(inner)?;
                    Ok(Type::Pointer(Box::new(inner)))
                } else {
                    Err(())
                }
            }
        }
    }
}
```

## Type Checking

### Type Compatibility

```rust
impl Type {
    pub fn is_compatible(&self, other: &Type) -> bool {
        match (self, other) {
            // Exact matches
            (Type::I64, Type::I64) => true,
            (Type::String, Type::String) => true,
            (Type::Bool, Type::Bool) => true,
            
            // Numeric conversions (planned)
            (Type::I32, Type::I64) => true,  // Widening conversion
            (Type::F32, Type::F64) => true,  // Precision upgrade
            
            // Function compatibility
            (Type::Function(p1, r1), Type::Function(p2, r2)) => {
                p1.len() == p2.len() && 
                p1.iter().zip(p2.iter()).all(|(t1, t2)| t1.is_compatible(t2)) &&
                r1.is_compatible(r2)
            }
            
            // Tensor compatibility (same element type)
            (Type::Tensor { var_type: e1, .. }, Type::Tensor { var_type: e2, .. }) => e1 == e2,
            
            _ => false,
        }
    }
}
```

### Type Inference

```rust
pub fn infer_expression_type(expr: &Expression) -> Result<Type, TypeError> {
    match expr {
        Expression::Number(_) => Ok(Type::I64),
        Expression::StringLiteral(_) => Ok(Type::String),
        Expression::Boolean(_) => Ok(Type::Bool),
        Expression::Variable(var) => Ok(var.var_type.clone()),
        Expression::BinaryOp { op, left, right } => {
            let left_type = infer_expression_type(left)?;
            let right_type = infer_expression_type(right)?;
            infer_binary_op_type(op, &left_type, &right_type)
        }
        Expression::UnaryOp { op, expr } => {
            let expr_type = infer_expression_type(expr)?;
            infer_unary_op_type(op, &expr_type)
        }
    }
}
```

### Binary Operation Type Rules

```rust
fn infer_binary_op_type(op: &str, left: &Type, right: &Type) -> Result<Type, TypeError> {
    match op {
        "+" | "-" | "*" | "/" => {
            match (left, right) {
                (Type::I64, Type::I64) => Ok(Type::I64),
                (Type::F64, Type::F64) => Ok(Type::F64),
                (Type::I64, Type::F64) => Ok(Type::F64), // Promote to float
                (Type::F64, Type::I64) => Ok(Type::F64), // Promote to float
                _ => Err(TypeError::InvalidBinaryOperation {
                    op: op.to_string(),
                    left: left.clone(),
                    right: right.clone(),
                })
            }
        }
        "==" | "!=" | "<" | ">" | "<=" | ">=" => {
            if left.is_compatible(right) {
                Ok(Type::Bool)
            } else {
                Err(TypeError::IncompatibleComparison {
                    left: left.clone(),
                    right: right.clone(),
                })
            }
        }
        "=" => {
            if right.is_compatible(left) {
                Ok(left.clone())
            } else {
                Err(TypeError::TypeMismatch {
                    expected: left.clone(),
                    found: right.clone(),
                })
            }
        }
        _ => Err(TypeError::UnknownOperator(op.to_string()))
    }
}
```

## Type Conversion

### Implicit Conversions

```mm
let x: i32 = 42;
let y: i64 = x;        // Implicit widening conversion
let z: f64 = y;        // Implicit int-to-float conversion
```

### Explicit Conversions (Planned)

```mm
let x: i64 = 42;
let y: i32 = cast<i32>(x);     // Explicit narrowing cast
let z: string = toString(x);    // Explicit string conversion
```

## Error Types

```rust
#[derive(Debug, Clone)]
pub enum TypeError {
    TypeMismatch {
        expected: Type,
        found: Type,
    },
    InvalidBinaryOperation {
        op: String,
        left: Type,
        right: Type,
    },
    IncompatibleComparison {
        left: Type,
        right: Type,
    },
    UnknownOperator(String),
    UnknownType(String),
    FunctionArityMismatch {
        expected: usize,
        found: usize,
    },
}
```

## LLVM Type Mapping

```rust
impl Type {
    pub fn to_llvm(&self) -> String {
        match self {
            Type::Bool => "i1".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "i8".to_string(),  // LLVM treats as signed
            Type::U16 => "i16".to_string(),
            Type::U32 => "i32".to_string(),
            Type::U64 => "i64".to_string(),
            Type::F32 => "float".to_string(),
            Type::F64 => "double".to_string(),
            Type::String => "i8*".to_string(),  // Pointer to char array
            Type::Void => "void".to_string(),
            Type::Function(params, ret) => {
                let param_types = params.iter()
                    .map(|t| t.to_llvm())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} ({})*", ret.to_llvm(), param_types)
            }
            // Tensors lower to element pointers in codegen paths
            Type::UserDefined(_) => "i8*".to_string(),  // Generic pointer
            Type::ToBeEvaluated => "i8*".to_string(), // Placeholder
        }
    }
}
```

## Future Enhancements

### Generic Types

```mm
struct List<T> {
    data: array<T>,
    size: i64,
}

function map<T, U>(list: List<T>, func: function(T) -> U) -> List<U> {
    // Generic function implementation
}
```

### Union Types

```mm
type Number = i64 | f64;
type Result<T> = T | Error;
```

### Trait System

```mm
trait Printable {
    function print(self) -> none;
}

impl Printable for i64 {
    function print(self) -> none {
        // Implementation
    }
}
```

### Lifetime Annotations

```mm
function longest<'a>(x: &'a string, y: &'a string) -> &'a string {
    if x.length() > y.length() { x } else { y }
}
```

## Testing

### Type Equality Testing

```rust
#[test]
fn test_type_equality() {
    assert_eq!(Type::I64, Type::I64);
    assert_ne!(Type::I64, Type::I32);
    
    let func_type1 = Type::Function(vec![Type::I64], Box::new(Type::Bool));
    let func_type2 = Type::Function(vec![Type::I64], Box::new(Type::Bool));
    assert_eq!(func_type1, func_type2);
}
```

### Type Compatibility Testing

```rust
#[test]
fn test_type_compatibility() {
    assert!(Type::I32.is_compatible(&Type::I64));  // Widening
    assert!(!Type::I64.is_compatible(&Type::I32)); // Narrowing
    assert!(Type::F32.is_compatible(&Type::F64));  // Precision
}
```

### Type Display Testing

```rust
#[test]
fn test_type_display() {
    assert_eq!(Type::I64.to_string(), "i64");
    assert_eq!(Type::Bool.to_string(), "bool");
    
    let func_type = Type::Function(
        vec![Type::I64, Type::String], 
        Box::new(Type::Bool)
    );
    assert_eq!(func_type.to_string(), "function (i64, string) -> bool");
}
```

## Integration Points

### With Parser
- Type annotation parsing
- Type validation during parsing
- Default type assignment

### With Variable System
- Variable type storage
- Type compatibility checking
- Type inference for declarations

### With Expression System
- Expression type inference
- Operation result types
- Type promotion rules

### With Code Generator
- LLVM type mapping
- Type-specific code generation
- Memory layout calculation

## Performance Considerations

- **Type checking**: Compile-time only (zero runtime cost)
- **Type storage**: Minimal memory overhead
- **Type comparison**: Efficient enum matching
- **LLVM mapping**: Direct translation to LLVM types

The type system provides strong static typing with zero runtime overhead, enabling both safety and performance in MM-Lang programs.
