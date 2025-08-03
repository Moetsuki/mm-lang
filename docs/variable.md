# Variable System Module

[← Back to README](../README.md)

## Overview

The variable module (`variable.rs`) defines the core structure for representing variables in MM-Lang. Variables carry both name and type information, forming the foundation for the language's type system and symbol management.

## Variable Structure

### Core Variable Definition

```rust
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub var_type: Type,
}
```

**Components:**
- **name**: The identifier used to reference the variable
- **var_type**: The type information from the type system

## Variable Creation and Usage

### Basic Variable Creation

```rust
use crate::types::Type;
use crate::variable::Variable;

// Creating a variable
let var = Variable {
    name: "x".to_string(),
    var_type: Type::I64,
};
```

### In Language Syntax

Variables appear in various contexts within MM-Lang:

#### Variable Declarations
```mm
let x: i64 = 42;           // Variable with explicit type
let name: string = "test"; // String variable
let flag: bool = true;     // Boolean variable
```

#### Function Parameters
```mm
function calculate(input: i64, scale: f64) -> f64 {
    return input * scale;
}
```

#### Variable References
```mm
x = 5;              // Assignment to variable
result = x + y;     // Variables in expressions
print(message);     // Variable as function argument
```

## Type Integration

### Supported Variable Types

Variables can have any type from the type system:

#### Primitive Types
```mm
let count: i64 = 100;          // 64-bit signed integer
let price: f32 = 19.99;        // 32-bit float
let active: bool = true;        // Boolean
let message: string = "hello";  // String
```

#### Advanced Types (Planned)
```mm
let items: array<i64> = [1, 2, 3];           // Array type
let callback: function(i64) -> bool = check;  // Function type
let custom: UserType = UserType::new();       // User-defined type
```

### Type Safety

Variables enforce type safety through the type system:

```rust
impl Variable {
    pub fn new(name: String, var_type: Type) -> Self {
        Variable { name, var_type }
    }
    
    pub fn is_compatible(&self, other_type: &Type) -> bool {
        self.var_type == *other_type
    }
}
```

## Variable Scoping

### Scope Rules

Variables follow lexical scoping rules:

#### Block Scope
```mm
{
    let x: i64 = 10;    // x available in this block
    {
        let y: i64 = 20; // y available in inner block
        // Both x and y accessible here
    }
    // Only x accessible here
}
// Neither x nor y accessible here
```

#### Function Scope
```mm
function example(param: i64) -> i64 {
    let local: i64 = param * 2;  // Function-local variable
    return local;
}
// param and local not accessible outside function
```

#### Global Scope
```mm
let global_var: i64 = 100;  // Available throughout program

function use_global() -> i64 {
    return global_var + 50;  // Can access global variable
}
```

### Variable Shadowing

Inner scopes can shadow outer variables:

```mm
let x: i64 = 10;
{
    let x: string = "hello";  // Shadows outer x
    print(x);                 // Prints "hello"
}
print(x);                     // Prints 10 (outer x)
```

## Variable in AST

### Expression Context

Variables appear as expressions:

```rust
#[derive(Debug, Clone)]
pub enum Expression {
    Variable(Variable),  // Variable reference
    // ... other expression types
}
```

### Statement Context

Variables appear in various statements:

```rust
// In assignments
Statement::Assignment {
    identifier: Expression::Variable(variable),
    value: expression,
}

// In declarations
Statement::VariableDecl {
    identifier: variable,
    value: expression,
}

// In function parameters
Statement::Function {
    name: "func_name".to_string(),
    params: vec![variable1, variable2],
    // ...
}
```

## Symbol Table Integration

### Variable Storage

Variables are stored in symbol tables for scope management:

```rust
pub struct SymbolTable {
    pub symbols: HashMap<Register, Variable>,
}

impl SymbolTable {
    pub fn insert(&mut self, register: Register, variable: Variable) {
        self.symbols.insert(register, variable);
    }
    
    pub fn lookup(&self, name: &str) -> Option<&Variable> {
        self.symbols.values().find(|var| var.name == name)
    }
}
```

### Scope Resolution

Variables are resolved through scope chain traversal:

```rust
impl Scope {
    pub fn resolve_variable(&self, name: &str) -> Option<&Variable> {
        // Search from innermost to outermost scope
        for scope_level in self.stack.iter().rev() {
            if let Some(var) = scope_level.lookup(name) {
                return Some(var);
            }
        }
        None
    }
}
```

## LLVM Code Generation

### Variable Allocation

Variables map to LLVM allocations:

```rust
fn allocate_variable(&mut self, variable: &Variable) -> Register {
    let reg = Register::new();
    let llvm_type = self.type_to_llvm(&variable.var_type);
    
    self.emit(&format!("  %{} = alloca {}", variable.name, llvm_type));
    
    // Store in symbol table
    self.scope.insert(reg.clone(), variable.clone());
    
    reg
}
```

### Variable Access

Variable references generate load instructions:

```rust
fn load_variable(&mut self, variable: &Variable) -> Register {
    let result_reg = Register::new();
    let llvm_type = self.type_to_llvm(&variable.var_type);
    
    self.emit(&format!("  %{} = load {}, {}* %{}", 
        result_reg.to_string(), 
        llvm_type, 
        llvm_type,
        variable.name
    ));
    
    result_reg
}
```

### Variable Assignment

Variable assignments generate store instructions:

```rust
fn store_variable(&mut self, variable: &Variable, value_reg: Register) {
    let llvm_type = self.type_to_llvm(&variable.var_type);
    
    self.emit(&format!("  store {} %{}, {}* %{}", 
        llvm_type, 
        value_reg.to_string(),
        llvm_type,
        variable.name
    ));
}
```

## Type Checking

### Assignment Compatibility

```rust
impl Variable {
    pub fn check_assignment(&self, expr_type: &Type) -> Result<(), TypeError> {
        if self.var_type != *expr_type {
            Err(TypeError::TypeMismatch {
                expected: self.var_type.clone(),
                found: expr_type.clone(),
                variable: self.name.clone(),
            })
        } else {
            Ok(())
        }
    }
}
```

### Function Parameter Matching

```rust
fn check_function_call(
    params: &[Variable], 
    args: &[Expression]
) -> Result<(), TypeError> {
    if params.len() != args.len() {
        return Err(TypeError::ArityMismatch);
    }
    
    for (param, arg) in params.iter().zip(args.iter()) {
        let arg_type = infer_expression_type(arg)?;
        param.check_assignment(&arg_type)?;
    }
    
    Ok(())
}
```

## Error Handling

### Undefined Variable Errors

```mm
x = y + 5;  // Error: Variable 'y' not declared
```

```rust
pub enum VariableError {
    UndefinedVariable(String),
    TypeMismatch { expected: Type, found: Type },
    RedefinitionError(String),
    ScopeViolation(String),
}
```

### Type Mismatch Errors

```mm
let x: i64 = "hello";  // Error: Cannot assign string to i64 variable
```

### Scope Violation Errors

```mm
{
    let x: i64 = 10;
}
print(x);  // Error: Variable 'x' not in scope
```

## Variable Lifetime

### Stack Variables

Most variables are stack-allocated:
- **Automatic cleanup**: Deallocated when scope ends
- **Efficient access**: Direct memory access
- **Type safety**: Compile-time type checking

### Heap Variables (Planned)

For dynamic allocation:
- **Dynamic sizing**: Arrays, strings
- **Reference counting**: Shared ownership
- **Garbage collection**: Automatic memory management

## Display and Debugging

The Variable struct supports debugging output:

```rust
impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.name, self.var_type)
    }
}
```

Example output:
```
x:i64
message:string
callback:function(i64) -> bool
```

## Future Enhancements

### Planned Features

#### Mutable/Immutable Variables
```mm
let x: i64 = 10;        // Immutable by default
let mut y: i64 = 20;    // Explicitly mutable
```

#### Variable Attributes
```rust
pub struct Variable {
    pub name: String,
    pub var_type: Type,
    pub is_mutable: bool,
    pub is_static: bool,
    pub visibility: Visibility,
}
```

#### Reference Variables
```mm
let x: i64 = 10;
let ref_x: &i64 = &x;   // Reference to x
```

#### Generic Variables
```mm
function identity<T>(value: T) -> T {
    let result: T = value;
    return result;
}
```

## Testing

### Variable Creation Testing

```rust
#[test]
fn test_variable_creation() {
    let var = Variable {
        name: "test_var".to_string(),
        var_type: Type::I64,
    };
    
    assert_eq!(var.name, "test_var");
    assert_eq!(var.var_type, Type::I64);
}
```

### Type Compatibility Testing

```rust
#[test]
fn test_type_compatibility() {
    let int_var = Variable {
        name: "x".to_string(),
        var_type: Type::I64,
    };
    
    assert!(int_var.is_compatible(&Type::I64));
    assert!(!int_var.is_compatible(&Type::String));
}
```

## Integration Points

### With Type System
- Type information storage and validation
- Type compatibility checking
- Type inference support

### With Parser
- Variable declaration parsing
- Variable reference resolution
- Parameter list construction

### With Code Generator
- Variable allocation strategies
- Memory layout optimization
- Register allocation hints

### With Optimizer
- Variable usage analysis
- Dead variable elimination
- Variable promotion optimization

## Performance Considerations

- **Memory efficiency**: Minimal overhead per variable
- **Access speed**: Direct hash table lookup
- **Type checking**: Compile-time validation
- **Clone operations**: Efficient for AST manipulation

The variable system provides a robust foundation for identifier management in MM-Lang with strong type safety and efficient implementation.
