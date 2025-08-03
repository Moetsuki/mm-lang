# Expression System Module

[← Back to README](../README.md)

## Overview

The expression module (`expression.rs`) defines the core expression types and evaluation system for MM-Lang. Expressions represent values, operations, and computations that can be evaluated to produce results.

## Expression Types

### Core Expression Enum

```rust
#[derive(Debug, Clone)]
pub enum Expression {
    Variable(Variable),
    Number(i64),
    StringLiteral(String),
    Boolean(bool),
    BinaryOp {
        op: String,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOp {
        op: String,
        expr: Box<Expression>,
    },
}
```

## Expression Categories

### 1. Literal Expressions

#### Number Literals
```mm
42        // Integer literal
-17       // Negative integer
0         // Zero
```

**Characteristics:**
- Represented as `i64` internally
- Direct value storage
- No additional computation needed

#### String Literals
```mm
"Hello, World!"
"This is a string"
""              // Empty string
```

**Characteristics:**
- UTF-8 encoded strings
- Enclosed in double quotes
- Support for basic content (escape sequences planned)

#### Boolean Literals
```mm
true
false
```

**Characteristics:**
- Simple boolean values
- Used in conditional expressions
- Result of comparison operations

### 2. Variable Expressions

```mm
x           // Variable reference
my_var      // Named variable
result      // Function result storage
```

**Characteristics:**
- References to declared variables
- Contains Variable struct with name and type information
- Resolved during semantic analysis

### 3. Binary Operations

Binary operations combine two expressions with an operator:

#### Arithmetic Operations
```mm
x + y       // Addition
a - b       // Subtraction
x * y       // Multiplication
a / b       // Division
x % y       // Modulo (planned)
```

#### Comparison Operations
```mm
x == y      // Equality
a != b      // Inequality
x < y       // Less than
a > b       // Greater than
x <= y      // Less than or equal
a >= b      // Greater than or equal
```

#### Assignment Operations
```mm
x = 5       // Assignment
y = x + 10  // Assignment with expression
```

**Structure:**
```rust
BinaryOp {
    op: String,           // Operator symbol
    left: Box<Expression>, // Left operand
    right: Box<Expression>, // Right operand
}
```

### 4. Unary Operations

Unary operations operate on a single expression:

```mm
-x          // Negation
!flag       // Logical NOT (planned)
```

**Structure:**
```rust
UnaryOp {
    op: String,           // Operator symbol
    expr: Box<Expression>, // Operand
}
```

## Display Implementation

The module provides a comprehensive `Display` implementation for debugging and error reporting:

```rust
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Expression::Variable(var) => format!("Variable::<{}>({})", var.var_type, var.name),
            Expression::Number(value) => format!("Number({})", value),
            Expression::StringLiteral(value) => format!("StringLiteral({})", value),
            Expression::Boolean(value) => format!("Boolean({})", value),
            Expression::BinaryOp { op, left, right } => {
                format!("BinaryOp({} {} {})", left, op, right)
            }
            Expression::UnaryOp { op, expr } => format!("UnaryOp({} {})", op, expr),
        };
        write!(f, "{}", fmtstr)
    }
}
```

## Expression Examples

### Simple Expressions
```mm
42                          // Number(42)
"hello"                     // StringLiteral(hello)
x                          // Variable::<i64>(x)
true                       // Boolean(true)
```

### Complex Expressions
```mm
x + y * 2                  // BinaryOp(Variable::<i64>(x) + BinaryOp(Variable::<i64>(y) * Number(2)))
(a + b) * c               // BinaryOp(BinaryOp(Variable::<i64>(a) + Variable::<i64>(b)) * Variable::<i64>(c))
x == y + 1                // BinaryOp(Variable::<i64>(x) == BinaryOp(Variable::<i64>(y) + Number(1)))
```

### Nested Expressions
```mm
function_call(x + y, z * 2) // Call with multiple expression arguments
if x > 0 { x } else { -x }  // Conditional with expression branches
```

## Operator Precedence

Expressions are parsed with proper operator precedence:

1. **Highest**: Unary operators (`-`, `!`)
2. **High**: Multiplicative (`*`, `/`, `%`)
3. **Medium**: Additive (`+`, `-`)
4. **Low**: Comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`)
5. **Lowest**: Assignment (`=`)

## Type Information

Expressions carry type information through the Variable components:

```rust
Expression::Variable(Variable {
    name: "x".to_string(),
    var_type: Type::I64,
})
```

This enables:
- **Type checking**: Verify operation compatibility
- **Code generation**: Generate appropriate LLVM IR
- **Error reporting**: Provide type-specific error messages

## Usage in Parser

Expressions are constructed during parsing:

```rust
// Parsing "x + 5"
let left = Expression::Variable(Variable { name: "x", var_type: Type::I64 });
let right = Expression::Number(5);
let expr = Expression::BinaryOp {
    op: "+".to_string(),
    left: Box::new(left),
    right: Box::new(right),
};
```

## Usage in Code Generation

Expressions are evaluated during LLVM IR generation:

```rust
fn transform_expression(&mut self, expr: Expression) -> Register {
    match expr {
        Expression::Number(value) => {
            let reg = Register::new();
            self.emit(&format!("%{} = add i64 0, {}", reg.to_string(), value));
            reg
        }
        Expression::BinaryOp { op, left, right } => {
            let left_reg = self.transform_expression(*left);
            let right_reg = self.transform_expression(*right);
            let result_reg = Register::new();
            let llvm_op = match op.as_str() {
                "+" => "add",
                "-" => "sub",
                "*" => "mul",
                "/" => "sdiv",
                // ... other operators
            };
            self.emit(&format!("%{} = {} i64 %{}, %{}", 
                result_reg.to_string(), llvm_op, left_reg.to_string(), right_reg.to_string()));
            result_reg
        }
        // ... other expression types
    }
}
```

## Expression Evaluation

Expressions support different evaluation contexts:

### 1. Compile-time Evaluation
- Constant folding for literal operations
- Type checking and validation
- Optimization opportunities

### 2. Runtime Evaluation
- LLVM IR generation
- Register allocation
- Efficient code generation

## Memory Management

Expressions use `Box<Expression>` for recursive structures:
- **Heap allocation**: Prevents stack overflow
- **Shared ownership**: Clone-friendly for AST manipulation
- **Memory efficiency**: Only allocates what's needed

## Error Handling

Expression validation includes:

### Type Errors
```mm
"hello" + 42              // Type mismatch
x + undefined_var         // Undefined variable
```

### Operator Errors
```mm
x && y                    // Unsupported operator (planned)
string / number           // Invalid operation
```

## Future Enhancements

### Planned Expression Types
- **Array Access**: `arr[index]`
- **Member Access**: `obj.field`
- **Function Calls**: `func(args)` as expressions
- **Ternary Operator**: `condition ? true_expr : false_expr`
- **Lambda Expressions**: `|x| x + 1`

### Advanced Features
- **Type Inference**: Automatic type deduction
- **Generic Expressions**: Template-based expressions
- **Compile-time Evaluation**: Constant expression evaluation
- **Short-circuit Evaluation**: Lazy evaluation for logical operators

## Testing

Expression module testing:

```rust
#[test]
fn test_expression_display() {
    let expr = Expression::BinaryOp {
        op: "+".to_string(),
        left: Box::new(Expression::Number(5)),
        right: Box::new(Expression::Number(10)),
    };
    assert_eq!(expr.to_string(), "BinaryOp(Number(5) + Number(10))");
}

#[test]
fn test_nested_expressions() {
    let expr = Expression::BinaryOp {
        op: "*".to_string(),
        left: Box::new(Expression::BinaryOp {
            op: "+".to_string(),
            left: Box::new(Expression::Number(1)),
            right: Box::new(Expression::Number(2)),
        }),
        right: Box::new(Expression::Number(3)),
    };
    // Should represent: (1 + 2) * 3
}
```

## Performance Considerations

- **Clone Operations**: Efficient for AST manipulation
- **Box Allocation**: Minimal heap overhead
- **String Storage**: Interning planned for operators
- **Type Storage**: Lightweight type information

The expression system provides a solid foundation for representing and manipulating program computations with room for future language feature expansion.
