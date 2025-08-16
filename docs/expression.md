# Expression System Module

[← Back to README](../README.md)

## Overview

The expression module (`expression.rs`) defines the core expression types and evaluation system for MM-Lang. Expressions represent values, operations, and computations that can be evaluated to produce results.

## Expression Types

### Core Expression Enum (current)

```rust
#[derive(Debug, Clone)]
pub enum Expression {
    Variable(Variable),
    Number(i64),
    StringLiteral(String),
    Boolean(bool),
    Cast { expr: Box<Expression>, target_type: Type },
    BinaryOp { op: String, left: Box<Expression>, right: Box<Expression> },
    UnaryOp { op: String, expr: Box<Expression> },
    Call { callee: Box<Expression>, args: Vec<Expression> },
    MethodCall { object: Box<Expression>, method: String, args: Vec<Expression> },
    FieldAccess { object: Box<Expression>, field: String },
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

Assignments are handled as statements (see Statement module). Binary operations here are pure expressions.

### 4. Unary Operations

Unary operations operate on a single expression:

```mm
-x          // Negation
!flag       // Logical NOT
```

**Structure:**
```rust
UnaryOp {
    op: String,           // Operator symbol
    expr: Box<Expression>, // Operand
}
```

### 5. Calls, Field Access, Method Calls, Casts

- Casts: `expr as Type` are represented as `Cast { expr, target_type }` and lowered to LLVM conversion instructions.
- Function calls: `Call { callee, args }`, where `callee` is often a `Variable`. Constructors use the class name; the `init` method is resolved during codegen.
- Field access: `FieldAccess { object, field }` compiles to `getelementptr` + `load/store` with the class layout (vtable at index 0, fields starting at index 1).
- Method calls: `MethodCall { object, method, args }` use dynamic dispatch via a per-class vtable.

## Display Implementation

The module implements `Display` in code for human-friendly debugging output.

## Expression Examples

### Simple Expressions
```mm
42                          // Number(42)
"hello"                     // StringLiteral(hello)
x                           // Variable::<i64>(x)
true                        // Boolean(true)
```

### Complex Expressions
```mm
x + y * 2                  // BinaryOp(Variable::<i64>(x) + BinaryOp(Variable::<i64>(y) * Number(2)))
(a + b) * c                // BinaryOp(BinaryOp(Variable::<i64>(a) + Variable::<i64>(b)) * Variable::<i64>(c))
x == y + 1                 // BinaryOp(Variable::<i64>(x) == BinaryOp(Variable::<i64>(y) + Number(1)))
```

### Calls/Access
```mm
printf("hi");             // Call
obj.field = 5;             // FieldAccess + store
obj.method(1, 2);          // MethodCall via vtable
n: i32 = (x as i32);       // Cast
```

## Operator Precedence

Expressions are parsed with proper operator precedence:

1. Highest: Unary operators (`-`, `!`)
2. High: Multiplicative (`*`, `/`, `%`)
3. Medium: Additive (`+`, `-`)
4. Low: Comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`)
5. Assignments are statements, not expressions.

## Usage in Code Generation (high level)

- Arithmetic/compare: integer ops; implicit integer widening or explicit casts inserted.
- Casts: `sext`/`trunc`/`zext`/`sitofp`/`fptosi`/`fpext`/`fptrunc`.
- Strings: private globals with `getelementptr` to C string pointer.
- Calls: free functions directly; constructors via resolved `init` method.
- Field access: `getelementptr` with index +1 (skip vtable), then `load`/`store`.
- Method calls: dynamic dispatch via vtable pointer loaded from the object.

## Error Handling

Type mismatches and unsupported operations panic with helpful context during development.

## Future Enhancements

- Array access `arr[i]`, ternary operator, lambda/closures, and short-circuit logical ops.
