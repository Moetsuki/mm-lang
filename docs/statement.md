# Statement System Module

[← Back to README](../README.md)

## Overview

The statement module (`statement.rs`) defines the core statement types that represent executable actions in MM-Lang programs. Statements form the building blocks of program control flow and execution.

## Statement Types

### Core Statement Enum (current)

```rust
#[derive(Debug, Clone)]
pub enum Statement {
    VariableDecl { identifier: Variable, value: Expression },
    Assignment { identifier: Expression, value: Expression },
    Call { callee: Expression, args: Vec<Expression> },
    If { condition: Expression, then_block: Block, else_block: Option<Block> },
    Function { name: String, ret_type: Type, params: Vec<Variable>, body: Block },
    Block { body: Block },
    Return { value: Expression },
    Class { name: String, parent: Option<String>, fields: Vec<(Variable, Visibility)>, methods: Vec<(Box<Statement>, Visibility)> },
    Struct { id: u64, name: String, parent: Option<Box<Statement>>, fields: Vec<Variable> },
}
```

## Statement Categories

### 1. Variable Declaration

```mm
let x: i64 = 42;
let name: string = "MM-Lang";
```

**Structure:**
```rust
VariableDecl {
    identifier: Variable,    // Variable with name and type
    value: Expression,       // Initial value expression
}
```

**Characteristics:**
- Introduces new variables into scope
- Requires explicit type annotation
- Must provide initial value
- Creates new symbol table entry

### 2. Assignment

```mm
x = 5;
result = x + y * 2;
array[index] = value;    // Planned
```

**Structure:**
```rust
Assignment {
    identifier: Expression,  // Target (usually Variable)
    value: Expression,      // Value to assign
}
```

**Characteristics:**
- Modifies existing variables
- Target must be assignable (lvalue)
- Type compatibility checked
- Most common statement type

### 3. Function Call

```mm
print("Hello, World!");
result = calculate(x, y, z);
process_data(input, config);
```

**Structure:**
```rust
Call { callee: Expression, args: Vec<Expression> }
```

**Characteristics:**
- Invokes functions or procedures
- May return values (expression context)
- May have side effects
- Arguments evaluated left-to-right

### 4. Conditional Statements

```mm
if x > 0 {
    print("positive");
} else {
    print("zero or negative");
}

if condition {
    action();
}  // No else clause
```

**Structure:**
```rust
If {
    condition: Expression,      // Boolean condition
    then_block: Block,         // Code to execute if true
    else_block: Option<Block>, // Optional else block
}
```

**Characteristics:**
- Conditional execution
- Condition must evaluate to boolean
- Supports nested if statements
- Optional else clause

### 5. Function Definition

```mm
function add(x: i64, y: i64) -> i64 {
    return x + y;
}

function greet(name: string) -> none {
    print("Hello, ", name);
}
```

**Structure:**
```rust
Function { name: String, ret_type: Type, params: Vec<Variable>, body: Block }
```

**Characteristics:**
- Defines reusable code blocks
- Parameters with explicit types
- Explicit return type
- Creates new scope for body
- Can be recursive

### 6. Block Statement

```mm
{
    let x = 5;
    let y = 10;
    print(x + y);
}  // Block creates new scope
```

**Structure:**
```rust
Block {
    body: Block,    // Nested block
}
```

**Characteristics:**
- Groups statements together
- Creates new variable scope
- Used for control flow structures
- Supports nesting

### 7. Return Statement

```mm
function factorial(n: i64) -> i64 {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}
```

**Structure:**
```rust
Return { value: Expression }
```

### 8. Class Declaration

```mm
class Point {
    public x: i32;
    public y: i32;

    init(x: i32, y: i32) { self.x = x; self.y = y; }
    public function get_x() -> i32 { return self.x; }
}
```

**Structure:**
```rust
Class { name: String, parent: Option<String>, fields: Vec<(Variable, Visibility)>, methods: Vec<(Box<Statement>, Visibility)> }
```

**Characteristics:**
- Exits function with value
- Must match function return type
- Can appear anywhere in function
- Terminates function execution

## Display Implementation

The module provides comprehensive string representation:

```rust
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Statement::VariableDecl { identifier, value } => {
                format!("VariableDecl({}, {})", identifier.name, value)
            }
            Statement::Assignment { identifier, value } => {
                format!("Assignment({}, {})", identifier, value)
            }
            Statement::Call { callee, args } => {
                format!("Call({}, [{}])",
                    callee,
                    args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join(", ")
                )
            }
            // ... other variants
        };
        write!(f, "{}", fmtstr)
    }
}
```

## Statement Examples

### Simple Statements
```mm
x = 42;                     // Assignment(Variable::<i64>(x), Number(42))
print("hello");             // Call(Variable(print), [StringLiteral(hello)])
return x + y;              // Return(BinaryOp(Variable(x) + Variable(y)))
```

### Complex Statements
```mm
if x > threshold {
    process_value(x);
    update_counter();
} else {
    log_error("Value too small");
}
```

### Function Definitions
```mm
function fibonacci(n: i64) -> i64 {
    if n <= 1 {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

## Control Flow

### Execution Order
1. **Sequential**: Statements execute in order
2. **Conditional**: If statements branch execution
3. **Function calls**: Transfer control temporarily
4. **Returns**: Exit function early

### Scope Management
- **Block statements**: Create new scopes
- **Function definitions**: Isolated parameter scopes
- **Variable declarations**: Add to current scope

## Usage in Parser

Statements are constructed during parsing:

```rust
// Parsing "x = 5;"
let identifier = Expression::Variable(Variable { name: "x", var_type: Type::I64 });
let value = Expression::Number(5);
let stmt = Statement::Assignment { identifier, value };
```

## Usage in Code Generation

Statements generate corresponding LLVM IR:

```rust
fn transform_statement(&mut self, stmt: Statement) -> Evaluation {
    match stmt {
        Statement::Assignment { identifier, value } => {
            let value_reg = self.transform_expression(value);
            if let Expression::Variable(var) = identifier {
                self.emit(&format!("store i64 %{}, i64* %{}", 
                    value_reg.to_string(), var.name));
            }
        }
        Statement::Call { callee, args } => {
            let arg_regs: Vec<_> = args.into_iter()
                .map(|arg| self.transform_expression(arg))
                .collect();
            // Generate call instruction
        }
        // ... other statement types
    }
}
```

## Semantic Analysis

Statements undergo semantic checking:

### Type Checking
- **Assignment compatibility**: Value type matches variable type
- **Function calls**: Argument types match parameters
- **Return statements**: Return type matches function signature

### Scope Validation
- **Variable access**: Variables exist in current scope
- **Function calls**: Functions are declared and accessible
- **Return placement**: Returns only in function contexts

## Error Handling

Statement validation catches various errors:

### Syntax Errors
```mm
x = ;                       // Missing value expression
if x > 0 print("yes");     // Missing braces
function() { }             // Missing name and parameters
```

### Semantic Errors
```mm
x = "hello";               // Type mismatch if x is integer
undefined_function();      // Function not declared
return x;                  // Return outside function
```

## Optimization Opportunities

### Dead Code Elimination
```mm
function test() -> none {
    print("before");
    return;
    print("unreachable");   // Dead code
}
```

### Constant Folding
```mm
x = 2 + 3;                 // Can be optimized to x = 5;
```

### Control Flow Optimization
```mm
if true {                  // Condition always true
    action();
} else {
    unreachable();         // Dead branch
}
```

## Future Enhancements

### Planned Statement Types
- **Loop statements**: `while`, `for`, `loop`
- **Match statements**: Pattern matching
- **Try-catch**: Error handling
- **Import statements**: Module system
- **Defer statements**: Cleanup actions

### Advanced Features
- **Statement expressions**: Statements that return values
- **Async statements**: Asynchronous execution
- **Parallel statements**: Concurrent execution
- **Compile-time statements**: Metaprogramming

## Testing

Statement module testing:

```rust
#[test]
fn test_assignment_display() {
    let stmt = Statement::Assignment {
        identifier: Expression::Variable(Variable { 
            name: "x".to_string(), 
            var_type: Type::I64 
        }),
        value: Expression::Number(42),
    };
    assert!(stmt.to_string().contains("Assignment"));
}

#[test]
fn test_function_definition() {
    let stmt = Statement::Function {
        name: "test".to_string(),
        ret_type: Type::Void,
        params: vec![],
        body: Block::new(vec![]),
    };
    // Verify function structure
}
```

## Integration with Other Modules

### With Expression Module
- Statements contain expressions as components
- Expression evaluation within statement context
- Type information flows between systems

### With Block Module
- Statements grouped into blocks
- Block scoping affects statement execution
- Nested statement structures

### With Type Module
- Type checking for statement validity
- Type inference for variable declarations
- Return type validation

## Performance Characteristics

- **Memory**: Efficient representation with Box for recursion
- **Cloning**: Supports AST manipulation and transformation
- **Execution**: Direct translation to efficient LLVM IR

The statement system provides the foundation for expressing program logic and control flow in MM-Lang with clear semantics and efficient execution.
