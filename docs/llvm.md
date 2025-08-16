# LLVM Backend Module

[← Back to README](../README.md)

## Overview

The LLVM module (`llvm.rs`) handles the final stage of compilation, transforming the Abstract Syntax Tree into LLVM Intermediate Representation (IR) code. This module manages scope, registers, memory allocation, and code generation for efficient execution.

## Core Architecture

### LLVM Compiler Structure

```rust
pub struct LLVM {
    prologue: IR,           // Global declarations and setup
    main_prologue: IR,      // Main function setup
    main: IR,              // Main function body
    main_epilogue: IR,     // Main function cleanup
    code: IR,              // Additional functions
    epilogue: IR,          // Program cleanup
    ast: Ast,              // Source AST
    scope: Scope,          // Symbol table management
}
```

The compiler is organized into distinct sections for clean code generation and optimization opportunities.

## Intermediate Representation (IR)

### IR Structure

```rust
#[derive(Debug, Clone)]
pub struct IR {
    pub instructions: Vec<String>,
}

impl IR {
    pub fn new() -> Self {
        IR { instructions: Vec::new() }
    }

    pub fn push(&mut self, instruction: String) {
        self.instructions.push(instruction);
    }
}
```

IR provides a simple accumulator for LLVM instructions with efficient string building.

## Register Management

### Register System

```rust
static REGISTER_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Register {
    id: u64,
    llvm_type: String,
}

impl Register {
    pub fn new() -> Self {
        let id = REGISTER_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Register {
            id,
            llvm_type: "none".to_string(),
        }
    }
}

impl ToString for Register {
    fn to_string(&self) -> String {
        format!("reg{}", self.id)
    }
}
```

**Characteristics:**
- **Unique IDs**: Thread-safe counter ensures uniqueness
- **SSA Form**: Each register assigned exactly once
- **Type tracking**: Future enhancement for type-specific optimizations
- **Hash support**: Efficient symbol table operations

## Scope Management

### Symbol Table

```rust
#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub symbols: HashMap<Register, Variable>,
}

impl SymbolTable {
    pub fn insert(&mut self, register: Register, var_info: Variable) {
        self.symbols.insert(register, var_info);
    }
    
    pub fn lookup(&self, name: &str) -> Option<&Variable> {
        self.symbols.values().find(|var| var.name == name)
    }
}
```

### Scope Stack

```rust
#[derive(Debug)]
pub struct Scope {
    stack: Vec<SymbolTable>,
}

impl Scope {
    pub fn enter_scope(&mut self) {
        self.stack.push(SymbolTable { symbols: HashMap::new() });
    }

    pub fn exit_scope(&mut self) {
        self.stack.pop();
    }

    pub fn resolve(&self, register: &Register) -> Option<&Variable> {
        for scope in self.stack.iter().rev() {
            if let Some(info) = scope.symbols.get(register) {
                return Some(info);
            }
        }
        None
    }
}
```

**Features:**
- **Lexical scoping**: Inner scopes can access outer variables
- **Variable shadowing**: Inner declarations hide outer ones
- **Function scoping**: Separate scopes for function parameters
- **Global scope**: Top-level variable storage

## String Literal Management

### String Data Structure

```rust
static STRING_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone)]
pub struct StringData {
    id: u64,
    value: String,
}

impl StringData {
    pub fn new(value: String) -> Self {
        let id = STRING_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        StringData { id, value }
    }

    pub fn name(&self) -> String {
        format!("str{}", self.id)
    }

    pub fn length(&self) -> usize {
        self.value.len()
    }
}
```

String literals are converted to global constants for efficient memory usage and string interning.

## Evaluation System

### Evaluation Structure

```rust
#[derive(Debug, Clone)]
pub struct Evaluation {
    pub prologue: IR,    // Setup instructions
    pub epilogue: IR,    // Cleanup instructions  
    pub register: Register, // Result register
}
```

Evaluations encapsulate the result of transforming expressions and statements, including any setup or cleanup code needed.

## Code Generation Process

### Compilation Pipeline

```rust
impl LLVM {
    pub fn compile(&mut self) {
        // 1. Setup prologue
        self.prologue.push(format!("\n; PROLOGUE"));
        
        // 2. Main function setup
        self.main_prologue.push(format!("define i32 @main() {{"));
        self.main_prologue.push(format!("entry:"));

        // 3. Transform AST to IR
        let eval = self.transform();
        self.main.instructions.extend(eval.prologue.instructions);
        self.main.instructions.extend(eval.epilogue.instructions);

        // 4. Main function cleanup
        self.main_epilogue.push(format!("  ret i32 0"));
        self.main_epilogue.push(format!("}}"));
    }

    pub fn output(&self) -> String {
        let mut output = String::new();
        output.push_str(&self.prologue.to_string());
        output.push_str(&self.main_prologue.to_string());
        output.push_str(&self.main.to_string());
        output.push_str(&self.main_epilogue.to_string());
        output.push_str(&self.code.to_string());
        output.push_str(&self.epilogue.to_string());
        output
    }
}
```

### Expression Transformation

```rust
pub fn transform_expression(&mut self, expr: Expression) -> Evaluation {
    let mut eval = Evaluation::new();

    match expr {
        Expression::Number(value) => {
            let reg = Register::new();
            eval.prologue.push(format!("  %{} = add i64 0, {}", reg.to_string(), value));
            eval.register = reg;
        }
        
        Expression::Variable(var) => {
            let reg = Register::new();
            eval.prologue.push(format!("  %{} = load i64, i64* %{}", 
                reg.to_string(), var.name));
            eval.register = reg;
        }
        
        Expression::BinaryOp { op, left, right } => {
            let left_eval = self.transform_expression(*left);
            let right_eval = self.transform_expression(*right);
            
            eval.prologue.instructions.extend(left_eval.prologue.instructions);
            eval.prologue.instructions.extend(right_eval.prologue.instructions);
            
            let result_reg = Register::new();
            let llvm_op = match op.as_str() {
                "+" => "add",
                "-" => "sub",
                "*" => "mul",
                "/" => "sdiv",
                "==" => "icmp eq",
                "!=" => "icmp ne",
                "<" => "icmp slt",
                ">" => "icmp sgt",
                "<=" => "icmp sle",
                ">=" => "icmp sge",
                _ => panic!("Unsupported operator: {}", op),
            };
            
            eval.prologue.push(format!("  %{} = {} i64 %{}, %{}", 
                result_reg.to_string(), llvm_op, 
                left_eval.register.to_string(), right_eval.register.to_string()));
            eval.register = result_reg;
        }
        
        Expression::StringLiteral(value) => {
            let string_data = StringData::new(value.clone());
            let reg = Register::new();
            
            // Global string constant
            self.prologue.push(format!("@{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"", 
                string_data.name(), string_data.length() + 1, value));
                
            // Get pointer to string
            eval.prologue.push(format!("  %{} = getelementptr inbounds [{} x i8], [{} x i8]* @{}, i64 0, i64 0", 
                reg.to_string(), string_data.length() + 1, string_data.length() + 1, string_data.name()));
            eval.register = reg;
        }
        
        // ... other expression types
    }

    eval
}
```

### Statement Transformation

```rust
pub fn transform_block(&mut self, statements: &[Statement]) -> Evaluation {
    let mut eval = Evaluation::new();
    
    self.scope.enter_scope();

    for statement in statements {
        match statement {
            Statement::Assignment { identifier, value } => {
                if let Expression::Variable(var) = identifier {
                    let value_eval = self.transform_expression(value.clone());
                    eval.prologue.instructions.extend(value_eval.prologue.instructions);
                    
                    // Allocate variable storage
                    eval.prologue.push(format!("  %{} = alloca i64", var.name));
                    
                    // Store value
                    eval.prologue.push(format!("  store i64 %{}, i64* %{}", 
                        value_eval.register.to_string(), var.name));
                    
                    // Add to symbol table
                    let var_reg = Register::new();
                    self.scope.insert(var_reg, var.clone());
                }
            }
            
            Statement::Call { callee, args } => {
                if let Expression::Variable(func_var) = callee {
                    match func_var.name.as_str() {
                        "print" => {
                            for arg in args {
                                let arg_eval = self.transform_expression(arg.clone());
                                eval.prologue.instructions.extend(arg_eval.prologue.instructions);
                                
                                // Generate printf call (simplified)
                                eval.prologue.push(format!("  call i32 @printf(i8* %reg{})", 
                                    arg_eval.register.id));
                            }
                        }
                        _ => panic!("Unsupported function: {}", func_var.name),
                    }
                }
            }
            
            Statement::If { condition, then_block, else_block } => {
                let cond_eval = self.transform_expression(condition.clone());
                eval.prologue.instructions.extend(cond_eval.prologue.instructions);
                
                let then_label = format!("then{}", Register::new().id);
                let else_label = format!("else{}", Register::new().id);
                let end_label = format!("end{}", Register::new().id);
                
                // Branch instruction
                if else_block.is_some() {
                    eval.prologue.push(format!("  br i1 %{}, label %{}, label %{}", 
                        cond_eval.register.to_string(), then_label, else_label));
                } else {
                    eval.prologue.push(format!("  br i1 %{}, label %{}, label %{}", 
                        cond_eval.register.to_string(), then_label, end_label));
                }
                
                // Then block
                eval.prologue.push(format!("{}:", then_label));
                let then_eval = self.transform_block(&then_block.statements);
                eval.prologue.instructions.extend(then_eval.prologue.instructions);
                eval.prologue.push(format!("  br label %{}", end_label));
                
                // Else block
                if let Some(else_block) = else_block {
                    eval.prologue.push(format!("{}:", else_label));
                    let else_eval = self.transform_block(&else_block.statements);
                    eval.prologue.instructions.extend(else_eval.prologue.instructions);
                    eval.prologue.push(format!("  br label %{}", end_label));
                }
                
                // End label
                eval.prologue.push(format!("{}:", end_label));
            }
            
            // ... other statement types
        }
    }

    self.scope.exit_scope();
    eval
}
```

## Type System Integration

### Type to LLVM Mapping

```rust
fn type_to_llvm(&self, t: &Type) -> String {
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
        Type::String => "i8*".to_string(),
        Type::Void => "void".to_string(),
        Type::Function(params, ret) => {
            let param_types = params.iter()
                .map(|t| self.type_to_llvm(t))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{} ({})*", self.type_to_llvm(ret), param_types)
        }
        Type::Array(elem_type) => format!("{}*", self.type_to_llvm(elem_type)),
        Type::UserDefined(_) => "i8*".to_string(),
        Type::ToBeEvaluated => "i8*".to_string(),
    }
}
```

## Generated LLVM IR Examples

### Simple Assignment

**MM-Lang Code:**
```mm
x = 42;
```

**Generated LLVM IR:**
```llvm
  %reg0 = add i64 0, 42
  %x = alloca i64
  store i64 %reg0, i64* %x
```

### Arithmetic Expression

**MM-Lang Code:**
```mm
result = x + y * 2;
```

**Generated LLVM IR:**
```llvm
  %reg0 = load i64, i64* %x
  %reg1 = add i64 0, 2
  %reg2 = mul i64 %y, %reg1
  %reg3 = add i64 %reg0, %reg2
  %result = alloca i64
  store i64 %reg3, i64* %result
```

### Conditional Statement

**MM-Lang Code:**
```mm
if x > 5 {
    print("large");
} else {
    print("small");
}
```

**Generated LLVM IR:**
```llvm
  %reg0 = load i64, i64* %x
  %reg1 = add i64 0, 5
  %reg2 = icmp sgt i64 %reg0, %reg1
  br i1 %reg2, label %then3, label %else4

then3:
  %reg5 = getelementptr inbounds [6 x i8], [6 x i8]* @str0, i64 0, i64 0
  call i32 @printf(i8* %reg5)
  br label %end6

else4:
  %reg7 = getelementptr inbounds [6 x i8], [6 x i8]* @str1, i64 0, i64 0
  call i32 @printf(i8* %reg7)
  br label %end6

end6:
```

## Optimization Opportunities

### Dead Code Elimination

Remove unreachable code after return statements.

### Constant Folding

Evaluate constant expressions at compile time:
```mm
x = 2 + 3;  // Becomes: x = 5;
```

### Register Allocation

Optimize register usage and minimize memory operations.

### Control Flow Optimization

Eliminate redundant branches and merge basic blocks.

## Error Handling

### Code Generation Errors

```rust
pub enum CodeGenError {
    UndefinedVariable(String),
    UnsupportedOperation(String),
    InvalidFunction(String),
    TypeMismatch { expected: String, found: String },
}
```

### Error Recovery

The code generator provides helpful error messages with context:
- Variable names and line numbers
- Expected vs actual types
- Suggested fixes

## Future Enhancements

### Advanced Features

- **Function definitions**: User-defined functions
- **Array operations**: Indexing and manipulation
- **Memory management**: Automatic cleanup
- **Optimization passes**: Dead code elimination, constant propagation
- **Debug information**: Line number preservation
- **Exception handling**: Error propagation

### Performance Optimizations

- **Register pressure**: Minimize memory operations
- **Instruction scheduling**: Optimize for target architecture
- **Loop optimization**: Unrolling and vectorization
- **Inlining**: Function call elimination

## Testing

### Code Generation Testing

```rust
#[test]
fn test_simple_assignment() {
    let source = "x = 42;";
    let tokens = tokenize(source);
    let mut ast = Ast::new(tokens);
    let block = ast.parse();
    let mut llvm = LLVM::new(ast);
    llvm.compile();
    
    let output = llvm.output();
    assert!(output.contains("add i64 0, 42"));
    assert!(output.contains("store i64"));
}
```

### Integration Testing

Full pipeline testing from source to LLVM IR to verify correctness.

## Integration Points

### With Parser
- AST consumption and traversal
- Statement and expression processing
- Error propagation from parsing

### With Type System
- Type validation during code generation
- LLVM type mapping
- Type-specific instruction generation

### With Optimizer (Planned)
- Optimization pass framework
- IR transformation and analysis
- Performance measurement and tuning

## Performance Characteristics

- **Compilation speed**: Linear in AST size
- **Memory usage**: Proportional to program size
- **Generated code quality**: Efficient LLVM IR
- **Scalability**: Handles large programs efficiently

The LLVM backend provides a solid foundation for generating efficient machine code while maintaining clear code organization and extensibility for future enhancements.

## Current implementation notes

- The LLVM struct for classes stores a vtable pointer at index 0, followed by fields in declaration order; field access uses GEP with field_index + 1.
- Dynamic dispatch is implemented via per-class read-only VTable globals. Method calls load the vtable from the object, index the function pointer, and call with `self` as the first parameter.
- The backend injects C bindings in the prologue and the scope: `printf`, `scanf`, `malloc`, `free`.
- Registers are typed; conversions are inserted automatically (sext/trunc/zext) for integer widening/narrowing and via casts for explicit `as` conversions.
- Strings are emitted as private unnamed_addr constants and addressed with `getelementptr` to produce a `ptr` for interop.
