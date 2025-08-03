# Block System Module

[← Back to README](../README.md)

## Overview

The block module (`block.rs`) provides the fundamental structure for grouping statements and managing scope in MM-Lang. Blocks represent sequences of statements that execute together and create lexical scopes for variable declarations.

## Block Structure

### Core Block Definition

```rust
#[derive(Debug, Clone)]
pub struct Block {
    pub id: u64,
    pub statements: Vec<Statement>,
}
```

**Components:**
- **id**: Unique identifier for each block (debugging/optimization)
- **statements**: Ordered sequence of statements to execute

### Block ID Generation

```rust
static BLOCK_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

impl Block {
    pub fn new(statements: Vec<Statement>) -> Self {
        let id = BLOCK_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Block { id, statements }
    }
}
```

Each block receives a unique ID for:
- **Debugging**: Tracing execution flow
- **Optimization**: Identifying optimization opportunities
- **Code generation**: Creating unique LLVM basic blocks

## Block Types and Usage

### 1. Program Root Block

```mm
// Top-level program
x = 5;
y = 10;
print(x + y);
```

The root block contains all top-level statements and represents the main program execution.

### 2. Function Body Blocks

```mm
function calculate(a: i64, b: i64) -> i64 {
    // Function body block
    let result = a * b + 10;
    return result;
}
```

Function bodies are blocks that:
- Create new variable scopes
- Contain function-local statements
- Support return statements

### 3. Control Flow Blocks

```mm
if condition {
    // Then block
    process_true_case();
    update_state();
} else {
    // Else block
    handle_false_case();
}
```

Control flow constructs use blocks for:
- **Conditional execution**: If/else branches
- **Loop bodies**: While/for loop content (planned)
- **Exception handling**: Try/catch blocks (planned)

### 4. Explicit Scope Blocks

```mm
{
    // Explicit block for scoping
    let temp = expensive_calculation();
    process(temp);
    // temp goes out of scope here
}
```

Explicit blocks provide:
- **Variable scoping**: Limit variable lifetime
- **Resource management**: Cleanup at block end
- **Code organization**: Logical grouping

## Block Execution Model

### Sequential Execution

Statements within a block execute in order:

```mm
{
    statement1();  // Executes first
    statement2();  // Executes second
    statement3();  // Executes third
}
```

### Early Termination

Some statements can terminate block execution early:

```mm
function example() -> i64 {
    print("Starting");
    return 42;        // Block execution stops here
    print("Never executed");  // Dead code
}
```

### Exception Propagation (Planned)

```mm
{
    risky_operation();  // May throw exception
    normal_operation(); // Skipped if exception thrown
}
```

## Scope Management

### Variable Scope

Blocks create lexical scopes:

```mm
{
    let x = 10;           // x declared in this scope
    {
        let y = 20;       // y declared in inner scope
        print(x + y);     // Both x and y accessible
    }
    // y no longer accessible here
    print(x);             // x still accessible
}
// x no longer accessible here
```

### Nested Scopes

Inner blocks can access outer scope variables:

```mm
let global_var = 100;
{
    let local_var = 50;
    {
        let inner_var = 25;
        // Can access: inner_var, local_var, global_var
        result = global_var + local_var + inner_var;
    }
    // Can access: local_var, global_var
}
// Can access: global_var
```

### Shadow Variables

Inner scopes can shadow outer variables:

```mm
let x = 10;
{
    let x = 20;          // Shadows outer x
    print(x);            // Prints 20
}
print(x);                // Prints 10 (outer x restored)
```

## Display Implementation

```rust
impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = format!(
            "Block(id: {}, statements: [{}])",
            self.id,
            self.statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );
        write!(f, "{}", fmtstr)
    }
}
```

### Example Output

```
Block(id: 0, statements: [
    Assignment(Variable(x), Number(5)), 
    Assignment(Variable(y), Number(10)), 
    Call(Variable(print), [BinaryOp(Variable(x) + Variable(y))])
])
```

## Block in AST Construction

### Parser Integration

The parser creates blocks during parsing:

```rust
// Parsing a function body
pub fn parse(&mut self) -> Block {
    let mut statements = Vec::new();
    
    while let Some(token) = self.next_token() {
        match token {
            // Parse various statement types
            // Add to statements vector
        }
    }
    
    Block::new(statements)
}
```

### Nested Block Parsing

```rust
// Parsing if statement with blocks
let condition = self.parse_expr();
self.expect(&Token::Punctuation("{"));
let then_block = self.parse();  // Recursive block parsing
self.expect(&Token::Punctuation("}"));

let else_block = if self.peek_is_keyword("else") {
    self.expect_keyword("else");
    self.expect(&Token::Punctuation("{"));
    let block = self.parse();   // Another recursive parse
    self.expect(&Token::Punctuation("}"));
    Some(block)
} else {
    None
};
```

## LLVM Code Generation

### Basic Block Generation

Each MM-Lang block maps to LLVM basic blocks:

```rust
fn transform_block(&mut self, block: &Block) -> Evaluation {
    let mut eval = Evaluation::new();
    
    // Create LLVM basic block
    let block_label = format!("block{}", block.id);
    eval.prologue.push(format!("{}:", block_label));
    
    // Transform each statement
    for statement in &block.statements {
        let stmt_eval = self.transform_statement(statement);
        eval.merge(stmt_eval);
    }
    
    eval
}
```

### Control Flow Integration

```rust
// IF statement with blocks
fn transform_if(&mut self, condition: Expression, then_block: Block, else_block: Option<Block>) {
    let cond_reg = self.transform_expression(condition);
    
    let then_label = format!("then{}", then_block.id);
    let else_label = format!("else{}", else_block.as_ref().map_or(0, |b| b.id));
    let end_label = format!("end{}", self.next_label_id());
    
    // Branch instruction
    self.emit(&format!("br i1 %{}, label %{}, label %{}", 
        cond_reg.to_string(), then_label, else_label));
    
    // Then block
    self.transform_block(&then_block);
    self.emit(&format!("br label %{}", end_label));
    
    // Else block (if present)
    if let Some(else_block) = else_block {
        self.transform_block(&else_block);
        self.emit(&format!("br label %{}", end_label));
    }
    
    // Continuation
    self.emit(&format!("{}:", end_label));
}
```

## Block Optimization

### Dead Code Elimination

```mm
{
    print("reachable");
    return 42;
    print("unreachable");  // Can be eliminated
}
```

### Block Merging

```mm
{
    statement1();
}
{
    statement2();
}
// Can be merged into single block
```

### Constant Propagation

```mm
{
    let x = 5;
    let y = x + 10;    // y = 15 (constant)
    print(y);          // print(15)
}
```

## Error Handling

### Block Validation

- **Syntax errors**: Missing braces, malformed statements
- **Semantic errors**: Type mismatches, undefined variables
- **Control flow errors**: Invalid returns, unreachable code

### Error Context

Block IDs help provide context in error messages:

```
Error in block 3: Variable 'x' not found
At statement 2: Assignment(Variable(x), Number(42))
```

## Memory Management

### Cloning

Blocks support efficient cloning for AST manipulation:

```rust
let original_block = Block::new(statements);
let modified_block = original_block.clone();
// Safe to modify without affecting original
```

### Heap Allocation

Large statement vectors are heap-allocated, preventing stack overflow in deeply nested structures.

## Testing

### Block Construction Testing

```rust
#[test]
fn test_block_creation() {
    let statements = vec![
        Statement::Assignment { /* ... */ },
        Statement::Call { /* ... */ },
    ];
    
    let block = Block::new(statements);
    assert_eq!(block.statements.len(), 2);
    assert!(block.id > 0);  // Should have unique ID
}
```

### Block Display Testing

```rust
#[test]
fn test_block_display() {
    let block = Block::new(vec![]);
    let display = block.to_string();
    assert!(display.contains("Block(id:"));
    assert!(display.contains("statements: []"));
}
```

## Future Enhancements

### Planned Features

- **Block metadata**: Source location, optimization hints
- **Block types**: Different block kinds (function, loop, etc.)
- **Block analysis**: Control flow analysis, dataflow analysis
- **Block transformation**: Optimization passes, refactoring

### Advanced Optimizations

- **Block scheduling**: Reorder for performance
- **Block fusion**: Merge compatible blocks  
- **Block splitting**: Split for parallelization
- **Block caching**: Memoize expensive computations

## Integration Points

### With Parser
- Recursive block parsing for nested structures
- Block boundary detection and validation
- Error recovery within block context

### With Code Generator
- Block-to-basic-block mapping
- Control flow graph construction
- Register allocation across blocks

### With Optimizer
- Block-level optimization passes
- Inter-block analysis and transformation
- Dead block elimination

## Performance Characteristics

- **Creation**: O(1) with atomic ID generation
- **Access**: O(1) for statement access by index
- **Iteration**: O(n) for statement processing
- **Memory**: O(n) where n is number of statements

The block system provides a clean abstraction for grouping statements while maintaining efficient execution and clear semantics for scope management.
