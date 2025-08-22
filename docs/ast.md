# AST Parser Module

[← Back to README](../README.md)

## Overview

The AST (Abstract Syntax Tree) parser module (`ast.rs`) is responsible for converting a stream of tokens into a structured tree representation of the program. This module implements recursive descent parsing and builds the syntax tree that will be used for semantic analysis and code generation.

## Key Components

### Ast Structure

```rust
pub struct Ast {
    tokens: Vec<LexicalToken>,
    pos: usize,
    tree: Option<Block>,
    source_file: SourceFile,
    class_names: HashSet<String>,
    struct_names: HashSet<String>,
}
```

The parser maintains:
- **tokens**: Input token stream from the tokenizer
- **pos**: Current position in the token stream  
- **tree**: The resulting parsed syntax tree

## Core Methods

### Navigation Methods

#### `next_token(&mut self) -> Option<&LexicalToken>`
Consumes and returns the next token, advancing the parser position.

#### `peek_token(&self) -> Option<&LexicalToken>`
Returns the next token without consuming it, useful for lookahead.

#### `expect(&mut self, expected: &Token)`
Consumes the next token and panics if it doesn't match the expected token type. Used for mandatory syntax elements.

#### `expect_operator(&mut self)`
Specialized version of `expect` for operators.

### Core Parsing Methods

#### `parse(&mut self) -> Block`
Main parsing entry point that processes the entire token stream and returns the root Block.

**Process:**
1. Initialize statement list
2. Process tokens until end of stream
3. Match token types and delegate to specialized parsers
4. Return constructed Block

#### `parse_expr(&mut self) -> Expression`
Parses expressions using operator precedence rules.

Expressions use precedence climbing from lowest to highest:
1. Logical OR (`||`)
2. Logical AND (`&&`)
3. Comparisons (`==`, `!=`, `<`, `>`, `<=`, `>=`)
4. Additive (`+`, `-`)
5. Term (`*`, `/`)
6. Cast (`as`)
7. Unary (`-`, `!`)
8. Postfix (indexing, calls, field/method access)
9. Factor/primary

#### `parse_term(&mut self) -> Expression`
Handles higher precedence operations (multiplication, division).

#### `parse_factor(&mut self) -> Expression`
Handles the highest precedence elements (literals and identifiers) and now also supports struct literals when an `Identifier` is immediately followed by `{` and the identifier is a known struct name.

## Statement Parsing

### Variable Assignment
```mm
x = 5;
identifier = expression;
```

**Process:**
1. Identify the variable name
2. Expect assignment operator (`=`)
3. Parse the value expression
4. Create Assignment statement

### Function Definitions
```mm
function name(param1: type1, param2: type2) -> return_type {
    // body
}
```

**Process:**
1. Consume `function` keyword
2. Parse function name
3. Parse parameter list with types
4. Parse return type (after `->`)
5. Parse function body block
6. Create Function statement

### If Statements
```mm
if condition {
    // then block
} else {
    // else block (optional)
}
```

**Process:**
1. Consume `if` keyword
2. Parse condition expression
3. Parse then block
4. Check for optional `else` clause
5. Parse else block if present
6. Create If statement

### Function Calls
### Struct Declarations
```mm
struct Point {
    x: i32,
    y: i32,
}
```

Characteristics:
- Accepts `,` or `;` separators, trailing comma allowed
- Optional trailing semicolon after the closing `}`

### Struct Literals
```mm
Point { x: 1, y: 2 }
```

Disambiguation: the parser records struct names when encountered so that `Identifier {` is only parsed as a struct literal if the identifier is a known struct name (avoids mis-parsing `if x < y {` as a struct literal after `y`).
```mm
function_name(arg1, arg2, arg3);
```

**Process:**
1. Identify function name
2. Parse argument list
3. Create Call statement

## Expression Parsing

The parser uses recursive descent with operator precedence to handle complex expressions:

### Binary and Logical Operations
```mm
x + y * z    // Parsed as: x + (y * z)
a == b + c   // Parsed as: a == (b + c)
```

### Parentheses
```mm
(x + y) * z  // Explicit precedence override
```

### Variable References
```mm
variable_name  // References to previously declared variables
```

## Type Parsing

#### `parse_type(&mut self) -> Type`
Parses type annotations used in function parameters and return types.

**Supported types:**
- Built-in types: `bool`, `i8/i16/i32/i64`, `u8/u16/u32/u64`, `f32`, `f64`, `string`, `none`
- Composite: `tensor[T]`, `class`, `struct`
- User-defined names resolve to class/struct in later stages; unknown types error

## Error Handling

The parser provides detailed error messages with position information:

```rust
panic!(
    "Expected {:?}, found {:?} at line {}, column {}",
    expected, lexical_token.token, lexical_token.line, lexical_token.column
);
```

**Error scenarios:**
- Unexpected tokens
- Missing required syntax elements
- Invalid expressions
- Malformed function definitions

## Grammar Rules

The parser implements this grammar (simplified):

```ebnf
program = { statement }

statement = assignment | function_def | if_statement | call_statement | return_statement

assignment = identifier "=" expression ";"

function_def = "function" identifier "(" parameter_list ")" "->" type "{" block "}"

parameter_list = [ parameter { "," parameter } ]
parameter = identifier ":" type

if_statement = "if" expression "{" block "}" [ "else" "{" block "}" ]

call_statement = identifier "(" argument_list ")" ";"

expression = term { ("==" | "!=" | "<" | ">" | "<=" | ">=") term }

term = factor { ("+" | "-") factor }

factor = mult_factor { ("*" | "/") mult_factor }

mult_factor = number | string | identifier | "(" expression ")"
```

## Example Parsing

### Input Code
```mm
function factorial(n: i64) -> i64 {
    if n <= 1 {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}
```

### Resulting AST Structure
```
Block {
    statements: [
        Function {
            name: "factorial",
            params: [Variable { name: "n", type: I64 }],
            body: Block {
                statements: [
                    If {
                        condition: BinaryOp { op: "<=", left: Variable("n"), right: Number(1) },
                        then_block: Block {
                            statements: [Return { value: Number(1) }]
                        },
                        else_block: Some(Block {
                            statements: [
                                Return {
                                    value: BinaryOp {
                                        op: "*",
                                        left: Variable("n"),
                                        right: Call {
                                            callee: Variable("factorial"),
                                            args: [BinaryOp { op: "-", left: Variable("n"), right: Number(1) }]
                                        }
                                    }
                                }
                            ]
                        })
                    }
                ]
            }
        }
    ]
}
```

## Usage

```rust
use ast::Ast;
use tokenizer::tokenize;

let source = "x = 5 + 10;";
let tokens = tokenize(source);
let mut ast = Ast::new(tokens);
let syntax_tree = ast.parse();
```

## Block Handling

The parser properly handles nested blocks with automatic scope management:

- **Function bodies**: Create new scopes
- **If/else blocks**: Create nested scopes
- **Explicit blocks**: Support for `{}` grouped statements

## Lookahead Strategy

The parser uses single-token lookahead to make parsing decisions:

```rust
if let Some(next_token) = self.peek_token() {
    match &next_token.token {
        Token::Operator(_) => // Handle assignment
        Token::Punctuation("(") => // Handle function call
        // ... other cases
    }
}
```

## Future Enhancements

- **Error recovery**: Continue parsing after errors
- **Better error messages**: More context-aware error reporting
- **Operator associativity**: Left/right associative operators
- **More expression types**: Array access, member access
- **Macro support**: Preprocessor-style macros
- **Generic parsing**: Template/generic function support

## Testing

The parser can be tested with various code constructs:

```rust
#[test]
fn test_function_parsing() {
    let source = "function add(x: i64, y: i64) -> i64 { return x + y; }";
    let tokens = tokenize(source);
    let mut ast = Ast::new(tokens);
    let tree = ast.parse();
    // Verify tree structure
}
```

## Performance Characteristics

- **Time Complexity**: O(n) where n is the number of tokens
- **Space Complexity**: O(d) where d is the maximum nesting depth
- **Memory Usage**: Proportional to AST size

The recursive descent approach provides predictable performance and clear code structure.
