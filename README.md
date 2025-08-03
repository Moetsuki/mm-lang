# MM-Lang Programming Language

A custom programming language compiler written in Rust that compiles to LLVM IR.

## Overview

MM-Lang is a statically-typed programming language with C-like syntax that compiles directly to LLVM IR. The language supports variables, functions, control flow, and basic arithmetic operations.

## Features

- **Static Typing**: Variables and functions must be explicitly typed
- **LLVM Backend**: Compiles to efficient LLVM IR
- **Control Flow**: Support for if-else statements and function calls
- **Type System**: Built-in types including integers, floats, booleans, and strings
- **Function Definitions**: First-class functions with parameters and return types

## Language Syntax

### Variable Assignments
```mm
x = 5;
y = 10;
z = x + y;
```

### Function Definitions
```mm
function add(x: i64, y: i64) -> i64 {
    return x + y;
}
```

### Control Flow
```mm
if x > 5 {
    print("x is greater than 5");
} else {
    print("x is 5 or less");
}
```

### Function Calls
```mm
result = add(10, 20);
print("Result is: ", result);
```

## Project Structure

```
mm-lang/
├── src/
│   ├── main.rs           # Entry point and test runner
│   ├── tokenizer.rs      # Lexical analysis
│   ├── ast.rs           # Abstract Syntax Tree parser
│   ├── expression.rs    # Expression definitions
│   ├── statement.rs     # Statement definitions
│   ├── block.rs         # Block structure
│   ├── variable.rs      # Variable definitions
│   ├── types.rs         # Type system
│   └── llvm.rs          # LLVM IR generation
├── Cargo.toml
└── README.md
```

## Module Documentation

- [**Tokenizer**](docs/tokenizer.md) - Lexical analysis and token generation
- [**AST Parser**](docs/ast.md) - Abstract Syntax Tree construction
- [**Expression System**](docs/expression.md) - Expression evaluation and types
- [**Statement System**](docs/statement.md) - Statement definitions and execution
- [**Block System**](docs/block.md) - Code block management
- [**Variable System**](docs/variable.md) - Variable declarations and management
- [**Type System**](docs/types.md) - Type definitions and checking
- [**LLVM Backend**](docs/llvm.md) - LLVM IR generation and compilation

## Getting Started

### Prerequisites

- Rust 1.70 or later
- Cargo package manager

### Building

```bash
git clone https://github.com/Moetsuki/mm-lang.git
cd mm-lang
cargo build
```

### Running

```bash
cargo run
```

### Testing

```bash
cargo test
```

## Example Programs

### Simple Arithmetic
```mm
x = 5;
y = 10;
result = x + y * 2;
print("Result: ", result);
```

### Function with Conditionals
```mm
function max(a: i64, b: i64) -> i64 {
    if a > b {
        return a;
    } else {
        return b;
    }
}

result = max(10, 20);
print("Maximum: ", result);
```

### Nested Control Flow
```mm
function categorize(value: i64) -> string {
    if value > 100 {
        return "large";
    } else {
        if value > 10 {
            return "medium";
        } else {
            return "small";
        }
    }
}
```

## Supported Types

- `bool` - Boolean values (true/false)
- `i8`, `i16`, `i32`, `i64` - Signed integers
- `u8`, `u16`, `u32`, `u64` - Unsigned integers
- `f32`, `f64` - Floating-point numbers
- `string` - String literals
- `array<T>` - Arrays of type T
- Custom user-defined types

## Operators

### Arithmetic
- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `/` - Division
- `%` - Modulo

### Comparison
- `==` - Equality
- `!=` - Inequality
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal
- `>=` - Greater than or equal

### Assignment
- `=` - Assignment

## Grammar (EBNF)

```ebnf
program = { statement } ;

statement = assignment
          | function_def
          | if_statement
          | call_statement
          | return_statement ;

assignment = identifier "=" expression ";" ;

function_def = "function" identifier "(" [ parameter_list ] ")" "->" type "{" { statement } "}" ;

parameter_list = parameter { "," parameter } ;
parameter = identifier ":" type ;

if_statement = "if" expression "{" { statement } "}" [ "else" "{" { statement } "}" ] ;

call_statement = expression "(" [ argument_list ] ")" ";" ;

return_statement = "return" expression ";" ;

expression = term { ( "+" | "-" | "==" | "!=" | "<" | ">" | "<=" | ">=" ) term } ;

term = factor { ( "*" | "/" | "%" ) factor } ;

factor = number
       | string_literal
       | identifier
       | "(" expression ")" ;

type = "bool" | "i8" | "i16" | "i32" | "i64" 
     | "u8" | "u16" | "u32" | "u64" 
     | "f32" | "f64" | "string" 
     | "array" "<" type ">" 
     | identifier ;
```

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Architecture

The compiler follows a traditional multi-stage architecture:

1. **Lexical Analysis** (`tokenizer.rs`) - Converts source code into tokens
2. **Syntax Analysis** (`ast.rs`) - Builds an Abstract Syntax Tree
3. **Semantic Analysis** - Type checking and symbol resolution
4. **Code Generation** (`llvm.rs`) - Generates LLVM IR
5. **Optimization** - LLVM optimizations (handled by LLVM)
6. **Code Emission** - Final machine code (handled by LLVM)

## Future Roadmap

- [ ] Advanced type system with generics
- [ ] Module system and imports
- [ ] Standard library
- [ ] Garbage collection
- [ ] Pattern matching
- [ ] Closures and higher-order functions
- [ ] Compile-time computation
- [ ] Error handling with exceptions
- [ ] Concurrency primitives

## Contact

- **Author**: Moetsuki
- **Repository**: [https://github.com/Moetsuki/mm-lang](https://github.com/Moetsuki/mm-lang)
- **Issues**: [https://github.com/Moetsuki/mm-lang/issues](https://github.com/Moetsuki/mm-lang/issues)
