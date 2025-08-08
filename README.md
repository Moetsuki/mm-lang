# MM-Lang Programming Language

A custom programming language compiler written in Rust that lowers to LLVM IR.

## Overview

MM-Lang is a small, statically‑typed, expression–oriented language with C / Rust inspired surface syntax. The current prototype supports:

- Explicit variable declarations with type annotations
- First‑class (top level) functions with parameters & return types
- Blocks, if / else control flow, return
- Arithmetic, comparison, unary `-` and logical negation `!`
- Implicit numeric coercions (widening) & explicit casts with `as`
- Strings (lowered to C strings) and interop with selected C functions (`printf`, `scanf`, `malloc`, `free`)
- Single inheritance classes with fields, visibility modifiers, methods & simple virtual table layout work‑in‑progress
- Method calls (`object.method()`)
- Basic constructor syntax via an `init { ... }` block (prototype)

> NOTE: The class / vtable system is experimental and still evolving. Many semantic checks (visibility enforcement, overriding validation, etc.) are not yet implemented.

## Core Features

### Types
Primitive & built‑in types (subset implemented):
`bool`, `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `string`.

### Variable Declarations / Assignment
```mm
x: i64 = 5;
y: i64 = 10;
x = y + 2;
```
Variables must be declared before use. Re‑assignment omits the type.

### Functions
```mm
function add(a: i64, b: i64) -> i64 {
    return a + b;
}

result: i64 = add(10, 32);
```
All non‑`void` / `NoneType` functions must end with a `return` statement (enforced by the compiler).

### Control Flow
```mm
x: i64 = 5;
if x > 10 {
    y: i64 = 20;
} else {
    y: i64 = 30;
}
```
Nested blocks are allowed. Each block introduces a new scope.

### Expressions & Operators
Arithmetic: `+ - * / %`

Comparison: `== != < > <= >=` (lowered currently to integer comparisons; equality for strings not yet implemented).

Unary: `-expr`, `!expr` (logical not on booleans / truthy integer values).

### Type Coercion & Casting
```mm
x: i32 = 5;
y: i8 = 10;
z: i64 = x + y;        // implicit widening

n: i64 = 100;
m: i32 = n as i32;     // explicit narrowing cast
```
Implicit coercion only widens (never loses precision). Explicit `as` is required to narrow.

### Strings & C Interop
String literals are lowered to private constant null‑terminated byte arrays. Example using `printf`:
```mm
msg: string = "Hello, World!";
printf(msg);
```
`printf` is declared automatically with a variadic signature. Additional C bindings: `scanf`, `malloc`, `free`.

## Classes (Experimental)
Single inheritance with visibility modifiers and methods.

```mm
class Entity {
    public name: string;
    protected id: u64;

    // Constructor (prototype syntax)
    init {
        self.name = "Unnamed";
    }

    public function name() -> string {
        return self.name;
    }
};

class Animal : Entity {
    private name: string;     // hides Entity.name (allowed currently)
    private age: i32;
    protected species: string;

    public function speak() -> string {
        return "Animal sound";
    }
};

class Dog : Animal {
    public breed: string;

    public function speak() -> string { // override
        return "Woof!";
    }
};

class Cat : Animal {
    public color: string;

    public function speak() -> string { // override
        return "Meow!";
    }
};
```

### Method Calls
```mm
ent: Entity = Entity();
ent.name = "Test Entity";   // field write
result: string = ent.name(); // method call
```
Method lookup & vtable dispatch are in progress; current lowering treats methods similarly to functions with an explicit `self` pointer concept under development.

### Visibility
`public`, `private`, `protected` can prefix field or method declarations. Enforcement is not yet implemented in the semantic layer; they are recorded in the AST for future use.

### Constructor (`init`)
An `init { ... }` block inside a class is planned to lower to a synthesized function named `__<ClassName>_init`. The LLVM backend currently expects constructors / destructors with those synthesized names; the parser work for automatic conversion is ongoing.

## Example Program
```mm
function max(a: i64, b: i64) -> i64 {
    if a > b { return a; } else { return b; }
}

class Entity { public name: string; public function name() -> string { return self.name; } };

x: i32 = 5;
y: i8 = 10;
wide: i64 = x + y;
msg: string = "Hi";
printf(msg);
```

## Updated Grammar (Provisional)
EBNF sketch reflecting implemented & in‑progress constructs:
```ebnf
program          = { statement } ;

statement        = variable_decl
                 | assignment
                 | function_def
                 | class_decl
                 | if_statement
                 | return_statement
                 | expression_stmt
                 | block ;

block            = "{" { statement } "}" ;

variable_decl    = identifier ":" type "=" expression ";" ;
assignment       = expression "=" expression ";" ;          (* lhs currently must be a variable *)
return_statement = "return" expression ";" ;
expression_stmt  = expression ";" ;                           (* e.g. function / method call *)

function_def     = "function" identifier "(" [ param_list ] ")" "->" type block ;
param_list       = param { "," param } ;
param            = identifier ":" type ;

class_decl       = "class" identifier [ ":" identifier ] "{" { class_member } "}" ";" ;
class_member     = visibility field_decl
                 | visibility method_def
                 | init_block ;

visibility       = "public" | "private" | "protected" ;
field_decl       = identifier ":" type ";" ;
method_def       = "function" identifier "(" [ param_list ] ")" "->" type block ;
init_block       = "init" block ;                            (* prototype *)

if_statement     = "if" expression block [ "else" block ] ;

expression       = method_call
                 | call
                 | binary ;

method_call      = primary "." identifier "(" [ arg_list ] ")" ;
call             = primary "(" [ arg_list ] ")" ;
arg_list         = expression { "," expression } ;

binary           = unary { bin_op unary } ;
unary            = [ ("-" | "!") ] primary ;
primary          = number
                 | string_literal
                 | identifier
                 | cast
                 | "(" expression ")" ;
cast             = primary "as" type ;

bin_op           = "+" | "-" | "*" | "/" | "%" |
                   "==" | "!=" | "<" | ">" | "<=" | ">=" ;

type             = "bool" | "i8" | "i16" | "i32" | "i64" |
                   "u8" | "u16" | "u32" | "u64" |
                   "f32" | "f64" | "string" |
                   identifier ;          (* future: arrays / generics *)
```
Items marked prototype may differ from actual parser behavior as features stabilize.

## Project Structure
```
mm-lang/
├── src/
│   ├── main.rs          # Entry point & sample tests
│   ├── tokenizer.rs     # Lexical analysis
│   ├── ast.rs           # AST construction
│   ├── expression.rs    # Expressions
│   ├── statement.rs     # Statements & class/visibility enums
│   ├── block.rs         # Block container
│   ├── variable.rs      # Variable representation
│   ├── types.rs         # Type enum / helpers
│   └── llvm.rs          # LLVM IR generation backend
├── docs/                # Design notes
├── Cargo.toml
└── README.md
```

## Building
```bash
git clone https://github.com/Moetsuki/mm-lang.git
cd mm-lang
cargo build
```

## Running
```bash
cargo run
```

## Testing
```bash
cargo test
```

## Roadmap
- [x] Basic arithmetic & control flow
- [x] Functions
- [x] Strings & printf binding
- [x] Implicit widening & explicit casting
- [x] Unary operators
- [x] Class AST & preliminary LLVM struct + vtable scaffolding
- [ ] Complete method lowering with self parameter
- [ ] Proper vtable inheritance / overriding
- [ ] Visibility enforcement
- [ ] Constructors / destructors end‑to‑end
- [ ] Arrays & heap allocation helpers
- [ ] Pattern matching
- [ ] Generics / parametric polymorphism
- [ ] Modules & imports

## License

This project is licensed under the Apache 2.0 License - see the [LICENSE](LICENSE) file for details.

## Architecture

The compiler follows a traditional multi-stage architecture:

1. **Lexical Analysis** (`tokenizer.rs`) - Converts source code into tokens
2. **Syntax Analysis** (`ast.rs`) - Builds an Abstract Syntax Tree
3. **Semantic Analysis** - Type checking and symbol resolution
4. **Code Generation** (`llvm.rs`) - Generates LLVM IR
5. **Optimization** - LLVM optimizations (handled by LLVM)
6. **Code Emission** - Final machine code (handled by LLVM)

## Future Roadmap

- [x] Type casting with `as` keyword
- [x] Implicit type coercion for compatible types
- [ ] Advanced type system with generics
- [ ] Module system and imports
- [ ] Standard library
- [x] Print statements and I/O operations
- [ ] Pattern matching
- [ ] Closures and higher-order functions
- [ ] Compile-time computation
- [ ] Concurrency primitives

## Contact

- **Author**: Moetsuki
- **Repository**: [https://github.com/Moetsuki/mm-lang](https://github.com/Moetsuki/mm-lang)
- **Issues**: [https://github.com/Moetsuki/mm-lang/issues](https://github.com/Moetsuki/mm-lang/issues)
