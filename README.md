# MM‑Lang Programming Language

A small, statically typed language compiler written in Rust that lowers to LLVM IR with executable output via Clang.

## Overview

MM‑Lang has a C/Rust‑inspired surface syntax. Core features work end‑to‑end (tokenize → parse → codegen → link → run).

Test status: 27 passed, 0 failed (via `cargo test -q`).

### Fully working (tested)
- Variable declarations with explicit types and assignments
- Arithmetic and comparisons; unary `-` and logical negation `!`
- Implicit integer widening and explicit casts using `as`
- Blocks and if/else control flow with proper scoping
- String literals (lowered to private constant C strings)
- C interop: `printf`, `scanf`, `malloc`, `free`
- Functions: definitions, calls, return values
- Classes (single inheritance):
    - Fields with visibility markers (public/private/protected) [parsing and layout]
    - Constructors via `init(...) { ... }`
    - Field access (`obj.field`) and assignment
    - Method calls with dynamic dispatch via per‑class vtables
- Tensors (contiguous buffers):
    - Declaration syntax: `arr: tensor[i64] = {1, 2, 3};`
    - Element indexing and assignment: `arr[1]`, `arr[0] = 42;`
    - Lowered to stack allocations with `getelementptr` for element access
- Else‑if chains: `if ... else if ... else ...`

### Known limitations (current behavior)
- Visibility keywords are parsed and preserved in types, but enforcement is not performed yet.
- Numeric literals are integers; float literals aren’t tokenized yet (float types exist for future use). Boolean literals (`true`/`false`) are not tokenized yet, but boolean results are produced by comparisons.
- Modules/imports, pattern matching, and generics are not implemented.
- Error reporting is panic‑driven and aimed at development use.

## Core Features

### Types
Primitive/built‑in:
`bool`, `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `string`, `none`.

Composite and compiler types used in codegen:
- `function name(args...) -> ret` (first‑class function type)
- `class Name [: Parent] { fields, methods }` (lowered to `%Name` with `%NameVTable`)
- `tensor[T]` contiguous buffer (1‑D today)
- `ptr<T>` pointers (used internally in codegen and interop)

### Variable Declarations / Assignment ✅
```mm
x: i64 = 5;
y: i64 = 10;
x = y + 2;
```
Variables must be declared before use. Re‑assignment omits the type.

### Functions ✅
```mm
function add(a: i64, b: i64) -> i64 {
    return a + b;
}

result: i64 = add(10, 32);
```
Function definitions and basic function calls are working. Return statements are supported.

### Control Flow ✅
```mm
x: i64 = 5;
if x > 10 {
    y: i64 = 20;
} else {
    y: i64 = 30;
}
```
Nested blocks are allowed. Each block introduces a new scope.

### Expressions & Operators ✅
Arithmetic: `+ - * / %`

Comparison: `== != < > <= >=` (lowered to integer comparisons).

Unary: `-expr`, `!expr` (logical not; non‑bools are compared against zero).

### Type Coercion & Casting ✅
```mm
x: i32 = 5;
y: i8 = 10;
z: i64 = x + y;        // implicit widening

n: i64 = 100;
m: i32 = n as i32;     // explicit narrowing cast
```
Implicit coercion only widens (never loses precision). Explicit `as` is required to narrow.

### Strings & C Interop ✅
String literals are lowered to private constant null‑terminated byte arrays. Example using `printf`:
```mm
msg: string = "Hello, World!";
printf(msg);
```
`printf` is declared automatically with a variadic signature. Additional C bindings provided: `scanf`, `malloc`, `free`.

### Tensors ✅
Contiguous buffers with initializer lists and indexing.

```mm
arr: tensor[i64] = {11, 22, 33};
sum: i64 = arr[0] + arr[1] + arr[2];
arr[1] = 44;
```
Lowered to stack allocations with per‑element stores/loads using `getelementptr`.

## Classes ✅
Single inheritance with fields, methods, and constructors. Dynamic dispatch is implemented via per‑class VTables.

```mm
class Entity {
    public name: string;
    protected id: u64;

    // Constructor (prototype syntax)
    init(new_id: u64) {
        self.id = new_id;
    }

    public function get_name() -> string {
        return self.name;
    }
};

class Animal : Entity {
    private age: i32;
    protected species: string;

    public function speak() -> string {
        return "Animal sound";
    }
};
```

### Current class support:
- ✅ Declarations with single inheritance
- ✅ Field declarations with visibility markers (not enforced yet)
- ✅ Methods and `init` constructors (called via `ClassName(...)`)
- ✅ Field access (`obj.field`) and assignment
- ✅ Method calls with dynamic dispatch through VTables
- ⚠️ No visibility enforcement; no method overloading

## Example Program (Working Features)
```mm
// Variable declarations and type coercion
x: i32 = 5;
y: i8 = 10;
wide: i64 = x + y;  // implicit widening

// Type casting
narrow: i32 = wide as i32;

// Unary operations
negative: i64 = -wide;

// Control flow
if x > 3 {
    msg: string = "Greater than 3";
    printf(msg);
} else {
    msg: string = "Not greater than 3";  
    printf(msg);
}
```

## Grammar (implemented subset)
EBNF sketch reflecting constructs implemented today.

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
assignment       = (identifier | field_access | array_access) "=" expression ";" ;
return_statement = "return" expression ";" ;
expression_stmt  = expression ";" ;

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
init_block       = "init" "(" [ param_list ] ")" block ;

if_statement     = "if" expression block { "else" "if" expression block } [ "else" block ] ;

expression       = initializer_list
                 | method_call
                 | call
                 | field_access
                 | array_access
                 | binary ;

method_call      = primary "." identifier "(" [ arg_list ] ")" ;
field_access     = primary "." identifier ;
call             = primary "(" [ arg_list ] ")" ;
array_access     = primary "[" expression "]" ;
arg_list         = expression { "," expression } ;

binary           = unary { bin_op unary } ;
unary            = [ ("-" | "!") ] primary ;
primary          = number
                 | string_literal
                 | identifier
                 | cast
                 | "(" expression ")"
                 | initializer_list ;
cast             = primary "as" type ;
initializer_list = "{" [ expression { "," expression } ] "}" ;

bin_op           = "+" | "-" | "*" | "/" | "%" |
                   "==" | "!=" | "<" | ">" | "<=" | ">=" ;

type             = "bool" | "i8" | "i16" | "i32" | "i64" |
                   "u8" | "u16" | "u32" | "u64" |
                   "f32" | "f64" | "string" | "none" |
                   "tensor" "[" type "]" |
                   identifier ;
```
All listed grammar constructs are parsed and code‑generated as described above.

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
│   ├── llvm.rs          # LLVM IR generation backend
│   ├── backtrace.rs     # Helper for caller tracing in logs
│   ├── span.rs          # Source span tracking
│   └── file.rs          # Source file abstraction
├── docs/                # Design notes
├── Cargo.toml
└── README.md
```

## Prerequisites
- Clang must be available on PATH (used to compile LLVM IR to native executable).

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

## Current Limitations & Notes
- Visibility is parsed but not enforced.
- Float literals are not tokenized; only integer literals exist today.
- Arrays, modules/imports, pattern matching, and generics are not implemented.
- Error messages are primarily intended for development iteration.

## Testing

Run the test suite to see current implementation status:

```bash
cargo test -q
```

### Test Results Overview
- Passing: 27/27

### Individual Tests
- `test_variable_declaration` ✅
- `test_assignment` ✅
- `test_casting` ✅
- `test_coercion` ✅
- `test_unary_op` ✅
- `test_unary_op_const` ✅
- `test_operator_comp` ✅
- `test_precedence_1` ✅
- `test_precedence_2` ✅
- `test_if_statement` ✅
- `test_if_else` ✅
- `test_if_else_elseif` ✅
- `test_if_else_elseif_2` ✅
- `test_if_ex` ✅
- `test_if_nx` ✅
- `test_block` ✅
- `test_printf` ✅
- `test_function` ✅
- `test_function_call` ✅
- `test_arithmetic_expression` ✅
- `test_fibonacci` ✅
- `test_simple_class` ✅
- `test_simple_inheritance` ✅
- `test_simple_constructor` ✅
- `test_class` ✅
- `test_method_call` ✅
- `test_tensor` ✅

## Roadmap

### ✅ Completed
- [x] Arithmetic, comparisons, unary ops
- [x] Variables and assignments
- [x] Implicit widening & explicit cast (`as`)
- [x] Blocks and if/else
- [x] Strings & `printf`
- [x] Function definitions and calls
- [x] Return statements
- [x] Class parsing (fields, visibility, methods, inheritance)
- [x] Constructors (`init`) and object construction
- [x] VTable generation and dynamic method dispatch
- [x] Tensors with initializer lists and indexing (1‑D)

### 🚧 In Progress
- [ ] Visibility enforcement
- [ ] Float literals, modules/imports
- [ ] Improved diagnostics
- [ ] Extended standard library bindings

### 📋 Planned
- [ ] Visibility enforcement in semantic analysis
- [ ] Arrays & heap allocation helpers
- [ ] Pattern matching
- [ ] Generics / parametric polymorphism  
- [ ] Modules & imports
- [ ] Standard library
- [ ] Closures and higher-order functions
- [ ] Compile-time computation
- [ ] Concurrency primitives

## License

This project is licensed under the Apache 2.0 License - see the [LICENSE](LICENSE) file for details.

## Architecture

Pipeline: Tokenize → Parse → Transform (LLVM IR) → Link via Clang → Run

Highlights:
- Tokenizer recognizes keywords (`class`, `public`/`private`/`protected`, `init`, `as`, …), operators, and punctuation (including `->`).
- Parser builds a rich AST including classes, method calls, field access, and casts.
- LLVM backend:
    - SSA‑like virtual registers with tracked types
    - String constants lowered to private globals
    - C interop declarations injected into the prologue
    - Class layout with leading vtable pointer and field offsets
    - Per‑class read‑only VTable constants and dynamic dispatch at call sites

## Future Roadmap

### Language Features
- [x] Casts with `as`, implicit integer widening
- [x] Basic I/O (`printf`)
- [ ] Float literals
- [ ] Multi‑dimensional tensors and slices
- [ ] Modules/imports
- [ ] Pattern matching
- [ ] Generics and parametric polymorphism  
- [ ] Closures and higher‑order functions
- [ ] Compile‑time computation
- [ ] Concurrency primitives

### Implementation Status
- ✅ Complete: Basic expressions, variables, control flow, type ops, functions, and classes (incl. constructors, method calls, inheritance)
- 🚧 Partial: Visibility enforcement, float literals, tensor ergonomics
- 📋 Planned: Items listed above

## Contact

- **Author**: Moetsuki
- **Repository**: [https://github.com/Moetsuki/mm-lang](https://github.com/Moetsuki/mm-lang)
- **Issues**: [https://github.com/Moetsuki/mm-lang/issues](https://github.com/Moetsuki/mm-lang/issues)
