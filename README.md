# MM‑Lang Programming Language

A small, statically typed language **compiler** written in Rust that lowers to LLVM IR or C and builds executables via Clang.

## Documentation

Deep‑dive docs for core modules and backends:

> __Syntax Parsing__
1. [Tokenizer Module](docs/tokenizer.md)
2. [AST Parser Module](docs/ast.md)
   - [Expressions](docs/expression.md)
   - [Statements](docs/statement.md)
   - [Blocks](docs/block.md)
   - [Variables](docs/variable.md)
   - [Types](docs/types.md)

> __Compilation__
- [LLVM IR Backend Module](docs/backend_llvm.md)
- [Compile-to-C Backend Module](docs/backend_c.md)

## Backends

Two code generation backends are available and validated by the test suite:

- LLVM IR backend
- C backend

You’ll see per‑test logs prefixed with the target, e.g. `[Backend=llvm]` and `[Backend=c]`.

### Fully working (tested)
- Variable declarations with explicit types and assignments
- Arithmetic and comparisons; unary `-` and logical negation `!`
- Logical operators: `&&`, `||` (eager evaluation currently)
- Implicit integer widening and explicit casts using `as`
- Blocks and if/else control flow with proper scoping
- String literals (lowered to private constant C strings)
- C interop: `printf`, `scanf`, `malloc`, `free`
- Functions: definitions, calls, return values
- Booleans: `true` and `false` literals, boolean expressions
- Floats: `f64` literals and mixed int/float arithmetic with proper lowering
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
- Structs:
    - Declarations: `struct Name { field: Type, ... }` (optional trailing comma; optional trailing semicolon after block)
    - Literals: `Name { field1: expr, field2: expr }`
    - Field access and assignment: `p.x`, `p.y = 7`
    - Plain layout (no vtable); fields indexed from 0 in LLVM IR

### Known limitations (current behavior)
- Visibility keywords are parsed and preserved in types, but enforcement is not performed yet.
- Logical operators are eager (no short‑circuit yet).
- Omitted fields in struct literals are left uninitialized; accessing them is undefined behavior for now.
- Modules/imports, pattern matching, and generics are not implemented.
- Error reporting is panic‑driven and aimed at development use.

## Core Features

### Types
Primitive/built‑in:
`bool`, `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `string`, `none`.

Composite and compiler types used in codegen:
- `function name(args...) -> ret` (first‑class function type)
- `class Name [: Parent] { fields, methods }` (lowered to `%Name` with `%NameVTable`)
- `struct Name [: Parent] { fields }` (lowered to `%Name` without vtable)
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

Comparison: `== != < > <= >=` (integers via `icmp`, floats via `fcmp`).

Logical: `&&`, `||` produce booleans (currently both sides are evaluated).

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
String literals are lowered to private constant null‑terminated byte arrays (LLVM) or `const char*` (C). Example using `printf`:
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
                 | struct_decl
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
struct_decl      = "struct" identifier [ ":" identifier ] "{" { struct_field [ ("," | ";") ] } "}" [ ";" ] ;
struct_field     = identifier ":" type ;
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
                 | struct_literal
                 | field_access
                 | array_access
                 | binary ;

method_call      = primary "." identifier "(" [ arg_list ] ")" ;
field_access     = primary "." identifier ;
call             = primary "(" [ arg_list ] ")" ;
struct_literal   = identifier "{" [ identifier ":" expression { "," identifier ":" expression } [ "," ] ] "}" ;
array_access     = primary "[" expression "]" ;
arg_list         = expression { "," expression } ;

binary           = logical_or ;
logical_or       = logical_and { "||" logical_and } ;
logical_and      = comparison { "&&" comparison } ;
comparison       = additive { ("==" | "!=" | "<" | ">" | "<=" | ">=") additive } ;
additive         = term { ("+" | "-") term } ;
term             = cast { ("*" | "/") cast } ;
unary            = [ ("-" | "!") ] postfix ;
postfix          = primary { ("[" expression "]") | ("." identifier [ "(" [ arg_list ] ")" ]) | ("(" [ arg_list ] ")") } ;
primary          = number | float | string_literal | identifier | cast | "(" expression ")" | initializer_list | struct_literal ;
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
│   ├── backend.rs       # Backend trait & TargetKind
│   ├── backend_c/       # C backend
│   │   ├── mod.rs
│   │   ├── target_c.rs  # C code generation
│   │   └── type_c.rs    # Type mapping to C
│   ├── backend_llvm/    # LLVM backend
│   │   ├── llvm.rs      # LLVM IR generation
│   │   └── type_llvm.rs # Type mapping to LLVM
│   ├── tokenizer.rs     # Lexical analysis
│   ├── ast.rs           # AST construction
│   ├── expression.rs    # Expressions
│   ├── statement.rs     # Statements & class/visibility enums
│   ├── block.rs         # Block container
│   ├── variable.rs      # Variable representation
│   ├── types.rs         # Type enum / helpers
│   ├── backtrace.rs     # Helper for caller tracing in logs
│   ├── span.rs          # Source span tracking
│   └── file.rs          # Source file abstraction
├── docs/                # Design notes
├── Cargo.toml
└── README.md
```

## Prerequisites
- Clang must be available on PATH (used to compile LLVM IR or C to native executables).

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
- Logical operators are eager (no short‑circuit yet).
- Arrays (tensors) are 1‑D only; no slices yet. Modules/imports, pattern matching, and generics are not implemented.
- Error messages are primarily intended for development iteration.

## Testing

Run the test suite to see current implementation status:

```bash
cargo test -q
```

### Test Results Overview
- Passing: 38/38

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
 - `test_boolean` ✅
 - `test_float_ops` ✅
 - `test_int_to_float_1` ✅
 - `test_float_to_int_1` ✅
 - `test_float_to_int_2` ✅
 - `test_float_to_double` ✅
 - `test_simple_struct` ✅
 - `test_struct_field_assignment` ✅
 - `test_struct_trailing_comma_and_semicolon` ✅
 - `test_struct_temp_literal_access` ✅
 - `test_struct_partial_literal_unused_field` ✅

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
 - [x] Struct declarations, literals, field access/assignment
 - [x] Booleans and logical operators (||, &&)
 - [x] Float literals and mixed arithmetic

### 🚧 In Progress
- [ ] Visibility enforcement
- [ ] Short‑circuiting logical operators
- [ ] Modules/imports
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

Pipeline: Tokenize → Parse → Transform (LLVM IR | C) → Generate executable via Clang → Run

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
- [x] Float literals
- [ ] Multi‑dimensional tensors and slices
- [ ] Modules/imports
- [ ] Pattern matching
- [ ] Generics and parametric polymorphism  
- [ ] Closures and higher‑order functions
- [ ] Compile‑time computation
- [ ] Concurrency primitives

### Implementation Status
- ✅ Complete: Basic expressions, variables, control flow, type ops, functions, classes (incl. constructors, method calls, inheritance), structs, tensors, booleans/logical ops, float literals
- 🚧 Partial: Visibility enforcement, short‑circuiting logical ops, tensor ergonomics
- 📋 Planned: Items listed above

## Contact

- **Author**: Moetsuki
- **Repository**: [https://github.com/Moetsuki/mm-lang](https://github.com/Moetsuki/mm-lang)
- **Issues**: [https://github.com/Moetsuki/mm-lang/issues](https://github.com/Moetsuki/mm-lang/issues)
