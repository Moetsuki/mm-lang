# MM-Lang Programming Language

A custom programming language compiler written in Rust that lowers to LLVM IR.

## Overview

MM-Lang is a small, statically‑typed, expression–oriented language with C / Rust inspired surface syntax. The current prototype supports:

**Fully Working Features (Tested):**
- Explicit variable declarations with type annotations
- Arithmetic, comparison, unary `-` and logical negation `!`
- Implicit numeric coercions (widening) & explicit casts with `as`
- Blocks, if / else control flow
- Basic I/O operations via `printf` binding
- String literals (lowered to C strings)
- Function definitions and function calls (basic functionality working)

**Partially Working Features:**
- Single inheritance classes with fields and visibility modifiers (AST parsing works, simple class definitions compile)
- Constructor syntax via `init { ... }` blocks (parsed but LLVM codegen has limitations)

**In Development:**
- Method calls and field access (parsing works, codegen has issues with complex inheritance)
- Advanced class features like proper vtable inheritance and method dispatch

> NOTE: The compiler successfully parses most language constructs and generates LLVM IR for basic operations, functions, and simple classes. Advanced features like complex class inheritance and method calls are still under development in the LLVM backend.

## Core Features

### Types
Primitive & built‑in types:
`bool`, `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `string`.

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

Unary: `-expr`, `!expr` (logical not on booleans / truthy integer values).

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
`printf` is declared automatically with a variadic signature. Additional C bindings: `scanf`, `malloc`, `free`.

## Classes (Experimental) ⚠️
Single inheritance with visibility modifiers and methods. **Note: Parsing works correctly, but LLVM code generation is incomplete.**

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

### Current Status:
- ✅ Class declarations with inheritance
- ✅ Field declarations with visibility modifiers
- ✅ Method declarations  
- ✅ Constructor `init` blocks with parameters
- ⚠️ LLVM struct generation (partial)
- ❌ Method calls and field access (codegen incomplete)
- ❌ Proper vtable inheritance/dispatch
- ❌ Visibility enforcement

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

## Updated Grammar (Provisional)
EBNF sketch reflecting implemented constructs. Note: All parsing is implemented, but LLVM code generation is incomplete for some features.

```ebnf
program          = { statement } ;

statement        = variable_decl        (* ✅ Working *)
                 | assignment           (* ✅ Working for variables *)
                 | function_def         (* ✅ Working *)
                 | class_decl           (* ⚠️ Parsing works, simple classes compile *) 
                 | if_statement         (* ✅ Working *)
                 | return_statement     (* ✅ Working *)
                 | expression_stmt      (* ⚠️ Limited support *)
                 | block ;              (* ✅ Working *)

block            = "{" { statement } "}" ;                    (* ✅ Working *)

variable_decl    = identifier ":" type "=" expression ";" ;   (* ✅ Working *)
assignment       = expression "=" expression ";" ;           (* ⚠️ Variables only *)
return_statement = "return" expression ";" ;                 (* ✅ Working *)
expression_stmt  = expression ";" ;                          (* ⚠️ Basic expressions only *)

function_def     = "function" identifier "(" [ param_list ] ")" "->" type block ; (* ✅ Working *)
param_list       = param { "," param } ;
param            = identifier ":" type ;

class_decl       = "class" identifier [ ":" identifier ] "{" { class_member } "}" ";" ; (* ⚠️ *)
class_member     = visibility field_decl
                 | visibility method_def  
                 | init_block ;

visibility       = "public" | "private" | "protected" ;      (* ✅ Parsed *)
field_decl       = identifier ":" type ";" ;                 (* ✅ Parsed *)
method_def       = "function" identifier "(" [ param_list ] ")" "->" type block ; (* ⚠️ *)
init_block       = "init" "(" [ param_list ] ")" block ;     (* ⚠️ Parsed *)

if_statement     = "if" expression block [ "else" block ] ;  (* ✅ Working *)

expression       = method_call          (* ⚠️ Parsed only *)
                 | call                 (* ⚠️ Parsed only *)
                 | binary ;             (* ✅ Working *)

method_call      = primary "." identifier "(" [ arg_list ] ")" ; (* ❌ Parsing works, codegen fails *)
call             = primary "(" [ arg_list ] ")" ;                (* ✅ Working *)
arg_list         = expression { "," expression } ;

binary           = unary { bin_op unary } ;                  (* ✅ Working *)
unary            = [ ("-" | "!") ] primary ;                 (* ✅ Working *)
primary          = number                                     (* ✅ Working *)
                 | string_literal                             (* ✅ Working *)
                 | identifier                                 (* ✅ Working *)
                 | cast                                       (* ✅ Working *)
                 | "(" expression ")" ;                       (* ✅ Working *)
cast             = primary "as" type ;                       (* ✅ Working *)

bin_op           = "+" | "-" | "*" | "/" | "%" |             (* ✅ Working *)
                   "==" | "!=" | "<" | ">" | "<=" | ">=" ;   (* ✅ Working *)

type             = "bool" | "i8" | "i16" | "i32" | "i64" |   (* ✅ Working *)
                   "u8" | "u16" | "u32" | "u64" |            (* ✅ Working *)
                   "f32" | "f64" | "string" |                (* ✅ Working *)
                   identifier ;          (* future: arrays / generics *)
```

**Legend:**
- ✅ **Working**: Full parsing + LLVM codegen + tested
- ⚠️ **Partial**: Parsing complete, LLVM codegen incomplete  
- ❌ **Not implemented**: Neither parsing nor codegen

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

## Current Limitations & Known Issues

### Working Features
- ✅ All basic language constructs: variables, arithmetic, comparisons, type coercion/casting
- ✅ Control flow: if/else statements, blocks, scoping
- ✅ Unary operations and string literals
- ✅ C interop: printf function calls
- ✅ Function definitions and basic function calls
- ✅ Simple class definitions (without complex inheritance)

### Known Issues
- ❌ **Complex class inheritance**: Field resolution fails with multi-level inheritance
- ❌ **Method calls**: Type conversion issues in method dispatch
- ❌ **Field access operations**: Some codegen limitations for object field access
- ⚠️ **Type system**: Some edge cases in automatic type conversion for complex objects

### Debug Information
The compiler provides detailed AST output and LLVM IR generation traces. Failed tests show exactly where in the compilation pipeline issues occur, making it easy to track progress on incomplete features.

### Specific Test Failure Details
- **`test_class`**: Fails during complex inheritance field resolution - "Field 'id' not found in class 'Animal'" when trying to access inherited fields from parent classes
- **`test_method_call`**: Fails with "Unsupported automatic conversion from ptr to i32" during method dispatch, indicating type system issues with object references

## Testing

Run the test suite to see current implementation status:

```bash
cargo test
```

### Test Results Overview:
- ✅ **Passing (12 tests)**: Basic variable declarations, arithmetic operations, type coercion/casting, unary operations, control flow (if/else), blocks, printf output, functions, function calls, and simple classes
- ❌ **Failing (2 tests)**: Complex classes with inheritance, method calls - these features have parsing implemented but encounter LLVM code generation issues

### Individual Test Status:
- `test_variable_declaration` ✅
- `test_assignment` ✅  
- `test_casting` ✅
- `test_coercion` ✅
- `test_unary_op` ✅
- `test_unary_op_const` ✅
- `test_if_statement` ✅
- `test_block` ✅
- `test_printf` ✅
- `test_function` ✅
- `test_function_call` ✅
- `test_simple_class` ✅
- `test_class` ❌ (Complex inheritance - field resolution issues)
- `test_method_call` ❌ (Type conversion issues in method dispatch)

## Roadmap

### ✅ Completed
- [x] Basic arithmetic & control flow
- [x] Variable declarations and assignments  
- [x] Implicit widening & explicit casting
- [x] Unary operators
- [x] Strings & printf binding
- [x] Function definitions and basic function calls
- [x] Return statements
- [x] Simple class definitions
- [x] Class AST parsing & visibility modifiers
- [x] Constructor `init` block parsing
- [x] Inheritance parsing

### 🚧 In Progress  
- [ ] Complex multi-level class inheritance (field resolution issues)
- [ ] Method calls and field access (type conversion challenges)
- [ ] Proper vtable inheritance & method dispatch
- [ ] Constructor/destructor end-to-end implementation

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

The compiler follows a traditional multi-stage architecture:

1. **Lexical Analysis** (`tokenizer.rs`) - Converts source code into tokens ✅
2. **Syntax Analysis** (`ast.rs`) - Builds an Abstract Syntax Tree ✅  
3. **Semantic Analysis** - Type checking and symbol resolution ⚠️ (works for basic features, issues with complex inheritance)
4. **Code Generation** (`llvm.rs`) - Generates LLVM IR ✅ (works for most features, specific issues with inheritance field resolution and method dispatch)
5. **Optimization** - LLVM optimizations (handled by LLVM) ✅
6. **Code Emission** - Final machine code (handled by LLVM) ✅

The current implementation successfully handles the lexical and syntax analysis phases for all planned language features. The LLVM code generation backend works well for functions, simple classes, and all basic operations but encounters specific issues with complex class inheritance field resolution and method call type conversion.

## Future Roadmap

### Language Features
- [x] Type casting with `as` keyword  
- [x] Implicit type coercion for compatible types
- [x] Basic I/O operations (printf)
- [ ] Advanced type system with generics
- [ ] Module system and imports  
- [ ] Standard library
- [ ] Pattern matching
- [ ] Closures and higher-order functions
- [ ] Compile-time computation
- [ ] Concurrency primitives

### Implementation Status
- ✅ **Complete**: Basic expressions, variables, control flow, type operations, functions, simple classes
- 🚧 **Partial**: Complex class inheritance, method calls (parsing complete, codegen has specific issues) 
- 📋 **Planned**: Advanced features listed above

## Contact

- **Author**: Moetsuki
- **Repository**: [https://github.com/Moetsuki/mm-lang](https://github.com/Moetsuki/mm-lang)
- **Issues**: [https://github.com/Moetsuki/mm-lang/issues](https://github.com/Moetsuki/mm-lang/issues)
