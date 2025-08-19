#![allow(dead_code)]
#![allow(unused_mut)]
mod ast;
mod block;
mod expression;
mod llvm;
mod statement;
mod tokenizer;
mod types;
mod variable;
mod backtrace;
mod span;
mod file;

use ast::Ast;
use statement::Statement;
use std::backtrace::Backtrace;
use std::io::Write;
use std::process::{Command, Stdio};
use std::thread;
use tokenizer::tokenize;

use crate::file::SourceFile;

fn get_caller_name() -> Option<String> {
    let bt = Backtrace::force_capture();

    let bt_str = bt.to_string();

    let mut lines = bt_str.lines();

    let mut caller = String::from("");

    let mut skip_lines = 2; // Skip the first two lines which are the backtrace header and the current function
    while let (Some(line1), Some(line2)) = (lines.next(), lines.next()) {
        if line2.starts_with("             at /rustc/") {
            // This is a Rust function, skip it
            continue;
        }
        if skip_lines > 0 {
            skip_lines -= 1;
            continue;
        }
        //println!("({})-({})", line1, line2);

        caller = line1.to_string();

        break;
    }

    // Find the last :: in the caller string and extract from that to the end
    if let Some(last_colon) = caller.rfind("::") {
        return Some(caller[last_colon + 2..].to_string());
    }

    None
}

#[track_caller]
fn process(
    source: &str,
    expected: Option<String>,
    expected_exit_code: Option<i32>,
    print_asm: bool,
) {
    let source_file = SourceFile::new("source.mm", source.to_string());

    let mut tokens = tokenize(source, &source_file);

    let caller = get_caller_name().unwrap_or_else(|| "unknown".to_string());
    println!("Processing source code from: {}", caller);

    let outfile = "build/output_".to_string() + &caller;
    let asmfile = "build/asm_".to_string() + &caller;
    let outfile_invoke = "./".to_string() + &outfile;

    // for token in &tokens {
    //     println!("{:?}", token);
    // }

    let mut ast = Ast::new(tokens, &source_file);

    let _block = ast.parse();

    //print_block(&_block, 0);

    let mut llvm = llvm::LLVM::new(ast, &source_file);

    llvm.compile();

    let ir_output = llvm.output();
    for (i, line) in ir_output.lines().enumerate() {
        println!("{:>3}: {}", i, line);
    }

    if print_asm {
        // Compile LLVM IR via stdin
        let mut clang = Command::new("clang")
            .args(["-x", "ir", "-", "-S", "-g", "-O0", "-o", &asmfile])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to start clang");

        // Write IR to clang's stdin
        if let Some(stdin) = clang.stdin.as_mut() {
            stdin
                .write_all(ir_output.as_bytes())
                .expect("Failed to write to clang stdin");
        }

        // Wait for compilation to complete
        let output = clang
            .wait_with_output()
            .expect("Failed to read clang output");

        if output.status.success() {
            // Now read and echo the assembly
            let assembly_code =
                std::fs::read_to_string(&asmfile).expect("Failed to read generated assembly file");

            println!("Generated Assembly:\n{}", assembly_code);
        } else {
            println!("ASM generation failed:");
            println!("{}", String::from_utf8_lossy(&output.stderr));
        }
    }

    {
        // Compile LLVM IR via stdin
        let mut clang = Command::new("clang")
            .args(["-x", "ir", "-", "-o", &outfile]) // -x ir tells clang it's LLVM IR, - means stdin
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to start clang");

        // Write IR to clang's stdin
        if let Some(stdin) = clang.stdin.as_mut() {
            stdin
                .write_all(ir_output.as_bytes())
                .expect("Failed to write to clang stdin");
        }

        // Wait for compilation to complete
        let output = clang
            .wait_with_output()
            .expect("Failed to read clang output");

        if output.status.success() {
            println!("Compilation successful!");

            // Wait a little bit to make sure the file has been created
            thread::sleep(std::time::Duration::from_millis(1000));

            // Run the executable
            let run_output = Command::new(&outfile_invoke)
                .output()
                .expect("Failed to run executable");

            let std_out = String::from_utf8_lossy(&run_output.stdout);

            let exit_code = run_output.status.code().unwrap();

            let std_err = String::from_utf8_lossy(&run_output.stderr);

            println!("Program output:");
            println!("stdout: {}", &std_out);
            if !run_output.stderr.is_empty() {
                panic!("stderr: {}", &std_err);
            }
            println!("Exit code: {}", &exit_code);


            if let Some(_expected_exit_code) = expected_exit_code {
                // trunc exit_code to 8 bits
                assert_eq!(exit_code as i8, _expected_exit_code as i8, "Program did not exit with expected code");
            }
            if let Some(expected_output) = expected {
                assert_eq!(
                    std_out.trim(),
                    expected_output.trim(),
                    "Program output did not match expected output"
                );
            }
        } else {
            println!("Compilation failed:");
            println!("{}", String::from_utf8_lossy(&output.stderr));
            panic!("");
        }
    }
}

fn print_block(block: &block::Block, level: usize) {
    let ident_steps = 4;
    for statement in &block.statements {
        match statement {
            Statement::Block { body, .. } => {
                println!("{:indent$}Block {{", "", indent = level * ident_steps);
                print_block(body, level + 1);
                println!("{:indent$}}}", "", indent = level * ident_steps);
            }
            Statement::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                println!(
                    "{:indent$}If condition: {:?}",
                    "",
                    condition,
                    indent = level * ident_steps
                );
                println!("{:indent$}Then {{", "", indent = level * ident_steps);
                print_block(then_block, level + 1);
                println!("{:indent$}}}", "", indent = level * ident_steps);
                if let Some(else_block) = else_block {
                    println!("{:indent$}Else {{", "", indent = level * ident_steps);
                    print_block(else_block, level + 1);
                    println!("{:indent$}}}", "", indent = level * ident_steps);
                }
            }
            _ => println!(
                "{:indent$}{:?}",
                "",
                statement,
                indent = level * ident_steps
            ),
        }
    }
}

fn main() {
    let source = r#"
    function baz(x: i32, y: i32) -> i64 {
        return x + y;
    }
    "#;
    process(source, None, None, false);
}

#[test]
fn test_assignment() {
    let source = "x: i64 = 5; y: i64 = 10; return 2 * x + y / 2;";
    process(source, None, Some(15), false);
}

#[test]
fn test_function() {
    let source = r#"
    function add(a: i64, b: i64) -> i64 {
        return a + b;
    }

    return add(5, 10);
    "#;
    process(source, None, Some(15), false);
}

#[test]
fn test_if_statement() {
    let source = r#"
    x: i64 = 5;
    if x > 10 {
        y: i64 = 20;
    } else {
        y: i64 = 30;
        return y;
    }
    "#;
    process(source, None, Some(30), false);
}

#[test]
fn test_block() {
    let source = r#"
    {
        x: i64 = 5;
        y: i64 = 10;
        if x < y {
            x = y;
            return x;
        }
    }
    "#;
    process(source, None, Some(10), false);
}

#[test]
fn test_variable_declaration() {
    let source = r#"
    x: i64 = 42;
    y: i64 = 100;
    "#;
    process(source, None, None, false);
}

#[test]
fn test_function_call() {
    let source = r#"
    function foo(x: i64) -> i64{
        return x * 2;
    }
    result: i64 = foo(10);

    return result;
    "#;
    process(source, None, Some(20), false);
}

#[test]
fn test_casting() {
    let source = r#"
    x: i64 = 5;
    y: i32 = x as i32;
    z: i64 = x + y;
    "#;
    process(source, None, None, false);
}

#[test]
fn test_coercion() {
    let source = r#"
    x: i32 = 5;
    y: i8 = 10;
    z: i64 = x + y; // Implicit coercion from i32 to i64
    "#;
    process(source, None, None, false);
}

#[test]
fn test_unary_op() {
    let source = r#"
    x: i64 = -5;
    y: i64 = - x + 2;
    "#;
    process(source, None, None, false);
}

#[test]
fn test_unary_op_const() {
    let source = r#"
    x: i64 = - ( - 4 - 2 );
    "#;
    process(source, None, None, false);
}

#[test]
fn test_printf() {
    let source = r#"
    str: string = "Hello, World!";
    printf(str);
    "#;
    process(source, None, None, false);
}

#[test]
fn test_operator_comp() {
    let source = r#"
        return 5 == 6;
    "#;
    process(source, None, Some(0), false);
}

#[test]
fn test_precedence_1() {
    let source = r#"
    x: i64 = 5;
    y: i64 = 10;
    z: i64 = x + y * 2; // y * 2 is evaluated first
    return z; // Should return 25
    "#;
    process(source, None, Some(25), false);
}

#[test]
fn test_precedence_2 () {
    let source = r#"
    x: i64 = 5;
    y: i64 = 10;
    z: bool = (25 == x + y * 2); // y * 2 is evaluated first
    return z; // Should return true
    "#;
    process(source, None, Some(1), false);
}

#[test]
fn test_class() {
    let source = r#"
    class Entity {
        public name: string;
        protected id: u64;

        init (new_id: u64) {
            self.id = new_id;
        }
    };

    class Animal : Entity {
        private age: i32;
        protected species: string;

        init (new_age: i32, new_species: string) {
            self.age = new_age;
            self.species = new_species;
        }
    };

    class Dog : Animal {
        public breed: i16;

        init (new_breed: i16) {
            self.breed = new_breed;
        }
    };

    class Cat : Animal {
        public color: i8;

        init (new_color: i8) {
            self.color = new_color;
        }
    };
    
    "#;
    process(source, None, None, false);
}

#[track_caller]
#[test]
fn test_method_call() {
    let source = r#"
    class Entity {
        public name: string;

        init(str: string) {
            self.name = str;
        }

        public function name() -> string {
            return self.name;
        }
    };

    ent: Entity = Entity();
    ent.name = "Test Entity";
    result: string = ent.name();
    printf(result);
    "#;
    process(source, Some("Test Entity".to_string()), None, false);
}

#[test]
fn test_simple_class() {
    let source = r#"
    class Point {
        public x: i32;
        public y: i32;

        init(x: i32, y: i32) {
            self.x = x;
            self.y = y;
        }

        public function get_x() -> i32 {
            return self.x;
        }

        public function get_y() -> i32 {
            return self.y;
        }
    };

    p: Point = Point(5, -15);

    return p.get_x() + p.y;
    "#;
    process(source, None, Some(-10), false);
}

#[test]
fn test_simple_inheritance() {
    let source = r#"
    class Animal {
        public name: string;
        public id: u64;

        init(name: string, id: u64) {
            self.name = name;
            self.id = id;
        }

        public function speak() -> string {
            return "Animal sound";
        }
    };

    class Dog : Animal {
        public breed: string;

        public function bark() -> string {
            return "Woof!";
        }
    };
    "#;
    process(source, None, None, false);
}

#[test]
fn test_simple_constructor() {
    let source = r#"
    class Point {
        public x: i32;
        public y: i32;

        init(x: i32, y: i32) {
            self.x = x;
            self.y = y;
        }
    };

    p : Point = Point(6, 9);
    "#;
    process(source, None, None, false);
}

#[test]
fn test_arithmetic_expression() {
    let source = r#"
    result: i64 = (((5 + 3) * 2 - 4 / 2) + 1) * 2;
    return result; // Should return 30
    "#;
    process(source, None, Some(30), false);
}

#[test]
fn test_fibonacci() {
    let source = r#"
    function fib(n: i32) -> i32 {
        if n <= 1 {
            return n;
        } else {
            return fib(n - 1) + fib(n - 2);
        }
    }

    return fib(10); // Should return 55
    "#;
    process(source, None, Some(55), false);
}
