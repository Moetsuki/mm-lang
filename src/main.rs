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

use ast::Ast;
use statement::Statement;
use std::io::Write;
use std::process::{Command, Stdio};
use tokenizer::tokenize;

fn process(source: &str) {
    let mut tokens = tokenize(source);

    for token in &tokens {
        println!("{:?}", token);
    }

    let mut ast = Ast::new(tokens);

    let block = ast.parse();

    print_block(&block, 0);

    let mut llvm = llvm::LLVM::new(ast);

    llvm.compile();

    let ir_output = llvm.output();
    println!("{}", ir_output);

    // Compile LLVM IR via stdin
    let mut clang = Command::new("clang")
        .args(&["-x", "ir", "-", "-o", "build/output"]) // -x ir tells clang it's LLVM IR, - means stdin
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

        // Run the executable
        let run_output = Command::new("./build/output")
            .output()
            .expect("Failed to run executable");

        println!("Program output:");
        println!("stdout: {}", String::from_utf8_lossy(&run_output.stdout));
        if !run_output.stderr.is_empty() {
            println!("stderr: {}", String::from_utf8_lossy(&run_output.stderr));
        }
        println!("Exit code: {}", run_output.status.code().unwrap_or(-1));
    } else {
        println!("Compilation failed:");
        println!("{}", String::from_utf8_lossy(&output.stderr));
    }
}

fn print_block(block: &block::Block, level: usize) {
    let ident_steps = 4;
    for statement in &block.statements {
        match statement {
            Statement::Block { body } => {
                println!("{:indent$}Block {{", "", indent = level * ident_steps);
                print_block(body, level + 1);
                println!("{:indent$}}}", "", indent = level * ident_steps);
            }
            Statement::If {
                condition,
                then_block,
                else_block,
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
    process(source);
}

#[test]
fn test_assignment() {
    let source = "x: i64 = 5; y: i64 = 10;";
    process(source);
}

#[test]
fn test_function() {
    let source = r#"
    function add(a: i64, b: i64) -> i64 {
        return a + b;
    }
    "#;
    process(source);
}

#[test]
fn test_if_statement() {
    let source = r#"
    x: i64 = 5;
    if x > 10 {
        y: i64 = 20;
    } else {
        y: i64 = 30;
    }
    "#;
    process(source);
}

#[test]
fn test_block() {
    let source = r#"
    {
        x: i64 = 5;
        y: i64 = 10;
        if x < y {
            x = y;
        }
    }
    "#;
    process(source);
}

#[test]
fn test_variable_declaration() {
    let source = r#"
    x: i64 = 42;
    y: i64 = 100;
    "#;
    process(source);
}

#[test]
fn test_function_call() {
    let source = r#"
    function foo(x: i64) -> i64{
        return x * 2;
    }
    result: i64 = foo(10);
    "#;
    process(source);
}

#[test]
fn test_casting() {
    let source = r#"
    x: i64 = 5;
    y: i32 = x as i32;
    z: i64 = x + y;
    "#;
    process(source);
}

#[test]
fn test_coercion() {
    let source = r#"
    x: i32 = 5;
    y: i8 = 10;
    z: i64 = x + y; // Implicit coercion from i32 to i64
    "#;
    process(source);
}

#[test]
fn test_unary_op() {
    let source = r#"
    x: i64 = -5;
    y: i64 = - x + 2;
    "#;
    process(source);
}

#[test]
fn test_unary_op_const() {
    let source = r#"
    x: i64 = - ( - 4 - 2 );
    "#;
    process(source);
}

#[test]
fn test_printf() {
    let source = r#"
    str: string = "Hello, World!";
    printf(str);
    "#;
    process(source);
}
