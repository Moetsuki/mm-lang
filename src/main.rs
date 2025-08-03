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

    println!("{}", llvm.output());
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
    x: i32 = 5; y: u32 = 10;
    z: i64 = x + y // This is a comment
    bar : u8 = 2;
    if z > 15 {
        print("yes + no"); //Another comment
    } else {
        if bar<=5 {
            print("small");
        } else {
            print("big");
        }
    }

    function foo(text: string) {
        print(text);
    }
    "#;
    process(source);
}

#[test]
fn test_assignment() {
    let source = "x = 5; y = 10;";
    process(source);
}
