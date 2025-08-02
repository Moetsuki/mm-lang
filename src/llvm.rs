use core::panic;

use crate::types::Type;
use crate::variable::Variable;
use crate::{ast::Ast, expression::Expression, statement::Statement};
use std::collections::HashMap;

use std::sync::atomic::{AtomicU64, Ordering};

///
/// Scope
///
#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub symbols: HashMap<Register, Variable>,
}

#[derive(Debug)]
pub struct Scope {
    stack: Vec<SymbolTable>,
}

impl Scope {
    pub fn new() -> Self {
        Scope { stack: Vec::new() }
    }

    pub fn enter_scope(&mut self) {
        self.stack.push(SymbolTable {
            symbols: HashMap::new(),
        });
    }

    pub fn exit_scope(&mut self) {
        self.stack.pop();
    }

    pub fn insert(&mut self, register: Register, var_info: Variable) {
        if let Some(current) = self.stack.last_mut() {
            current.symbols.insert(register, var_info);
        }
    }

    pub fn resolve(&self, register: &Register) -> Option<&Variable> {
        for scope in self.stack.iter().rev() {
            if let Some(info) = scope.symbols.get(register) {
                return Some(info);
            }
        }
        None
    }

    pub fn resolve_mut(&mut self, register: &Register) -> Option<&mut Variable> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(info) = scope.symbols.get_mut(register) {
                return Some(info);
            }
        }
        None
    }

    pub fn find(&self, variable: &Variable) -> Option<&Register> {
        for scope in self.stack.iter().rev() {
            for symbol in scope.symbols.iter() {
                if symbol.1.name == variable.name {
                    return Some(symbol.0);
                }
            }
        }
        None
    }

    pub fn exists(&self, variable: &Variable) -> bool {
        for scope in self.stack.iter().rev() {
            if scope.symbols.values().any(|v| v.name == variable.name) {
                return true;
            }
        }
        false
    }
}

///
/// Intermediate Representation (IR)
///
#[derive(Debug, Clone)]
pub struct IR {
    pub instructions: Vec<String>,
}

impl IR {
    pub fn new() -> Self {
        IR {
            instructions: Vec::new(),
        }
    }

    pub fn push(&mut self, instruction: String) {
        self.instructions.push(instruction);
    }
}

impl ToString for IR {
    fn to_string(&self) -> String {
        self.instructions.join("\n")
    }
}

///
/// Evaluation
///
#[derive(Debug, Clone)]
pub struct Evaluation {
    pub prologue: IR,
    pub epilogue: IR,
    pub register: Register,
}

///
/// Register
///
static REGISTER_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Register {
    id: u64,
}

impl Register {
    pub fn new() -> Self {
        let id = REGISTER_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Register { id }
    }
}

impl ToString for Register {
    fn to_string(&self) -> String {
        format!("reg{}", self.id)
    }
}

///
/// Strings
///
static STRING_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone)]
pub struct StringData {
    id: u64,
    value: String,
}

impl StringData {
    pub fn new(value: String) -> Self {
        let id = STRING_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        StringData { id, value }
    }

    pub fn name(&self) -> String {
        format!("str{}", self.id)
    }

    pub fn length(&self) -> usize {
        self.value.len()
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl ToString for StringData {
    fn to_string(&self) -> String {
        format!("str{} = \"{}\"", self.id, self.value)
    }
}

///
/// LLVM
///
pub struct LLVM {
    prologue: IR,
    main_prologue: IR,
    main: IR,
    main_epilogue: IR,
    code: IR,
    epilogue: IR,
    ast: Ast,
    scope: Scope,
}

impl LLVM {
    pub fn new(ast: Ast) -> Self {
        LLVM {
            prologue: IR::new(),
            epilogue: IR::new(),
            main_prologue: IR::new(),
            main: IR::new(),
            main_epilogue: IR::new(),
            code: IR::new(),
            ast,
            scope: Scope::new(),
        }
    }

    pub fn compile(&mut self) {
        self.prologue.push(format!("\n; PROLOGUE"));
        self.prologue
            .push(format!("; LLVM IR generated from MM-lang\n"));

        self.main_prologue.push(format!("\n; MAIN PROLOGUE"));
        //self.main_prologue.push(format!("@main = global i32 0"));
        self.main_prologue.push(format!("define i32 @main() {{\n"));

        let eval = self.transform();
        self.main.instructions.extend(eval.prologue.instructions);
        self.main.instructions.extend(eval.epilogue.instructions);

        self.main_epilogue.push(format!("\n; MAIN EPILOGUE"));
        self.main_epilogue.push(format!("  ret i32 0"));
        self.main_epilogue.push(format!("}}"));
    }

    pub fn output(&self) -> String {
        let mut output = String::new();
        output.push_str(&self.prologue.to_string());
        output.push_str(&self.main_prologue.to_string());
        output.push_str(&self.main.to_string());
        output.push_str(&self.main_epilogue.to_string());
        output.push_str(&self.code.to_string());
        output.push_str(&self.epilogue.to_string());
        output
    }

    pub fn transform(&mut self) -> Evaluation {
        let mut eval = Evaluation {
            prologue: IR::new(),
            epilogue: IR::new(),
            register: Register::new(),
        };

        self.scope.enter_scope();

        let statements = self.ast.get().unwrap().statements.clone();
        for statement in &statements {
            match statement {
                Statement::Assignment {
                    identifier: lhs,
                    value: rhs,
                } => {
                    match lhs {
                        Expression::Variable(_) => {
                            //
                        }
                        _ => {
                            panic!(
                                "Unsupported left-hand side identifier type in LLVM IR transformation"
                            );
                        }
                    }

                    match rhs {
                        Expression::Number(_) => {
                            let rhs_eval = self.transform_expression(rhs.clone());

                            eval.prologue
                                .instructions
                                .extend(rhs_eval.prologue.instructions);

                            eval.epilogue
                                .instructions
                                .extend(rhs_eval.epilogue.instructions);

                            let result_register = Register::new();

                            self.scope.insert(
                                result_register.clone(),
                                Variable {
                                    name: lhs.to_string(),
                                    var_type: Type::I32,
                                },
                            );

                            eval.epilogue.push(format!(
                                "%{} = add i32 0, %{}",
                                result_register.to_string(),
                                rhs_eval.register.to_string(),
                            ));
                        }
                        _ => {
                            panic!("Unsupported identifier type in LLVM IR transformation");
                        }
                    }
                }
                _ => {
                    panic!("Unsupported statement type in LLVM IR transformation");
                }
            }
        }

        self.scope.exit_scope();

        eval
    }

    pub fn transform_expression(&mut self, expr: Expression) -> Evaluation {
        let mut eval = Evaluation {
            prologue: IR::new(),
            epilogue: IR::new(),
            register: Register::new(),
        };

        match expr {
            Expression::Number(value) => {
                eval.epilogue.push(format!(
                    "%{} = add i32 0, {}",
                    eval.register.to_string(),
                    value
                ));
            }
            Expression::StringLiteral(value) => {
                let string_data = StringData::new(value);

                // @.str1001 = private constant [6 x i8] c"hello\00"
                self.prologue.push(format!(
                    "@.{} = private constant [{} x i8] c\"{}\\00\"\n",
                    string_data.name(),
                    string_data.length() + 1,
                    string_data
                        .value()
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                ));

                // %msg = getelementptr [6 x i8], [6 x i8]* @.str, i32 0, i32 0
                eval.epilogue.push(format!(
                    "%{} = getelementptr [{} x i8], [{} x i8]* @.{}, i32 0, i32 0",
                    eval.register.to_string(),
                    string_data.length() + 1,
                    string_data.length() + 1,
                    string_data.name()
                ));
            }
            Expression::BinaryOp { op, left, right } => {
                let left_eval = self.transform_expression(*left);
                let right_eval = self.transform_expression(*right);

                eval.prologue
                    .instructions
                    .extend(left_eval.prologue.instructions);
                eval.prologue
                    .instructions
                    .extend(right_eval.prologue.instructions);

                match op.as_str() {
                    "+" => {
                        eval.epilogue.push(format!(
                            "%{} = add i32 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                    }
                    "-" => {
                        eval.epilogue.push(format!(
                            "%{} = sub i32 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                    }
                    "*" => {
                        eval.epilogue.push(format!(
                            "{} = mul i32 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                    }
                    "/" => {
                        eval.epilogue.push(format!(
                            "%{} = sdiv i32 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                    }
                    _ => {
                        panic!("Unsupported binary operator: {}", op);
                    }
                }
            }
            Expression::UnaryOp { op, expr } => {
                let inner_eval = self.transform_expression(*expr);
                eval.prologue
                    .instructions
                    .extend(inner_eval.prologue.instructions);

                match op.as_str() {
                    "-" => {
                        eval.epilogue.push(format!(
                            "%{} = sub i32 0, %{}",
                            eval.register.to_string(),
                            inner_eval.register.to_string()
                        ));
                    }
                    "!" => {
                        eval.epilogue.push(format!(
                            "%{} = xor i1 true, %{}",
                            eval.register.to_string(),
                            inner_eval.register.to_string()
                        ));
                    }
                    _ => {
                        panic!("Unsupported unary operator: {}", op);
                    }
                }
            }
            _ => {
                panic!("Unsupported expression type in LLVM IR transformation");
            }
        }

        eval
    }
}
