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

    pub fn insert_top(&mut self, register: Register, var_info: Variable) {
        if let Some(current) = self.stack.first_mut() {
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

    pub fn find_function(&self, name: &str) -> Option<&Variable> {
        // Search only in the global scope (first element of stack)
        if let Some(global_scope) = self.stack.first() {
            for var in global_scope.symbols.values() {
                if var.name == name {
                    if let Type::Function(_, _) = var.var_type {
                        return Some(var);
                    }
                }
            }
        }
        None
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
    llvm_type: String,
}

impl Register {
    pub fn new() -> Self {
        let id = REGISTER_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Register {
            id,
            llvm_type: "none".to_string(),
        }
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

    fn type_to_llvm(&self, t: &Type) -> String {
        match t {
            Type::Bool => "i1".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "i8".to_string(),
            Type::U16 => "i16".to_string(),
            Type::U32 => "i32".to_string(),
            Type::U64 => "i64".to_string(),
            Type::F32 => "float".to_string(),
            Type::F64 => "double".to_string(),
            Type::String => "i8*".to_string(),
            Type::NoneType => "void".to_string(),
            _ => "i32".to_string(), // Default fallback
        }
    }

    pub fn compile(&mut self) {
        self.prologue.push(format!("\n; PROLOGUE"));
        self.prologue
            .push(format!("; LLVM IR generated from MM-lang\n"));

        self.main_prologue.push(format!("\n; MAIN PROLOGUE"));
        //self.main_prologue.push(format!("@main = global i32 0"));
        self.main_prologue.push(format!("define i32 @main() {{"));
        self.main_prologue.push(format!("entry:"));

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

    #[track_caller]
    pub fn transform(&mut self) -> Evaluation {
        let statements = self.ast.get().unwrap().statements.clone();
        self.transform_block(&statements)
    }

    #[track_caller]
    pub fn transform_block(&mut self, statements: &[Statement]) -> Evaluation {
        let mut eval = Evaluation {
            prologue: IR::new(),
            epilogue: IR::new(),
            register: Register::new(),
        };

        self.scope.enter_scope();

        for statement in statements {
            match statement {
                Statement::VariableDecl { identifier, value } => {
                    let var_info = Variable {
                        name: identifier.name.clone(),
                        var_type: Type::ToBeEvaluated,
                    };
                    let var_register = Register::new();
                    self.scope.insert(var_register, var_info.clone());

                    let value_eval = self.transform_expression(value.clone());
                    eval.prologue
                        .instructions
                        .extend(value_eval.prologue.instructions);

                    eval.epilogue.push(format!(
                        "%{} = {}",
                        eval.register.to_string(),
                        value_eval.register.to_string()
                    ));
                }
                Statement::Assignment {
                    identifier: lhs,
                    value: rhs,
                } => match lhs {
                    Expression::Variable(var_info) => {
                        let rhs_eval = self.transform_expression(rhs.clone());
                        eval.prologue
                            .instructions
                            .extend(rhs_eval.prologue.instructions);

                        eval.epilogue.push(format!(
                            "%{} = {}",
                            eval.register.to_string(),
                            rhs_eval.register.to_string()
                        ));

                        self.scope.insert(eval.register.clone(), var_info.clone());
                    }
                    _ => {
                        panic!("Unsupported lhs expression in LLVM IR transformation");
                    }
                },
                Statement::If {
                    condition,
                    then_block,
                    else_block,
                } => {
                    let condition_eval = self.transform_expression(condition.clone());
                    eval.prologue
                        .instructions
                        .extend(condition_eval.prologue.instructions);
                    eval.prologue
                        .instructions
                        .extend(condition_eval.epilogue.instructions);

                    // Create labels
                    let true_label = format!("if_true_{}", eval.register.id);
                    let false_label = format!("if_false_{}", eval.register.id);
                    let done_label = format!("if_done_{}", eval.register.id);

                    // Compare condition with zero
                    let cmp_register = Register::new();
                    eval.epilogue.push(format!(
                        "%{} = icmp ne i64 %{}, 0",
                        cmp_register.to_string(),
                        condition_eval.register.to_string()
                    ));

                    // Branch based on comparison
                    eval.epilogue.push(format!(
                        "br i1 %{}, label %{}, label %{}",
                        cmp_register.to_string(),
                        true_label,
                        if else_block.is_some() {
                            &false_label
                        } else {
                            &done_label
                        }
                    ));

                    // True block
                    eval.epilogue.push(format!("{}:", true_label));
                    let then_eval = self.transform_block(&then_block.statements);
                    eval.epilogue
                        .instructions
                        .extend(then_eval.prologue.instructions);
                    eval.epilogue
                        .instructions
                        .extend(then_eval.epilogue.instructions);
                    eval.epilogue.push(format!("br label %{}", done_label));

                    // False block (if exists)
                    if let Some(else_block) = else_block {
                        eval.epilogue.push(format!("{}:", false_label));
                        let else_eval = self.transform_block(&else_block.statements);
                        eval.epilogue
                            .instructions
                            .extend(else_eval.prologue.instructions);
                        eval.epilogue
                            .instructions
                            .extend(else_eval.epilogue.instructions);
                        eval.epilogue.push(format!("br label %{}", done_label));
                    }

                    // Done label
                    eval.epilogue.push(format!("{}:", done_label));
                }
                Statement::Function {
                    name,
                    ret_type,
                    params,
                    body,
                } => {
                    // Get LLVM types
                    let return_type_llvm = self.type_to_llvm(ret_type);

                    // Generate function signature
                    let param_str = params
                        .iter()
                        .map(|p| format!("{} %{}", self.type_to_llvm(&p.var_type), p.name))
                        .collect::<Vec<_>>()
                        .join(", ");

                    self.code.push(format!(
                        "\ndefine {} @{}({}) {{",
                        return_type_llvm, name, param_str
                    ));
                    self.code.push(format!("entry:"));

                    // Set up function scope
                    self.scope.enter_scope();

                    // Add parameters to scope
                    for param in params {
                        let param_register = Register::new();
                        // In LLVM, function parameters are already available as registers
                        // So we can directly map them
                        self.scope.insert(param_register.clone(), param.clone());
                        self.code.push(format!(
                            "%{} = add {} 0, %{}",
                            param_register.to_string(),
                            self.type_to_llvm(&param.var_type),
                            param.name
                        ));
                    }

                    // Make sure the first level of statements have a return statement
                    if !body.statements.iter().any(|s| match s {
                        Statement::Return { .. } => true,
                        _ => false,
                    }) {
                        panic!("Function '{}' must have a return statement", name);
                    }

                    // Transform function body
                    let body_eval = self.transform_block(&body.statements);
                    self.code
                        .instructions
                        .extend(body_eval.prologue.instructions);
                    self.code
                        .instructions
                        .extend(body_eval.epilogue.instructions);

                    // Find the return statement and check if its type matches the function's return type

                    self.code.push(format!("}}"));

                    self.scope.exit_scope();

                    // Register function in global scope
                    let var_info = Variable {
                        name: name.clone(),
                        var_type: Type::Function(
                            params.iter().map(|p| p.var_type.clone()).collect(),
                            Box::new(ret_type.clone()),
                        ),
                    };
                    let func_register = Register::new();
                    self.scope.insert_top(func_register, var_info);
                }
                Statement::Call { callee, args } => {
                    // Get function name from callee
                    let func_name = match callee {
                        Expression::Variable(var) => var.name.clone(),
                        _ => panic!("Function calls must use variable names"),
                    };

                    // Look up function in global scope to get return type and parameter types
                    let func_var = self.scope.find_function(&func_name).expect(&format!(
                        "Function '{}' not found in global scope",
                        func_name
                    ));

                    let (param_types, return_type) = match &func_var.var_type {
                        Type::Function(params, ret) => (params.clone(), ret.as_ref().clone()),
                        _ => panic!("Variable '{}' is not a function", func_name),
                    };

                    // Handle function call arguments
                    let mut arg_evals = Vec::new();
                    for arg in args {
                        let arg_eval = self.transform_expression(arg.clone());
                        eval.prologue
                            .instructions
                            .extend(arg_eval.prologue.instructions.clone());
                        eval.prologue
                            .instructions
                            .extend(arg_eval.epilogue.instructions.clone());
                        arg_evals.push(arg_eval);
                    }

                    // Get LLVM return type
                    let return_type_llvm = self.type_to_llvm(&return_type);

                    // Generate function call with proper types
                    let args_str = arg_evals
                        .iter()
                        .zip(param_types.iter())
                        .map(|(arg_eval, param_type)| {
                            format!(
                                "{} %{}",
                                self.type_to_llvm(param_type),
                                arg_eval.register.to_string()
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ");

                    eval.epilogue.push(format!(
                        "%{} = call {} @{}({})",
                        eval.register.to_string(),
                        return_type_llvm,
                        func_name,
                        args_str
                    ));
                }
                Statement::Return { value } => {
                    let return_eval = self.transform_expression(value.clone());
                    eval.prologue
                        .instructions
                        .extend(return_eval.prologue.instructions);
                    eval.prologue
                        .instructions
                        .extend(return_eval.epilogue.instructions);

                    // Add return statement
                    eval.epilogue.push(format!(
                        "ret {} %{}",
                        return_eval.register.llvm_type,
                        return_eval.register.to_string()
                    ));
                }
                _ => {
                    panic!(
                        "Unsupported statement type in LLVM IR transformation\n[Statement]:\n {:?}",
                        statement
                    );
                }
            }
        }

        self.scope.exit_scope();

        eval
    }

    #[track_caller]
    pub fn transform_expression(&mut self, expr: Expression) -> Evaluation {
        let mut eval = Evaluation {
            prologue: IR::new(),
            epilogue: IR::new(),
            register: Register::new(),
        };

        match expr {
            Expression::Number(value) => {
                eval.epilogue.push(format!(
                    "%{} = add i64 0, {}",
                    eval.register.to_string(),
                    value
                ));
                eval.register.llvm_type = "i64".to_string();
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

                // %msg = getelementptr [6 x i8], [6 x i8]* @.str, i64 0, i64 0
                eval.epilogue.push(format!(
                    "%{} = getelementptr [{} x i8], [{} x i8]* @.{}, i64 0, i64 0",
                    eval.register.to_string(),
                    string_data.length() + 1,
                    string_data.length() + 1,
                    string_data.name()
                ));

                eval.register.llvm_type = "i8*".to_string(); // Pointer to string
            }
            Expression::Variable(var) => {
                let var_register = self
                    .scope
                    .find(&var)
                    .expect(format!("Variable {} not found in scope", var.name).as_str());

                eval.register = var_register.clone();
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
                            "%{} = add i64 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                        eval.register.llvm_type = "i64".to_string();
                    }
                    "-" => {
                        eval.epilogue.push(format!(
                            "%{} = sub i64 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                        eval.register.llvm_type = "i64".to_string();
                    }
                    "*" => {
                        eval.epilogue.push(format!(
                            "%{} = mul i64 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                        eval.register.llvm_type = "i64".to_string();
                    }
                    "/" => {
                        eval.epilogue.push(format!(
                            "%{} = sdiv i64 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                        eval.register.llvm_type = "i64".to_string();
                    }
                    ">" => {
                        eval.epilogue.push(format!(
                            "%{} = icmp gt i64 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                        eval.register.llvm_type = "i1".to_string(); // Boolean result
                    }
                    "<" => {
                        eval.epilogue.push(format!(
                            "%{} = icmp lt i64 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                        eval.register.llvm_type = "i1".to_string(); // Boolean result
                    }
                    ">=" => {
                        eval.epilogue.push(format!(
                            "%{} = icmp ge i64 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                        eval.register.llvm_type = "i1".to_string(); // Boolean result
                    }
                    "<=" => {
                        eval.epilogue.push(format!(
                            "%{} = icmp le i64 %{}, %{}",
                            eval.register.to_string(),
                            left_eval.register.to_string(),
                            right_eval.register.to_string()
                        ));
                        eval.register.llvm_type = "i1".to_string(); // Boolean result
                    }
                    _ => {
                        panic!(
                            "LLVM Expression transform: unsupported binary operator: {}",
                            op
                        );
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
                            "%{} = sub i64 0, %{}",
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
                panic!(
                    "Unsupported expression type in LLVM IR transformation!\n[Expression]:\n {}",
                    expr
                );
            }
        }

        eval
    }
}
