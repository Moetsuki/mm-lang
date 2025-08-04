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
    pub id: u64,
    pub var_type: Type,
}

impl Register {
    pub fn new(var_type: Type) -> Self {
        let id = REGISTER_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Register { id, var_type }
    }

    pub fn llvm_type(&self) -> String {
        match self.var_type {
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
            Type::ToBeEvaluated => "i64".to_string(),
            _ => panic!("Unsupported type {} for LLVM register", self.var_type),
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
        self.main_prologue.push(format!("entry:\n"));

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
            register: Register::new(Type::ToBeEvaluated),
        };

        self.scope.enter_scope();

        for statement in statements {
            match statement {
                Statement::VariableDecl {
                    identifier: var_info,
                    value,
                } => {
                    let value_eval = self.transform_expression(value.clone());
                    eval.prologue
                        .instructions
                        .extend(value_eval.prologue.instructions);
                    eval.epilogue
                        .instructions
                        .extend(value_eval.epilogue.instructions);

                    // Handle type coercion if needed
                    let final_register = if value_eval.register.var_type != var_info.var_type {
                        self.insert_type_conversion(
                            &mut eval,
                            &value_eval.register,
                            &var_info.var_type,
                        )
                    } else {
                        value_eval.register
                    };

                    // Use the final_register directly as the variable's register
                    self.scope.insert(final_register.clone(), var_info.clone());
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

                        // replace the register with the one from rhs_eval
                        eval.register = rhs_eval.register.clone();

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
                    let cmp_register = Register::new(Type::Bool);
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
                    let return_type_llvm = self.type_to_llvm(&ret_type.clone());

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
                    self.code.push(format!("entry:\n"));

                    // Set up function scope
                    self.scope.enter_scope();

                    // Add parameters to scope
                    for param in params {
                        let param_register = Register::new(param.var_type.clone());
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
                    let func_register = Register::new(ret_type.clone());
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
                        return_eval.register.llvm_type(),
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
            register: Register::new(Type::ToBeEvaluated),
        };

        match expr {
            Expression::Number(value) => {
                eval.epilogue.push(format!(
                    "%{} = add i64 0, {}",
                    eval.register.to_string(),
                    value
                ));
                eval.register.var_type = Type::I64;
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

                eval.register.var_type = Type::String;
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

                // Determine the result type (promote to larger type)
                let result_type = self.determine_binary_op_result_type(
                    &left_eval.register.var_type,
                    &right_eval.register.var_type,
                );

                // Convert operands to result type if needed
                let left_converted = if left_eval.register.var_type != result_type {
                    self.insert_type_conversion(&mut eval, &left_eval.register, &result_type)
                } else {
                    left_eval.register
                };

                let right_converted = if right_eval.register.var_type != result_type {
                    self.insert_type_conversion(&mut eval, &right_eval.register, &result_type)
                } else {
                    right_eval.register
                };

                let result_llvm_type = self.type_to_llvm(&result_type);

                match op.as_str() {
                    "+" => {
                        eval.epilogue.push(format!(
                            "%{} = add {} %{}, %{}",
                            eval.register.to_string(),
                            result_llvm_type,
                            left_converted.to_string(),
                            right_converted.to_string()
                        ));
                    }
                    "-" => {
                        eval.epilogue.push(format!(
                            "%{} = sub {} %{}, %{}",
                            eval.register.to_string(),
                            result_llvm_type,
                            left_converted.to_string(),
                            right_converted.to_string()
                        ));
                    }
                    "*" => {
                        eval.epilogue.push(format!(
                            "%{} = mul {} %{}, %{}",
                            eval.register.to_string(),
                            result_llvm_type,
                            left_converted.to_string(),
                            right_converted.to_string()
                        ));
                    }
                    "/" => {
                        eval.epilogue.push(format!(
                            "%{} = sdiv {} %{}, %{}",
                            eval.register.to_string(),
                            result_llvm_type,
                            left_converted.to_string(),
                            right_converted.to_string()
                        ));
                    }
                    ">" => {
                        eval.epilogue.push(format!(
                            "%{} = icmp gt {} %{}, %{}",
                            eval.register.to_string(),
                            result_llvm_type,
                            left_converted.to_string(),
                            right_converted.to_string()
                        ));
                    }
                    "<" => {
                        eval.epilogue.push(format!(
                            "%{} = icmp lt {} %{}, %{}",
                            eval.register.to_string(),
                            result_llvm_type,
                            left_converted.to_string(),
                            right_converted.to_string()
                        ));
                    }
                    ">=" => {
                        eval.epilogue.push(format!(
                            "%{} = icmp ge {} %{}, %{}",
                            eval.register.to_string(),
                            result_llvm_type,
                            left_converted.to_string(),
                            right_converted.to_string()
                        ));
                    }
                    "<=" => {
                        eval.epilogue.push(format!(
                            "%{} = icmp le {} %{}, %{}",
                            eval.register.to_string(),
                            result_llvm_type,
                            left_converted.to_string(),
                            right_converted.to_string()
                        ));
                    }
                    _ => {
                        panic!(
                            "LLVM Expression transform: unsupported binary operator: {}",
                            op
                        );
                    }
                }

                eval.register.var_type = result_type;
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
            Expression::Cast { expr, target_type } => {
                let inner_eval = self.transform_expression(*expr);
                eval.prologue
                    .instructions
                    .extend(inner_eval.prologue.instructions);
                eval.prologue
                    .instructions
                    .extend(inner_eval.epilogue.instructions);

                let source_type = &inner_eval.register.llvm_type();
                let target_llvm = self.type_to_llvm(&target_type);

                match (source_type.as_str(), target_llvm.as_str()) {
                    // Integer truncation (larger to smaller)
                    ("i64", "i32") | ("i64", "i16") | ("i64", "i8") => {
                        eval.epilogue.push(format!(
                            "%{} = trunc {} %{} to {}",
                            eval.register.to_string(),
                            source_type,
                            inner_eval.register.to_string(),
                            target_llvm
                        ));
                    }

                    // Integer extension (smaller to larger)
                    ("i32", "i64") | ("i16", "i64") | ("i8", "i64") => {
                        // Use sext for signed extension, zext for unsigned
                        eval.epilogue.push(format!(
                            "%{} = sext {} %{} to {}",
                            eval.register.to_string(),
                            source_type,
                            inner_eval.register.to_string(),
                            target_llvm
                        ));
                    }

                    // Float to integer
                    ("float", "i32") | ("float", "i64") | ("double", "i32") | ("double", "i64") => {
                        eval.epilogue.push(format!(
                            "%{} = fptosi {} %{} to {}",
                            eval.register.to_string(),
                            source_type,
                            inner_eval.register.to_string(),
                            target_llvm
                        ));
                    }

                    // Integer to float
                    ("i32", "float") | ("i64", "float") | ("i32", "double") | ("i64", "double") => {
                        eval.epilogue.push(format!(
                            "%{} = sitofp {} %{} to {}",
                            eval.register.to_string(),
                            source_type,
                            inner_eval.register.to_string(),
                            target_llvm
                        ));
                    }

                    // Float precision conversion
                    ("float", "double") => {
                        eval.epilogue.push(format!(
                            "%{} = fpext {} %{} to {}",
                            eval.register.to_string(),
                            source_type,
                            inner_eval.register.to_string(),
                            target_llvm
                        ));
                    }
                    ("double", "float") => {
                        eval.epilogue.push(format!(
                            "%{} = fptrunc {} %{} to {}",
                            eval.register.to_string(),
                            source_type,
                            inner_eval.register.to_string(),
                            target_llvm
                        ));
                    }

                    // Boolean conversions
                    ("i1", "i32") | ("i1", "i64") => {
                        eval.epilogue.push(format!(
                            "%{} = zext {} %{} to {}",
                            eval.register.to_string(),
                            source_type,
                            inner_eval.register.to_string(),
                            target_llvm
                        ));
                    }

                    // Same type (no-op)
                    (src, tgt) if src == tgt => {
                        eval.register = inner_eval.register;
                        return eval;
                    }

                    _ => {
                        panic!("Unsupported cast from {} to {}", source_type, target_llvm);
                    }
                }

                eval.register.var_type = target_type;
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

    fn insert_type_conversion(
        &self,
        eval: &mut Evaluation,
        from_register: &Register,
        target_type: &Type,
    ) -> Register {
        if from_register.var_type == *target_type {
            return from_register.clone();
        }

        let new_register = Register::new(target_type.clone());
        let from_llvm = from_register.llvm_type();
        let to_llvm = self.type_to_llvm(target_type);

        match (from_llvm.as_str(), to_llvm.as_str()) {
            // Truncation from i64
            ("i64", "i32") | ("i64", "i16") | ("i64", "i8") | ("i64", "i1") => {
                eval.epilogue.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Truncation from i32
            ("i32", "i16") | ("i32", "i8") | ("i32", "i1") => {
                eval.epilogue.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Truncation from i16
            ("i16", "i8") | ("i16", "i1") => {
                eval.epilogue.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Truncation from i8
            ("i8", "i1") => {
                eval.epilogue.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Sign extension to i64
            ("i32", "i64") | ("i16", "i64") | ("i8", "i64") | ("i1", "i64") => {
                eval.epilogue.push(format!(
                    "%{} = sext {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Sign extension to i32
            ("i8", "i32") | ("i16", "i32") | ("i1", "i32") => {
                eval.epilogue.push(format!(
                    "%{} = sext {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Sign extension to i16
            ("i8", "i16") | ("i1", "i16") => {
                eval.epilogue.push(format!(
                    "%{} = sext {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Sign extension to i8
            ("i1", "i8") => {
                eval.epilogue.push(format!(
                    "%{} = zext {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            _ => panic!(
                "Unsupported automatic conversion from {} to {}",
                from_llvm, to_llvm
            ),
        }

        new_register
    }

    fn determine_binary_op_result_type(&self, left_type: &Type, right_type: &Type) -> Type {
        use crate::types::Type::*;

        match (left_type, right_type) {
            // Same types
            (I8, I8) => I8,
            (I16, I16) => I16,
            (I32, I32) => I32,
            (I64, I64) => I64,

            // Mixed types - promote to larger type
            (I8, I16) | (I16, I8) => I16,
            (I8, I32) | (I32, I8) => I32,
            (I8, I64) | (I64, I8) => I64,
            (I16, I32) | (I32, I16) => I32,
            (I16, I64) | (I64, I16) => I64,
            (I32, I64) | (I64, I32) => I64,

            (F32, F32) => F32,
            (F64, F64) => F64,

            // Mixed float types - promote to larger type
            (F32, F64) | (F64, F32) => F64,

            _ => I64, // Default fallback
        }
    }
}
