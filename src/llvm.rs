#![allow(clippy::format_in_format_args)]

use core::panic;

use crate::backtrace;
use crate::block::Block;
use crate::statement::Visibility;
use crate::types::Type;
use crate::variable::Variable;
use crate::{ast::Ast, expression::Expression, statement::Statement};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
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

    pub fn pop_by_name(&mut self, name: &str) -> Option<(Register, Variable)> {
        let mut result: Option<(Register, Variable)> = None;

        self.stack.iter_mut().rev().for_each(|scope| {
            let res = scope.symbols.iter().find(|s| s.1.name == name);

            if let Some(r) = res {
                result = Some((r.0.clone(), r.1.clone()));
            }

            if result.as_ref().is_some()
                && let Some((k, _v)) = scope.symbols.iter().find(|s| s.1.name == name)
            {
                scope.symbols.remove(&k.clone());
            };
        });

        result
    }

    pub fn find_by_name(&self, name: &str) -> Option<(Register, Variable)> {
        let mut result: Option<(Register, Variable)> = None;

        self.stack.iter().rev().for_each(|scope| {
            let res = scope.symbols.iter().find(|s| s.1.name == name);

            if let Some(r) = res {
                result = Some((r.0.clone(), r.1.clone()));
            }
        });

        result
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
                if var.name == name
                    && let Type::Function { .. } = var.var_type
                {
                    return Some(var);
                }
            }
        }
        None
    }

    pub fn debug(&self) {
        self.stack.iter().for_each(|entry| {
            entry
                .symbols
                .iter()
                .for_each(|symbol| println!("{:?}\n", symbol));
            println!("-------------------");
        });
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

impl Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.instructions.join("\n"))
    }
}

///
/// Evaluation
///
#[derive(Debug, Clone)]
pub struct Evaluation {
    pub code: IR,
    pub register: Register,
    pub current_class_context: Option<Class>,
    pub current_struct_context: Option<Struct>,
    pub current_func_context: Option<Function>,
}

///
/// Register
///
static REGISTER_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Register {
    pub id: u64,
    pub var_type: Type,
    pub name: String,
}

impl Register {
    #[track_caller]
    pub fn new(var_type: Type) -> Self {
        let id = REGISTER_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        let caller = backtrace::get_immediate_caller();
        println!(
            "{:<30} Register::new({})",
            format!("{}:{}:{}", caller.file, caller.line, caller.column),
            id
        );
        Register {
            id,
            var_type,
            name: String::from(""),
        }
    }

    #[track_caller]
    pub fn new_var(var_type: Type, name: String) -> Self {
        let id = REGISTER_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        let caller = backtrace::get_immediate_caller();
        println!(
            "{:<30} Register::new_var({}_{})",
            format!("{}:{}:{}", caller.file, caller.line, caller.column),
            name,
            id
        );
        Register { id, var_type, name }
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
            Type::String => "ptr".to_string(),
            Type::Void => "void".to_string(),
            Type::Pointer(..) => "ptr".to_string(),
            _ => panic!("Unsupported type {} for LLVM register", self.var_type),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.name.is_empty() {
            write!(f, "reg{}", self.id)
        } else {
            write!(f, "{}_{}", self.name, self.id)
        }
    }
}

///
/// Classes, Structs and Functions
///
#[derive(Debug, Clone)]
pub struct Class {
    pub id: u64,
    pub name: String,
    pub parent: Option<Box<Class>>,
    pub statement: Statement,
    pub all_fields: Vec<((Variable, Visibility), String)>,
    pub all_methods: Vec<((Box<Statement>, Visibility), String)>,
}

static CLASS_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

impl Class {
    pub fn new(name: String, parent: Option<Box<Class>>, class_stm: Statement) -> Self {
        let id = CLASS_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Class {
            id,
            name,
            parent,
            statement: class_stm,
            all_fields: Vec::new(),
            all_methods: Vec::new(),
        }
    }

    pub fn undecorated_name(&self) -> String {
        format!("class{}", self.id)
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub id: u64,
    pub name: String,
    pub parent: Option<Box<Struct>>,
    pub statement: Statement,
    pub all_fields: Vec<(Variable, String)>,
}

impl Struct {
    pub fn new(name: String, parent: Option<Box<Struct>>, struct_stm: Statement) -> Self {
        let id = STRUCT_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Struct {
            id,
            name,
            parent,
            statement: struct_stm,
            all_fields: Vec::new(),
        }
    }

    pub fn undecorated_name(&self) -> String {
        format!("struct{}", self.id)
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }
}

static STRUCT_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone)]
pub struct Function {
    pub id: u64,
    pub name: String,
    pub owner: Option<Class>,
    pub statement: Statement,
    pub args: Vec<Variable>,
    pub ret_type: Type,
}

impl Function {
    pub fn new(
        name: String,
        owner: Option<Class>,
        statement: Statement,
        args: Vec<Variable>,
        ret_type: Type,
    ) -> Self {
        let id = FUNCTION_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Function {
            id,
            name,
            owner,
            statement,
            args,
            ret_type,
        }
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn undecorated_name(&self) -> String {
        format!("function{}", self.id)
    }
}

static FUNCTION_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

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

impl Display for StringData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "str{} = \"{}\"", self.id, self.value)
    }
}

///
/// LLVM
///
#[allow(clippy::upper_case_acronyms)]
pub struct LLVM {
    forward_decls: IR,
    prologue: IR,
    main_prologue: IR,
    main: IR,
    main_epilogue: IR,
    code: IR,
    epilogue: IR,
    ast: Ast,
    scope: Scope,
    class_definitions: HashMap<String, Class>,
    struct_definitions: HashMap<String, Struct>,
    current_class_scope: Option<Box<Class>>,
    current_struct_scope: Option<Box<Struct>>,
    current_func_scope: Option<Box<Function>>,
}

impl LLVM {
    pub fn new(ast: Ast) -> Self {
        LLVM {
            forward_decls: IR::new(),
            prologue: IR::new(),
            epilogue: IR::new(),
            main_prologue: IR::new(),
            main: IR::new(),
            main_epilogue: IR::new(),
            code: IR::new(),
            ast,
            scope: Scope::new(),
            class_definitions: HashMap::new(),
            struct_definitions: HashMap::new(),
            current_class_scope: None,
            current_struct_scope: None,
            current_func_scope: None,
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
            Type::String => "ptr".to_string(),
            Type::Void => "void".to_string(),
            Type::Pointer(_) => "ptr".to_string(),
            _ => panic!("Unsupported type {} for LLVM IR", t),
        }
    }

    fn type_is_signed(&self, t: &Type) -> bool {
        match t {
            Type::I8 => true,
            Type::I16 => true,
            Type::I32 => true,
            Type::I64 => true,
            Type::U8 => false,
            Type::U16 => false,
            Type::U32 => false,
            Type::U64 => false,
            _ => panic!("Unsupported type {} for LLVM IR type_is_signed", t),
        }
    }

    #[track_caller]
    pub fn compile(&mut self) {
        self.prologue.push("\n; PROLOGUE".to_string());
        self.prologue
            .push("; LLVM IR generated from MM-lang\n".to_string());

        self.scope.enter_scope(); // Enter binding scope

        self.enter_main();

        self.generate_c_bindings();

        self.main_prologue.push("\n; MAIN PROLOGUE".to_string());
        //self.main_prologue.push(format!("@main = global i32 0"));
        self.main_prologue.push("define i32 @main() {".to_string());
        self.main_prologue.push("entry:\n".to_string());

        let eval = self.transform();

        self.main.instructions.extend(eval.code.instructions);

        self.main_epilogue.push("\n; MAIN EPILOGUE".to_string());
        self.main_epilogue.push("  ret i32 0".to_string());
        self.main_epilogue.push("}".to_string());

        self.scope.exit_scope(); // Exit binding scope
    }

    pub fn output(&self) -> String {
        let mut output = String::new();
        output.push_str(&self.forward_decls.to_string());
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
        self.transform_block(&statements).unwrap()
    }

    #[track_caller]
    pub fn transform_block(&mut self, statements: &[Statement]) -> Option<Evaluation> {
        let mut eval = Evaluation {
            code: IR::new(),
            register: Register::new(Type::ToBeEvaluated("invalid_reg".to_string())),
            current_class_context: None,
            current_struct_context: None,
            current_func_context: None,
        };

        self.scope.enter_scope();

        for statement in statements {
            match statement {
                Statement::Block { body } => {
                    let result = self.transform_block(&body.statements);

                    if let Some(r) = result {
                        eval.code.instructions.extend(r.code.instructions);
                    }
                }
                Statement::VariableDecl {
                    identifier: var_info,
                    value,
                } => {
                    let value_eval = self.transform_expression(value.clone());
                    eval.code
                        .instructions
                        .extend(value_eval.code.instructions.clone());

                    // println!("{{Statement::VariableDecl}}\n");
                    // println!("{:?}", statement.clone());

                    // println!("  + [Identifier]");
                    // println!("      | [var_info]");
                    // println!("      +-----+ {:?}\n", var_info.clone());
                    // println!("      | [value]");
                    // println!("      +-----+ {}\n", value.clone());
                    // println!("      | [value_eval]");
                    // println!("      +-----+ {:?}\n", value_eval.clone());
                    // println!("  + [Value]");
                    // println!("      | [value_eval.register.var_type]");
                    // println!("      +-----+ {}\n", value_eval.register.var_type.clone());
                    // println!("  + [Type]");
                    // println!("      | [var_info.var_type]");
                    // println!("      +-----+ {}\n", var_info.var_type.clone());

                    // Handle type coercion if needed
                    let coerced_register = if value_eval.register.var_type != var_info.var_type {
                        match &var_info.var_type {
                            Type::ToBeEvaluated(..) => value_eval.register.clone(),
                            _ => self.insert_type_conversion(
                                &mut eval.code,
                                &value_eval.register,
                                &var_info.var_type,
                            ),
                        }
                    } else {
                        value_eval.register.clone()
                    };

                    // If its TBE search for class or struct on scope
                    let final_type = match &var_info.var_type {
                        Type::ToBeEvaluated(tbe_type) => self
                            .class_definitions
                            .get(tbe_type)
                            .map(|c| self.get_class_type(&c.name))
                            .unwrap_or(var_info.var_type.clone()),
                        _ => var_info.var_type.clone(),
                    };
                    let mut final_variable = var_info.clone();
                    final_variable.var_type = final_type;

                    let mut new_symbol: (Register, Variable) =
                        (coerced_register.clone(), final_variable.clone());
                    self.scope
                        .insert(new_symbol.0.clone(), new_symbol.1.clone());
                    // println!("     + New Symbol =\n{:?}", new_symbol.clone());
                }
                Statement::Assignment {
                    identifier: lhs,
                    value: rhs,
                } => match lhs {
                    Expression::Variable(_var_info) => {
                        let rhs_eval = self.transform_expression(rhs.clone());
                        eval.code
                            .instructions
                            .extend(rhs_eval.code.instructions.clone());

                        let lhs_eval = self.transform_expression(lhs.clone());
                        eval.code
                            .instructions
                            .extend(lhs_eval.code.instructions.clone());

                        println!("lhs = {:?}", lhs);
                        println!("rhs = {:?}", rhs);
                        println!("rhs_eval = {:?}", rhs_eval.clone());
                        println!("lhs_eval = {:?}", lhs_eval.clone());

                        // Handle type coercion if needed
                        let coerced_register =
                            if rhs_eval.register.var_type != lhs_eval.register.var_type {
                                self.insert_type_conversion(
                                    &mut eval.code,
                                    &rhs_eval.register,
                                    &lhs_eval.register.var_type,
                                )
                            } else {
                                rhs_eval.register
                            };

                        let result_register = Register::new(coerced_register.var_type.clone());

                        eval.code.instructions.push(format!(
                            "%{} = add {} 0, %{}",
                            result_register,
                            self.type_to_llvm(&lhs_eval.register.var_type),
                            coerced_register,
                        ));

                        eval.register = lhs_eval.register.clone();

                        // Check if it already exists in scope and replce the register with a new
                        // one that holds the result
                        let mut res = self.scope.pop_by_name(&_var_info.name);

                        if let Some(r) = res.as_mut() {
                            r.0 = result_register.clone();

                            self.scope.insert(r.0.clone(), r.1.clone());
                        }

                        if res.is_none() {
                            self.scope.insert(result_register, _var_info.clone());
                        }
                    }
                    Expression::FieldAccess { object: lhs, field } => {
                        let object_eval = self.transform_expression(*lhs.clone());

                        // Ensure the object is a pointer type
                        if let Type::Pointer(inner_type) = object_eval.register.clone().var_type {
                            eval.code
                                .instructions
                                .extend(object_eval.code.instructions);

                            // Get the class or struct name from the inner type
                            let object_name = if let Type::Class { name, .. } = *inner_type {
                                name
                            } else if let Type::Struct { name, .. } = *inner_type {
                                name
                            } else {
                                panic!("Object must be a class or struct type for field access");
                            };

                            // Find the field in the class or struct definition
                            let (field_index, field_var) =
                                if let Some(class) = self.class_definitions.get(&object_name) {
                                    class
                                        .all_fields
                                        .iter()
                                        .enumerate()
                                        .find(|(_, (f, _))| f.0.name == *field)
                                        .map(|(i, (f, _))| (i, f.0.clone()))
                                } else if let Some(struct_def) =
                                    self.struct_definitions.get(&object_name)
                                {
                                    struct_def
                                        .all_fields
                                        .iter()
                                        .enumerate()
                                        .find(|(_, (f, _))| f.name == *field)
                                        .map(|(i, (f, _))| (i, f.clone()))
                                } else {
                                    panic!(
                                        "Field '{}' not found in object type '{}'",
                                        field, object_name
                                    );
                                }
                                .unwrap_or_else(|| {
                                    panic!(
                                        "Field '{}' not found in object type '{}'",
                                        field, object_name
                                    )
                                });

                            // Create a new register for the field
                            let field_register = Register::new(field_var.var_type.clone());

                            // Calculate rhs of the assignment
                            let rhs_eval = self.transform_expression(rhs.clone());
                            eval.code
                                .instructions
                                .extend(rhs_eval.code.instructions);

                            // Handle type coercion if needed
                            let coerced_register =
                                if rhs_eval.register.var_type != field_var.var_type {
                                    self.insert_type_conversion(
                                        &mut eval.code,
                                        &rhs_eval.register,
                                        &field_var.var_type,
                                    )
                                } else {
                                    rhs_eval.register
                                };

                            // Generate LLVM IR for the assignment
                            eval.code.push(format!(
                                "%{} = getelementptr inbounds %{}, ptr %{}, i64 0, i32 {}",
                                field_register,
                                object_name,
                                object_eval.register,
                                field_index + 1 // +1 because the first element is the vtable pointer
                            ));

                            eval.code.push(format!(
                                "store {} %{}, ptr %{}",
                                self.type_to_llvm(&coerced_register.var_type),
                                coerced_register,
                                field_register
                            ));

                            eval.register = coerced_register;
                        } else {
                            panic!("Object must be a pointer type for field access");
                        }
                    }
                    _ => {
                        panic!(
                            "Unsupported lhs expression in LLVM IR transformation!\n [Expression]: {:?}",
                            lhs
                        );
                    }
                },
                Statement::If {
                    condition,
                    then_block,
                    else_block,
                } => {
                    let condition_eval = self.transform_expression(condition.clone());
                    eval.code
                        .instructions
                        .extend(condition_eval.code.instructions);

                    // Create labels
                    let true_label = format!("if_true_{}", eval.register.id);
                    let false_label = format!("if_false_{}", eval.register.id);
                    let done_label = format!("if_done_{}", eval.register.id);

                    // Compare condition with zero
                    let cmp_register = Register::new(Type::Bool);
                    eval.code.push(format!(
                        "%{} = icmp ne i1 %{}, 0",
                        cmp_register, condition_eval.register
                    ));

                    // Branch based on comparison
                    eval.code.push(format!(
                        "br i1 %{}, label %{}, label %{}",
                        cmp_register,
                        true_label,
                        if else_block.is_some() {
                            &false_label
                        } else {
                            &done_label
                        }
                    ));

                    // True block
                    eval.code.push(format!("{}:", true_label));
                    let then_eval = self
                        .transform_block(&then_block.statements)
                        .expect("Failed to compile true-statements block");
                    eval.code
                        .instructions
                        .extend(then_eval.code.instructions);
                    eval.code.push(format!("br label %{}", done_label));

                    // False block (if exists)
                    if let Some(else_block) = else_block {
                        eval.code.push(format!("{}:", false_label));
                        let else_eval = self
                            .transform_block(&else_block.statements)
                            .expect("Failed to compile false-statements block");
                        eval.code
                            .instructions
                            .extend(else_eval.code.instructions);
                        eval.code.push(format!("br label %{}", done_label));
                    }

                    // Done label
                    eval.code.push(format!("{}:", done_label));
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
                    self.code.push("entry:\n".to_string());

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
                            param_register,
                            self.type_to_llvm(&param.var_type),
                            param.name
                        ));
                    }

                    // If return type is not Void
                    // Make sure the first level of statements have a return statement
                    if ret_type != &Type::Void
                        && !body
                            .statements
                            .iter()
                            .any(|s| matches!(s, Statement::Return { .. }))
                    {
                        panic!("Function '{}' must have a return statement", name);
                    }

                    // Set function to current context
                    self.current_func_scope = Some(Box::new(Function::new(
                        name.clone(),
                        self.current_class_scope.as_ref().map(|c| *c.clone()),
                        Statement::Function {
                            name: name.clone(),
                            params: params.clone(),
                            ret_type: ret_type.clone(),
                            body: body.clone(),
                        },
                        params.clone(),
                        ret_type.clone(),
                    )));

                    // Transform function body
                    let mut body_eval = self
                        .transform_block(&body.statements)
                        .expect("Failed to compile function body");

                    self.enter_main(); // Clear function context

                    // Check the body_eval register, if we don't have a match
                    // We need to fix the last instruction.
                    if body_eval.register.var_type != *ret_type {
                        // Make sure the last instruction of the epilogue is a ret
                        if let Some(last) = body_eval.code.instructions.last()
                            && !last.starts_with("ret")
                        {
                            panic!(
                                "Function '{}' return type mismatch: expected {}, got {}",
                                name, ret_type, body_eval.register.var_type
                            );
                        };
                        // Remove the last instruction
                        body_eval.code.instructions.pop();

                        // Coerce the return value register to the expected return type in a new register
                        let coerced_register = self.insert_type_conversion(
                            &mut body_eval.code,
                            &body_eval.register,
                            ret_type,
                        );

                        // Return the coerced register
                        body_eval.code.push(format!(
                            "ret {} %{}",
                            self.type_to_llvm(&coerced_register.var_type),
                            coerced_register
                        ));
                    }

                    self.code
                        .instructions
                        .extend(body_eval.code.instructions);
                    self.code.push("}".to_string());

                    self.scope.exit_scope();

                    // Register function in global scope
                    let var_info = Variable {
                        name: name.clone(),
                        var_type: Type::Function {
                            name: name.clone(),
                            args: params.iter().map(|p| p.var_type.clone()).collect(),
                            ret_type: Box::new(ret_type.clone()),
                            is_variadic: false,
                        },
                    };
                    let func_register = Register::new(ret_type.clone());
                    eval.register = func_register.clone();
                    self.scope.insert_top(func_register.clone(), var_info);
                }
                Statement::Call { callee, args } => {
                    match callee {
                        Expression::Variable(var_expr) => {
                            //
                            // Get function and generate its signature for later use
                            //
                            let func_name = var_expr.name.clone();
                            let func_var =
                                self.scope.find_function(&func_name).unwrap_or_else(|| {
                                    panic!("Function '{}' not found in global scope", func_name)
                                }).clone();
                            let func_sig = self.generate_function_signature(
                                func_var.name.clone(),
                                &func_var.var_type.clone(),
                                false,
                                false,
                            );

                            //
                            // Get the arguments and generate arguement string for later use
                            //
                            let mut arg_evals = Vec::new();
                            for arg in args {
                                let arg_eval = self.transform_expression(arg.clone()).clone();
                                eval.code
                                    .instructions
                                    .extend(arg_eval.code.instructions.clone());
                                arg_evals.push(arg_eval.clone());
                            }
                            let (arg_types, ret_type) =
                                if let Type::Function { args, ret_type, .. } = &func_var.var_type {
                                    (args.clone(), ret_type.clone())
                                } else {
                                    panic!("Expected function type for '{}'", func_name);
                                };
                            let args_str = arg_evals
                                .iter()
                                .zip(arg_types.iter())
                                .map(|(arg_eval, param_type)| {
                                    format!(
                                        "{} %{}",
                                        self.type_to_llvm(param_type),
                                        arg_eval.register.clone()
                                    )
                                })
                                .collect::<Vec<_>>()
                                .join(", ");

                            //
                            // Create a new register to store the result
                            //
                            eval.register = Register::new_var(*ret_type, format!("{}_result", func_name.clone()));

                            //
                            // Call the function
                            //
                            eval.code.push(format!(
                                "%{} = call {}({})",
                                eval.register, func_sig, args_str
                            ));
                        }
                        _ => {
                            panic!(
                                "Unsupported callee expression in function call: {:?}",
                                callee
                            );
                        }
                    }
                }
                Statement::Return { value } => {
                    let return_eval = self.transform_expression(value.clone());
                    eval.code
                        .instructions
                        .extend(return_eval.code.instructions);

                    // Get return type from current function context
                    let expected_ret_type = self
                        .current_func_scope
                        .as_ref()
                        .map(|f| f.ret_type.clone())
                        .unwrap_or_else(|| {
                            panic!("Return statement outside of a function context!")
                        });

                    // println!("{{Statement::Return}}");
                    // println!("Inside Function Context");
                    // println!("{:?}", self.current_func_scope);
                    // println!("Expected Return Type: {:?}", expected_ret_type);
                    // println!("Actual Return Type: {:?}", return_eval.register.var_type);

                    // Add type coercion
                    if expected_ret_type != return_eval.register.var_type {
                        // Coerce the return value register to the expected return type in a new register
                        let coerced_register = self.insert_type_conversion(
                            &mut eval.code,
                            &return_eval.register,
                            &expected_ret_type,
                        );
                        eval.register = coerced_register;
                    } else {
                        // Set the evaluation register to the return value of this block
                        // This captures the return type information to be used later
                        eval.register = return_eval.register.clone();
                    }

                    // Add return statement
                    eval.code.push(format!(
                        "ret {} %{}",
                        eval.register.llvm_type(),
                        eval.register
                    ));
                }
                Statement::Class {
                    name,
                    parent,
                    fields,
                    methods,
                } => {
                    //
                    // Handle class transformation
                    //

                    //
                    // NOTES:
                    //
                    // Classes in LLVM IR are not directly supported. So we need to
                    //
                    // 1. Compile a struct for the class
                    // 2. Compile a constructor function for the class
                    // 3. Validate all fields are declared with VariableDecl in the constructor
                    // 4. Compile all methods as functions
                    // 5. Implement a VTABLE for the class if it has methods
                    // 6. Pass `this` around explicitly in method calls
                    //

                    //
                    // EXAMPLE:
                    //
                    // class Animal {
                    //     private name: string;
                    //     private age: i32;
                    //
                    //     public function speak() -> string {
                    //         return "Animal sound";
                    //     }
                    // };
                    //
                    // class Dog : Animal {
                    //     public breed: string;
                    //
                    //     public function speak() -> string {
                    //         return "Woof!";
                    //     }
                    // };

                    //
                    // COMPILES:
                    //
                    // ; Type definitions
                    // %AnimalVTable = type { i8* (%Animal*)* } // Forward declare %Animal
                    // %Animal = type { %AnimalVTable*, i8*, i32 }       ; vtable*, name, age
                    // %Dog    = type { %AnimalVTable*, i8*, i32, i8* }   ; vtable*, name, age, breed
                    //
                    // ; Declare the speak functions
                    // declare i8* @Animal_speak(%Animal*)
                    // declare i8* @Dog_speak(%Animal*)
                    //
                    // ; Insantiate VTables as global variables
                    // @Animal_vtable = global %AnimalVTable { i8* (%Animal*)* @Animal_speak }
                    // @Dog_vtable    = global %AnimalVTable { i8* (%Animal*)* @Dog_speak }

                    // ; Implement the Animal_speak method
                    // define i8* @Animal_speak(%Animal* %this) {
                    //    ...
                    // }
                    //
                    // ; Implement the Dog_speak method
                    // define i8* @Dog_speak(%Animal* %this) {
                    //    ...
                    // }

                    // Check if class already exists by name
                    if self.class_definitions.contains_key(name) {
                        panic!("Class '{}' already defined", name);
                    }

                    // If we have a parent, fetch it from the definitions
                    let parent_class = if let Some(parent_name) = parent {
                        self.class_definitions
                            .get(parent_name)
                            .map(|c| Box::new(c.clone()))
                            .or_else(|| {
                                panic!(
                                    "Parent class '{}' not found for class '{}'",
                                    parent_name, name
                                )
                            })
                    } else {
                        None
                    };

                    // Create a new class definition
                    let mut class_def = Class::new(name.clone(), parent_class, statement.clone());

                    // Collect methods and fields from parent classes up the chain, starting from base class moving up
                    let mut all_methods = Vec::new();
                    let mut all_fields = Vec::new();
                    let mut current_class: Option<&Class> = Some(&class_def);
                    while let Some(class) = current_class {
                        // Add methods and fields from the current class
                        if let Statement::Class {
                            name,
                            methods,
                            fields,
                            ..
                        } = &class.statement
                        {
                            let mut tmp_methods = Vec::new();
                            let mut tmp_fields = Vec::new();
                            for method in methods.iter() {
                                if let Statement::Function { .. } = &method.0.as_ref() {
                                    tmp_methods.push((method.clone(), name.clone()));
                                }
                            }
                            for field in fields.iter() {
                                tmp_fields.push((field.clone(), name.clone()));
                            }
                            // Prepend the class methods and fields to the lists
                            // This ensures that methods from parent classes appear first (on top of the VTable)
                            // And parent fields show first
                            all_methods.splice(0..0, tmp_methods);
                            all_fields.splice(0..0, tmp_fields);
                        }
                        // Move to the parent class
                        current_class = class.parent.as_deref();
                    }

                    class_def.all_methods = all_methods.clone();
                    class_def.all_fields = all_fields.clone();

                    //

                    // Define the class type
                    self.prologue.push(format!(
                        "%{} = type {{\n  {:<55} {}{}\n}}",
                        class_def.name(),
                        format!("ptr{}", if fields.is_empty() { "" } else { "," }),
                        format!("; {}::__VTable\n", class_def.name()),
                        all_fields
                            .iter()
                            .enumerate()
                            .map(|(i, (f, class_name))| {
                                let comma = if i + 1 < all_fields.len() { "," } else { "" };
                                format!(
                                    "  {:<55} ; {}::{}",
                                    format!("{}{}", self.type_to_llvm(&f.0.var_type), comma),
                                    class_name,
                                    f.0.name.clone()
                                )
                            })
                            .collect::<Vec<_>>()
                            .join("\n")
                    ));

                    // Find the constructor function for the class
                    let constructor_name = format!("__{}_init", class_def.name());
                    let destructor_name = format!("__{}_destroy", class_def.name());
                    let _constructor = methods
                        .iter()
                        .find(|(m, _)| matches!(m.as_ref(), Statement::Function { name, .. } if name == &constructor_name))
                        .map(|(m, _)| m.clone())
                        .unwrap_or_else(|| {
                            panic!(
                                "Constructor '{}' not found for class '{}'",
                                constructor_name,
                                class_def.name()
                            )
                        });
                    let _destructor = methods
                        .iter()
                        .find(|(m, _)| matches!(m.as_ref(), Statement::Function { name, .. } if name == &destructor_name))
                        .map(|(m, _)| m.clone())
                        .unwrap_or_else(|| {
                            panic!(
                                "Destructor '{}' not found for class '{}'",
                                destructor_name,
                                class_def.name()
                            )
                        });

                    // Build VTable entries ensuring commas separate types (before comments)
                    let mut method_sigs: Vec<(String, String, String)> = Vec::new();
                    for (method, class_name) in &all_methods {
                        if let Statement::Function {
                            name,
                            ret_type,
                            params,
                            ..
                        } = method.0.as_ref()
                        {
                            let return_type = self.type_to_llvm(ret_type);

                            // First implicit param is always %ClassName*
                            let mut param_types: Vec<String> = vec!["ptr".to_string()];
                            for p in params {
                                param_types.push(self.type_to_llvm(&p.var_type));
                            }

                            let signature = format!("{}({})", return_type, param_types.join(", "));
                            method_sigs.push((signature, class_name.to_string(), name.to_string()));
                        } else {
                            panic!("Expected function statement for class method");
                        }
                    }

                    let vt_body = method_sigs
                        .iter()
                        .enumerate()
                        .map(|(i, (sig, class_name, func_name))| {
                            let comma = if i + 1 < method_sigs.len() { "," } else { " " };
                            format!(
                                "  {} ; {:<48} ; {}::{}",
                                format!("ptr{}", comma),
                                sig,
                                class_name,
                                func_name
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("\n");

                    self.prologue.push(format!(
                        "%{}VTable = type {{\n{}\n}}",
                        class_def.name(),
                        vt_body
                    ));

                    // Insert the class definition into the class definitions
                    self.class_definitions
                        .insert(name.clone(), class_def.clone());

                    // Define all member methods of this class in LLVM IR
                    for method in methods {
                        if let Statement::Function {
                            name,
                            ret_type,
                            params,
                            body,
                        } = method.0.as_ref()
                        {
                            // Generate function signature
                            let return_type_llvm = self.type_to_llvm(ret_type);

                            // Create a register for `self` (the class instance)
                            let self_register = Register::new_var(
                                Type::Pointer(Box::new(Type::Class {
                                    name: class_def.name(),
                                    parent: None,
                                    fields: vec![],  // You can populate this if needed
                                    methods: vec![], // You can populate this if needed
                                })),
                                "self".to_string(),
                            );

                            //
                            // Generate function body <START>
                            //
                            self.current_class_scope = Some(Box::new(class_def.clone()));

                            // Enter method scope
                            self.scope.enter_scope();

                            // Push `self` to the function scope
                            self.scope.insert(
                                self_register.clone(),
                                Variable {
                                    name: "self".to_string(),
                                    var_type: self_register.var_type.clone(),
                                },
                            );

                            // Add other parameters to the scope
                            let mut param_reg_pair = Vec::new();
                            for param in params {
                                let param_register =
                                    Register::new_var(param.var_type.clone(), param.name.clone());
                                self.scope.insert(param_register.clone(), param.clone());
                                param_reg_pair.push((param_register, param));
                            }

                            // Set function to current context
                            self.current_func_scope = Some(Box::new(Function::new(
                                name.clone(),            // name
                                Some(class_def.clone()), // owner
                                Statement::Function {
                                    // statement
                                    name: name.clone(),
                                    params: params.clone(),
                                    ret_type: ret_type.clone(),
                                    body: body.clone(),
                                },
                                params.clone(),   // args
                                ret_type.clone(), // ret_type
                            )));

                            // Add `self` as the first parameter
                            let mut param_str = format!("ptr %{}", self_register);
                            if !params.is_empty() {
                                let params_str = params
                                    .iter()
                                    .map(|p| {
                                        // find reg from param_reg_pair using p.name as key
                                        let param_reg = &param_reg_pair
                                            .iter()
                                            .find(|(_, param)| param.name == p.name)
                                            .unwrap_or_else(|| {
                                                panic!(
                                                    "Parameter '{}' not found in method parameters",
                                                    p.name
                                                )
                                            })
                                            .0;
                                        format!("{} %{}", self.type_to_llvm(&p.var_type), param_reg)
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                param_str = format!("{}, {}", param_str, params_str);
                            }

                            // println!("Processing method: {}::{}", class_def.name, name);
                            let body_llvm = self.transform_block(&body.statements);

                            // Exit method scope
                            self.scope.exit_scope();

                            self.current_class_scope = None;
                            //
                            // Generate function body </END>
                            //

                            // Insert the function definition into the LLVM IR
                            self.prologue.push(format!(
                                "; {}::{}\ndefine {} @__{}_{}({}) {{\nentry:",
                                class_def.name,
                                name,
                                return_type_llvm,
                                class_def.name,
                                name,
                                param_str,
                            ));
                            self.prologue
                                .instructions
                                .extend(body_llvm.clone().unwrap().code.instructions);

                            if name == &format!("__{}_init", class_def.name)
                                || name == &format!("__{}_destroy", class_def.name)
                            {
                                self.prologue.push("  ret void".to_string());
                            }

                            self.prologue.push("}".to_string());
                        } else {
                            panic!("Expected function statement for class method");
                        }
                    }

                    // Generate the read-only VTable for the class
                    let vtable_name = format!("k_{}VTable", class_def.name());
                    self.prologue.push(format!(
                        "@{} = constant %{}VTable {{",
                        vtable_name,
                        class_def.name()
                    ));
                    let mut method_sigs = Vec::new();
                    for (method, class_name) in &all_methods {
                        let (fun, cl, nm) =
                            if let Statement::Function { name, .. } = method.0.as_ref() {
                                (
                                    format!("  ptr @__{}_{}", class_name, name,),
                                    class_name,
                                    name,
                                )
                            } else {
                                panic!("Expected function statement for class method");
                            };
                        method_sigs.push((fun, cl.to_string(), nm.to_string()));
                    }
                    self.prologue.push(
                        method_sigs
                            .iter()
                            .enumerate()
                            .map(|(i, (sig, class_name, func_name))| {
                                let comma = if i + 1 < method_sigs.len() { "," } else { "" };
                                format!(
                                    "  {:<55} ; {}::{}",
                                    format!("{}{}", sig, comma),
                                    class_name,
                                    func_name
                                )
                            })
                            .collect::<Vec<_>>()
                            .join("\n"),
                    );
                    self.prologue.push("}".to_string());
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

        Some(eval)
    }

    #[track_caller]
    pub fn transform_expression(&mut self, expr: Expression) -> Evaluation {
        let mut eval = Evaluation {
            code: IR::new(),
            register: Register::new(Type::ToBeEvaluated("invalid_reg".to_string())),
            current_class_context: None,
            current_struct_context: None,
            current_func_context: None,
        };

        match expr {
            Expression::Number(value) => {
                eval.code
                    .push(format!("%{} = add i64 0, {}", eval.register, value));
                eval.register.var_type = Type::I64;
            }
            Expression::StringLiteral(value) => {
                let string_data = StringData::new(value);

                self.prologue.push(format!(
                    "@.{} = private constant [{} x i8] c\"{}\\00\"\n",
                    string_data.name(),
                    string_data.length() + 1,
                    string_data
                        .value()
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                ));

                eval.code.push(format!(
                    "%{} = getelementptr [{} x i8], ptr @.{}, i32 0, i32 0",
                    eval.register,
                    string_data.length() + 1,
                    string_data.name()
                ));

                eval.register.var_type = Type::String;
            }
            Expression::Variable(var) => {
                let var_symbol = self
                    .scope
                    .find_by_name(&var.name)
                    .unwrap_or_else(|| panic!("Variable {} not found in scope", var.name));

                eval.register = var_symbol.0.clone();
            }
            Expression::BinaryOp { op, left, right } => {
                let left_eval = self.transform_expression(*left);
                let right_eval = self.transform_expression(*right);

                eval.code
                    .instructions
                    .extend(left_eval.code.instructions);
                eval.code
                    .instructions
                    .extend(right_eval.code.instructions);

                // Determine the result type (promote to larger type)
                let result_type = self.determine_binary_op_result_type(
                    &left_eval.register.var_type,
                    &right_eval.register.var_type,
                );

                // Update the evaluation register's type
                eval.register.var_type = result_type.clone();

                // Convert operands to result type if needed
                let left_converted = if left_eval.register.var_type != result_type {
                    self.insert_type_conversion(
                        &mut eval.code,
                        &left_eval.register,
                        &result_type,
                    )
                } else {
                    left_eval.register
                };

                let right_converted = if right_eval.register.var_type != result_type {
                    self.insert_type_conversion(
                        &mut eval.code,
                        &right_eval.register,
                        &result_type,
                    )
                } else {
                    right_eval.register
                };

                let result_llvm_type = self.type_to_llvm(&result_type);

                match op.as_str() {
                    "+" => {
                        eval.code.push(format!(
                            "%{} = add {} %{}, %{}",
                            eval.register, result_llvm_type, left_converted, right_converted
                        ));
                    }
                    "-" => {
                        eval.code.push(format!(
                            "%{} = sub {} %{}, %{}",
                            eval.register, result_llvm_type, left_converted, right_converted
                        ));
                    }
                    "*" => {
                        eval.code.push(format!(
                            "%{} = mul {} %{}, %{}",
                            eval.register, result_llvm_type, left_converted, right_converted
                        ));
                    }
                    "/" => {
                        self.expect_signedness_match(
                            &left_converted.var_type,
                            &right_converted.var_type,
                        );

                        let llvm_op = match &left_converted.var_type.is_signed() {
                            true => "sdiv",
                            false => "udiv",
                        };

                        eval.code.push(format!(
                            "%{} = {} {} %{}, %{}",
                            eval.register,
                            llvm_op,
                            result_llvm_type,
                            left_converted,
                            right_converted
                        ));
                    }
                    ">" => {
                        eval.code.push(format!(
                            "%{} = icmp {}gt {} %{}, %{}",
                            eval.register,
                            if self.type_is_signed(&eval.register.var_type) {
                                "s"
                            } else {
                                "u"
                            },
                            result_llvm_type,
                            left_converted,
                            right_converted
                        ));
                    }
                    "<" => {
                        eval.code.push(format!(
                            "%{} = icmp {}lt {} %{}, %{}",
                            eval.register,
                            if self.type_is_signed(&eval.register.var_type) {
                                "s"
                            } else {
                                "u"
                            },
                            result_llvm_type,
                            left_converted,
                            right_converted
                        ));
                    }
                    ">=" => {
                        eval.code.push(format!(
                            "%{} = icmp {}ge {} %{}, %{}",
                            eval.register,
                            if self.type_is_signed(&eval.register.var_type) {
                                "s"
                            } else {
                                "u"
                            },
                            result_llvm_type,
                            left_converted,
                            right_converted
                        ));
                    }
                    "<=" => {
                        eval.code.push(format!(
                            "%{} = icmp {}le {} %{}, %{}",
                            eval.register,
                            if self.type_is_signed(&eval.register.var_type) {
                                "s"
                            } else {
                                "u"
                            },
                            result_llvm_type,
                            left_converted,
                            right_converted
                        ));
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
                eval.code
                    .instructions
                    .extend(inner_eval.code.instructions);

                match op.as_str() {
                    "-" => {
                        eval.code.push(format!(
                            "%{} = sub {} 0, %{}",
                            eval.register,
                            inner_eval.register.llvm_type(),
                            inner_eval.register
                        ));

                        // Update the evaluation register's type
                        eval.register.var_type = inner_eval.register.var_type.clone();
                    }
                    "!" => {
                        // First convert to boolean if not already
                        let bool_register = if inner_eval.register.var_type != Type::Bool {
                            let temp_reg = Register::new(Type::Bool);
                            eval.code.push(format!(
                                "%{} = icmp ne {} %{}, 0",
                                temp_reg,
                                inner_eval.register.llvm_type(),
                                inner_eval.register
                            ));
                            temp_reg
                        } else {
                            inner_eval.register
                        };

                        // Then negate the boolean
                        eval.code.push(format!(
                            "%{} = xor i1 %{}, true",
                            eval.register, bool_register
                        ));
                        eval.register.var_type = Type::Bool;
                    }
                    _ => {
                        panic!("Unsupported unary operator: {}", op);
                    }
                }
            }
            Expression::Cast { expr, target_type } => {
                let inner_eval = self.transform_expression(*expr);
                eval.code
                    .instructions
                    .extend(inner_eval.code.instructions);

                let source_type = &inner_eval.register.llvm_type();
                let target_llvm = self.type_to_llvm(&target_type);

                match (source_type.as_str(), target_llvm.as_str()) {
                    // Integer truncation (larger to smaller)
                    ("i64", "i32") | ("i64", "i16") | ("i64", "i8") => {
                        eval.code.push(format!(
                            "%{} = trunc {} %{} to {}",
                            eval.register, source_type, inner_eval.register, target_llvm
                        ));
                    }

                    // Integer extension (smaller to larger)
                    ("i32", "i64") | ("i16", "i64") | ("i8", "i64") => {
                        eval.code.push(format!(
                            "%{} = sext {} %{} to {}",
                            eval.register, source_type, inner_eval.register, target_llvm
                        ));
                    }

                    // Float to integer
                    ("float", "i32") | ("float", "i64") | ("double", "i32") | ("double", "i64") => {
                        eval.code.push(format!(
                            "%{} = fptosi {} %{} to {}",
                            eval.register, source_type, inner_eval.register, target_llvm
                        ));
                    }

                    // Integer to float
                    ("i32", "float") | ("i64", "float") | ("i32", "double") | ("i64", "double") => {
                        eval.code.push(format!(
                            "%{} = sitofp {} %{} to {}",
                            eval.register, source_type, inner_eval.register, target_llvm
                        ));
                    }

                    // Float precision conversion
                    ("float", "double") => {
                        eval.code.push(format!(
                            "%{} = fpext {} %{} to {}",
                            eval.register, source_type, inner_eval.register, target_llvm
                        ));
                    }
                    ("double", "float") => {
                        eval.code.push(format!(
                            "%{} = fptrunc {} %{} to {}",
                            eval.register, source_type, inner_eval.register, target_llvm
                        ));
                    }

                    // Boolean conversions
                    ("i1", "i32") | ("i1", "i64") => {
                        eval.code.push(format!(
                            "%{} = zext {} %{} to {}",
                            eval.register, source_type, inner_eval.register, target_llvm
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
            Expression::MethodCall {
                object,
                method,
                args: _args,
            } => {
                // println!(
                //     "{}\n{}\n{}\n{}\n{}\n{}\n{}",
                //     format!("Expression::MethodCall"),
                //     format!("      | [object]"),
                //     format!("      +-----+ {:?}\n", object.clone()),
                //     format!("      | [method]"),
                //     format!("      +-----+ {:?}\n", method.clone()),
                //     format!("      | [args]"),
                //     format!("      +-----+ {:?}\n", _args.clone()),
                // );

                // Evaluate the base object (e.g., `self` or `this`)
                let object_eval = match &*object {
                    Expression::Variable(var) if var.name == "self" => {
                        // println!("------+ Expression::MethodCall -- `self` Specific Case");

                        // Ensure we are in a class scope
                        let class_scope = self
                            .current_class_scope
                            .as_ref()
                            .expect("Field access requires a class scope");

                        // `self` or `this` points to the current class
                        let _class_type = Type::Pointer(Box::new(match &class_scope.statement {
                            Statement::Class { .. } => self.get_class_type(&class_scope.name),
                            _ => panic!("Expected class statement for 'self'"),
                        }));

                        // Find `self` from the current variable scope
                        let mut self_symbol = self
                            .scope
                            .pop_by_name("self")
                            .expect("Expected 'self' variable in class scope");

                        self_symbol.0 = Register::new(_class_type);

                        self.scope
                            .insert(self_symbol.0.clone(), self_symbol.1.clone());

                        Evaluation {
                            code: IR::new(),
                            register: self_symbol.0.clone(),
                            current_class_context: self.current_class_scope.as_deref().cloned(),
                            current_struct_context: None,
                            current_func_context: None,
                        }
                    }
                    _ => self.transform_expression(*object),
                };

                let object_code = object_eval.code.instructions.clone();
                eval.code.instructions.extend(object_code);

                // println!(
                //     "{}\n{}\n",
                //     format!("| [Object Evaluation]"),
                //     format!("+-----+ {:?}", object_eval.clone())
                // );

                // Ensure the object is a pointer to a class
                let class_name = match &object_eval.register.var_type {
                    Type::Pointer(inner_type) => match &**inner_type {
                        Type::Class { name, .. } => name.clone(),
                        _ => panic!("Method access on non-class pointer"),
                    },
                    _ => panic!("Method access on non-pointer type"),
                };

                let method_class = self.class_definitions.get(&class_name).unwrap_or_else(|| {
                    panic!("Failed to find class {} in class definitions", class_name)
                });

                let owning_class = self
                    .class_definitions
                    .get(&method_class.name)
                    .cloned()
                    .unwrap_or_else(|| {
                        panic!("Class {} not found in class definitions", method_class.name)
                    });

                // println!(
                //     "{}\n{}\n",
                //     format!("| [Owning Class]"),
                //     format!("+-----+ {:?}", owning_class.clone())
                // );

                // PROCEDURE
                //
                // ; Step 1: Load vtable from %self (field 0)
                // %vtable_ptr_ptr = getelementptr inbounds %Point, ptr %self, i32 0, i32 0
                // %vtable_ptr = load ptr, ptr %vtable_ptr_ptr

                // ; Step 2: Load get_x function pointer (index 2 in vtable)
                // %get_x_ptr_ptr = getelementptr inbounds %PointVTable, ptr %vtable_ptr, i32 0, i32 2
                // %get_x_ptr_raw = load ptr, ptr %get_x_ptr_ptr

                // ; Step 3: Cast the function pointer to correct type: i32(ptr) -> i32
                // %get_x_fn = bitcast ptr %get_x_ptr_raw to ptr (ptr) -> i32

                // ; Step 4: Call the function, passing %self as the receiver (like 'this')
                // %result = call i32 %get_x_fn(ptr %self)

                // GEP to get the first pointer to vtable
                let vtable_ptr_ptr = Register::new_var(
                    Type::Pointer(Box::new(Type::Pointer(Box::new(
                        self.get_class_type(&owning_class.name),
                    )))),
                    String::from("vtable_ptr_ptr"),
                );
                eval.code.instructions.push(format!(
                    "%{} = getelementptr inbounds %{}, ptr %{}, i32 0, i32 0",
                    vtable_ptr_ptr, method_class.name, object_eval.register
                ));
                let vtable_ptr = Register::new_var(
                    Type::Pointer(Box::new(self.get_class_type(&owning_class.name))),
                    String::from("vtable_ptr"),
                );
                eval.code.instructions.push(format!(
                    "%{} = load ptr, ptr %{}",
                    vtable_ptr, vtable_ptr_ptr
                ));

                // Get the method pointer by finding its index in the vtable
                // From the class name we can find all_methods and the index of that
                // is the index in the VTable
                let (method_idx, method_entry) = owning_class
                    .all_methods
                    .iter()
                    .enumerate()
                    .find(|(_i, ((m, _vis), _name))| match &**m {
                        Statement::Function { name, .. } => *name == method,
                        _ => false,
                    })
                    .map(|(i, ((m, _vis), _name))| (i, m))
                    .unwrap_or_else(|| {
                        panic!(
                            "Method '{}' not found in class '{}'",
                            method, method_class.name
                        )
                    });
                let method_ret = match &**method_entry {
                    Statement::Function { ret_type, .. } => ret_type,
                    _ => panic!("Method statement {:?} isn't a function!", method_entry),
                };
                let (method_type, _method_args, _method_signature) = match &**method_entry {
                    Statement::Function {
                        name,
                        ret_type,
                        params,
                        ..
                    } => {
                        let param_types: Vec<Type> =
                            params.iter().map(|param| param.var_type.clone()).collect();

                        let param_sig_no_self = param_types
                            .iter()
                            .map(|t| self.type_to_llvm(t))
                            .collect::<Vec<_>>()
                            .join(", ");

                        let param_sig_all = if param_sig_no_self.is_empty() {
                            "ptr".to_string()
                        } else {
                            format!("ptr, {}", param_sig_no_self)
                        };

                        (
                            Type::Function {
                                name: name.clone(),
                                args: param_types.clone(),
                                ret_type: Box::new(ret_type.clone()),
                                is_variadic: false,
                            },
                            param_types,
                            format!("({}) -> {}", param_sig_all, self.type_to_llvm(ret_type)),
                        )
                    }
                    _ => panic!("Method statement {:?} isn't a function!", method_entry),
                };

                // Set the evaluation register to be the method's return type
                eval.register.var_type = method_ret.clone();
                eval.register.name = format!("{}_{}_result", class_name.clone().to_lowercase(), method.clone());

                // Load method from vtable by index
                let method_ptr_ptr = Register::new_var(
                    Type::Pointer(Box::new(Type::Pointer(Box::new(method_type.clone())))),
                    format!(
                        "{}_{}_method_ptr_ptr",
                        method_class.name.to_lowercase(),
                        method.clone()
                    ),
                );
                eval.code.instructions.push(format!(
                    "%{} = getelementptr inbounds %{}VTable, ptr %{}, i32 0, i32 {}",
                    method_ptr_ptr, method_class.name, vtable_ptr, method_idx
                ));
                let method_ptr_raw = Register::new_var(
                    Type::Pointer(Box::new(method_type.clone())),
                    format!(
                        "{}_{}_method_ptr_raw",
                        method_class.name.to_lowercase(),
                        method.clone()
                    ),
                );
                eval.code.instructions.push(format!(
                    "%{} = load ptr, ptr %{}",
                    method_ptr_raw, method_ptr_ptr
                ));

                // Prepare arguments
                let mut arg_registers = vec![];
                for arg in _args {
                    let arg = arg.clone();
                    let arg_eval = self.transform_expression(arg);
                    eval.code
                        .instructions
                        .extend(arg_eval.code.instructions);
                    arg_registers.push(arg_eval.register);
                }

                let arg_string_no_self = arg_registers
                    .iter()
                    .map(|a| format!("{} %{}", self.type_to_llvm(&a.var_type), a))
                    .collect::<Vec<_>>()
                    .join(", ");

                let arg_string_all = if arg_string_no_self.is_empty() {
                    format!("ptr %{}", object_eval.register)
                } else {
                    format!("ptr %{}, {}", object_eval.register, arg_string_no_self)
                };

                // Generate the method call
                eval.code.push(format!(
                    "%{} = call {} %{}({})",
                    eval.register,
                    self.type_to_llvm(method_ret),
                    method_ptr_raw,
                    arg_string_all
                ));
            }
            Expression::FieldAccess { object, field } => {
                // Evaluate the base object (e.g., `self` or `this`)
                let object_eval = match &*object {
                    Expression::Variable(var) if var.name == "self" => {
                        // Ensure we are in a class scope
                        let class_scope = self
                            .current_class_scope
                            .as_ref()
                            .expect("Field access requires a class scope");

                        // `self` or `this` points to the current class
                        let _class_type = Type::Pointer(Box::new(match &class_scope.statement {
                            Statement::Class { .. } => self.get_class_type(&class_scope.name),
                            _ => panic!("Expected class statement for 'self'"),
                        }));

                        // Find `self` from the current variable scope
                        let self_symbol = self
                            .scope
                            .find_by_name("self")
                            .expect("Expected 'self' variable in class scope");

                        Evaluation {
                            code: IR::new(),
                            register: self_symbol.0.clone(),
                            current_class_context: self.current_class_scope.as_deref().cloned(),
                            current_struct_context: None,
                            current_func_context: None,
                        }
                    }
                    _ => self.transform_expression(*object),
                };

                eval.code
                    .instructions
                    .extend(object_eval.code.instructions);

                // Ensure the object is a pointer to a class
                let _class_type = match &object_eval.register.var_type {
                    Type::Pointer(inner_type) => match **inner_type {
                        Type::Class { ref fields, .. } => fields.clone(),
                        _ => panic!("Field access on non-class pointer"),
                    },
                    _ => panic!("Field access on non-pointer type"),
                };

                // Walk class hierarchy to find the field and remember which class owns it
                let mut found: Option<(usize, Variable, Class)> = None;
                let mut class_cursor = object_eval.current_class_context.clone();

                while let Some(class_def) = class_cursor.clone() {
                    if let Statement::Class { fields, .. } = &class_def.statement
                        && let Some((i, (var, _vis))) = fields
                            .iter()
                            .enumerate()
                            .find(|(_, (f, _))| f.name == field)
                    {
                        found = Some((i, var.clone(), class_def.clone()));
                        break;
                    }
                    // Move to parent
                    class_cursor = class_def.parent.as_ref().map(|p| (*p.clone()));
                }

                let (field_index, field_var, field_class) = match found {
                    Some(t) => t,
                    None => panic!("Field '{}' not found in class hierarchy", field),
                };
                // field_class currently unused; keep for future logic
                let _owning_class = field_class.clone();

                // Generate GEP instruction to get the field pointer
                let field_ptr_register = Register::new_var(
                    field_var.var_type.clone(),
                    field_class.name.clone().to_lowercase(),
                );
                eval.code.push(format!(
                    "%{} = getelementptr inbounds %{}, ptr %{}, i32 0, i32 {}",
                    field_ptr_register,
                    field_class.name,
                    object_eval.register,
                    field_index + 1 // +1 because first element is the vtable pointer
                ));

                // If the field is a pointer to another class, update the register for further access
                let field_type = field_var.var_type.clone();
                if let Type::Pointer(inner_type) = &field_type {
                    if let Type::Class { .. } = **inner_type {
                        eval.register = field_ptr_register;
                    } else {
                        // Load the value if it's not a class pointer
                        let field_value_register =
                            Register::new_var(field_type.clone(), field_var.name.clone());
                        eval.code.push(format!(
                            "%{} = load {}, ptr %{}",
                            field_value_register,
                            self.type_to_llvm(&field_type),
                            field_ptr_register
                        ));
                        eval.register = field_value_register;
                    }
                } else {
                    // Load the value if it's not a pointer
                    let field_value_register =
                        Register::new_var(field_type.clone(), field_var.name.clone());
                    eval.code.push(format!(
                        "%{} = load {}, ptr %{}",
                        field_value_register,
                        self.type_to_llvm(&field_type),
                        field_ptr_register
                    ));
                    eval.register = field_value_register;
                }
            }
            Expression::Call { callee, args } => {
                match *callee {
                    Expression::Variable(var) => {
                        // Find the function in the current scope
                        let func = self.scope.find_by_name(&var.name);

                        // Find the class in the current scope
                        let class = self.class_definitions.get(&var.name).cloned().or_else(|| {
                            self.current_class_scope.as_ref().and_then(|c| {
                                if c.name == var.name {
                                    Some(*c.clone())
                                } else {
                                    None
                                }
                            })
                        });

                        // println!("[LLVM Call] Function: {:?}, Class: {:?}", func, class);
                        // println!("[LLVM Call] Variable: {:?}", var);
                        // println!("[LLVM Call] Scope: {:?}", self.scope);
                        // println!("[LLVM Call] Classes: {:?}", self.class_definitions);

                        // If the function is not found and the class name is the same
                        // as the variable name, treat it as a method call to the constructor
                        let (func_name, constructor, is_constructor) = if func.is_none()
                            && let Some(class) = class.as_ref()
                        {
                            let class_name = class.name.clone();
                            let class_def = class.clone();
                            match class_def.statement {
                                Statement::Class { .. } => {
                                    // Register the class type in the evaluation context
                                    let cl = self.get_class_type(&class_name);
                                    eval.register.var_type = Type::Pointer(Box::new(cl));
                                }
                                _ => panic!("Expected class statement for constructor"),
                            }
                            // Find the class definition and extract the constructor method from it
                            let c_name = format!("__{}_init", class_name);
                            let c = class_def
                                .all_methods
                                .iter()
                                .find(|((m, _), method_class_name)| matches!(m.as_ref(), Statement::Function { name, .. } if name == &c_name && method_class_name == &class_name))
                                .map(|(m, _)| m.clone())
                                .unwrap_or_else(|| {
                                    panic!(
                                        "Constructor '{}' not found for class '{}'",
                                        c_name,
                                        class_def.name()
                                    )
                                });

                            let c_fun = match &*c.0 {
                                Statement::Function {
                                    name,
                                    ret_type,
                                    params,
                                    ..
                                } => Type::Function {
                                    name: name.clone(),
                                    args: params.iter().map(|p| p.var_type.clone()).collect(),
                                    ret_type: Box::new(ret_type.clone()),
                                    is_variadic: false,
                                },
                                _ => panic!("Expected function statement for constructor"),
                            };

                            (
                                format!("__{}___{}_init", class_name, class_name),
                                Some(c_fun),
                                true,
                            )
                        } else {
                            eval.register.var_type = func.as_ref().unwrap().1.var_type.clone();
                            (var.name.clone(), None, false)
                        };

                        let mut arg_registers = Vec::new();

                        if let Some(class) = class.as_ref()
                            && is_constructor
                        {
                            eval.code
                                .push(format!("; Constructing {}", class.name.clone()));
                            // Make a new register for self and allocate the type in the stack
                            let self_reg = Register::new_var(
                                eval.register.var_type.clone(),
                                class.name.clone().to_lowercase(),
                            );

                            // Add `self` as the first argument
                            arg_registers.push(self_reg.clone());

                            // Since we call the constructor later, this whole
                            // expression will just return the `self` pointer
                            eval.register = self_reg.clone();

                            eval.code
                                .push(format!("%{} = alloca %{}, align 8", self_reg, class.name));
                            // Get the vtable pointer from the first element
                            eval.code.push(format!(
                                "%vtable_ptr = getelementptr inbounds %{}, ptr %{}, i32 0, i32 0",
                                class.name, self_reg
                            ));
                            // Store the pointer to constant vtable in readonly data in %vtable_ptr
                            eval.code.push(format!(
                                "store ptr @k_{}VTable, ptr %vtable_ptr, align 8",
                                class.name
                            ));
                        }

                        // Collect arguments from the type evaluated
                        let func_typ = if let Some(func) = func.as_ref() {
                            func.1.var_type.clone()
                        } else if let Some(c) = constructor.as_ref() {
                            c.clone()
                        } else {
                            panic!("Function or constructor type not found for call");
                        };

                        let (expected_args, expected_ret_type) = match &func_typ {
                            Type::Function { args, ret_type, .. } => {
                                (args.clone(), Box::new(ret_type.clone()))
                            }
                            _ => panic!("Expected function type"),
                        };

                        // Prepare user given arguments
                        for (i, arg) in args.into_iter().enumerate() {
                            let arg_eval = self.transform_expression(arg);
                            eval.code
                                .instructions
                                .extend(arg_eval.code.instructions);

                            // println!(">>>> {:?}", arg_eval.register);
                            // println!("<<<< {:?}", expected_args[i]);

                            // Insert type coercion
                            if arg_eval.register.var_type != expected_args[i] {
                                let coerced = self.insert_type_conversion(
                                    &mut eval.code,
                                    &arg_eval.register,
                                    &expected_args[i],
                                );
                                arg_registers.push(coerced);
                            } else {
                                arg_registers.push(arg_eval.register);
                            }
                        }

                        // Call the function
                        let args_str = arg_registers
                            .iter()
                            .map(|r| format!("{} %{}", self.type_to_llvm(&r.var_type), r))
                            .collect::<Vec<_>>()
                            .join(", ");

                        if **expected_ret_type == Type::Void {
                            // if is_constructor {
                            //     let class_type = self.get_class_type(&class.as_ref().unwrap().name);
                            //     eval.register = Register::new_var(
                            //         Type::Pointer(Box::new(class_type)),
                            //         class.as_ref().unwrap().name.clone().to_lowercase()
                            //     );
                            // } else {
                            //     eval.register = Register::new(Type::Void);
                            // }
                            eval.code.push(format!(
                                "call {} @{}({})",
                                self.type_to_llvm(&expected_ret_type),
                                func_name,
                                args_str
                            ));
                        } else {
                            eval.register = Register::new(**expected_ret_type.clone());
                            eval.code.push(format!(
                                "%{} = call {} @{}({})",
                                eval.register,
                                self.type_to_llvm(&expected_ret_type),
                                func_name,
                                args_str
                            ));
                        }
                    }
                    _ => panic!("Unsupported callee type in function call"),
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

    fn insert_type_conversion(
        &self,
        code: &mut IR,
        from_register: &Register,
        target_type: &Type,
    ) -> Register {
        if from_register.var_type == *target_type {
            return from_register.clone();
        }

        let new_register = Register::new(target_type.clone());

        // If we have a function, we need to get the return type as FROM
        let from_llvm = match &from_register.var_type {
            Type::Function { ret_type, .. } => self.type_to_llvm(ret_type),
            _ => from_register.llvm_type(),
        };

        // DEST type converting into
        let to_llvm = self.type_to_llvm(target_type);

        match (from_llvm.as_str(), to_llvm.as_str()) {
            // Truncation from i64
            ("i64", "i32") | ("i64", "i16") | ("i64", "i8") | ("i64", "i1") => {
                code.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register, from_llvm, from_register, to_llvm
                ));
            }
            // Truncation from i32
            ("i32", "i16") | ("i32", "i8") | ("i32", "i1") => {
                code.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register, from_llvm, from_register, to_llvm
                ));
            }
            // Truncation from i16
            ("i16", "i8") | ("i16", "i1") => {
                code.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register, from_llvm, from_register, to_llvm
                ));
            }
            // Truncation from i8
            ("i8", "i1") => {
                code.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register, from_llvm, from_register, to_llvm
                ));
            }
            // Sign extension to i64
            ("i32", "i64") | ("i16", "i64") | ("i8", "i64") | ("i1", "i64") => {
                code.push(format!(
                    "%{} = sext {} %{} to {}",
                    new_register, from_llvm, from_register, to_llvm
                ));
            }
            // Sign extension to i32
            ("i8", "i32") | ("i16", "i32") | ("i1", "i32") => {
                code.push(format!(
                    "%{} = sext {} %{} to {}",
                    new_register, from_llvm, from_register, to_llvm
                ));
            }
            // Sign extension to i16
            ("i8", "i16") | ("i1", "i16") => {
                code.push(format!(
                    "%{} = sext {} %{} to {}",
                    new_register, from_llvm, from_register, to_llvm
                ));
            }
            // Sign extension to i8
            ("i1", "i8") => {
                code.push(format!(
                    "%{} = zext {} %{} to {}",
                    new_register, from_llvm, from_register, to_llvm
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

    fn expect_signedness_match(&self, left: &Type, right: &Type) {
        if left.is_signed() != right.is_signed() {
            panic!(
                "Cannot mix signed and unsigned types: {:?} and {:?}",
                left, right
            );
        }
    }

    fn generate_function_signature(
        &self,
        func_name: String,
        function: &Type,
        no_args: bool,
        unnamed: bool,
    ) -> String {
        if let Type::Function {
            args,
            ret_type,
            is_variadic,
            ..
        } = function
        {
            let return_type = self.type_to_llvm(ret_type);
            let params = args
                .iter()
                .map(|arg_type| self.type_to_llvm(arg_type).to_string())
                .collect::<Vec<_>>()
                .join(", ");
            let variadic_str = if *is_variadic {
                if params.is_empty() {
                    String::from("")
                } else {
                    format!("({}, ...) ", params)
                }
            } else {
                params.clone()
            };
            if !unnamed {
                if !no_args {
                    format!("{} {}@{}", return_type, variadic_str, func_name)
                } else {
                    format!("{} {}@{}({})", return_type, variadic_str, func_name, params)
                }
            } else if !no_args {
                return_type.to_string()
            } else {
                format!("{} ({})", return_type, params)
            }
        } else {
            panic!("generate_function_signature called with non-function type");
        }
    }

    fn generate_c_bindings(&mut self) {
        self.prologue.push("\n; C Bindings".to_string());
        self.prologue
            .push("declare i32 @printf(ptr, ...)   ;".to_string());
        self.scope.insert_top(
            Register::new(Type::I32),
            Variable {
                name: "printf".to_string(),
                var_type: Type::Function {
                    name: "printf".to_string(),
                    args: vec![Type::String],
                    ret_type: Box::new(Type::I32),
                    is_variadic: true,
                },
            },
        );
        self.prologue
            .push("declare i32 @scanf(ptr, ...)   ;".to_string());
        self.scope.insert_top(
            Register::new(Type::I32),
            Variable {
                name: "scanf".to_string(),
                var_type: Type::Function {
                    name: "scanf".to_string(),
                    args: vec![Type::String],
                    ret_type: Box::new(Type::I32),
                    is_variadic: true,
                },
            },
        );
        self.prologue
            .push("declare ptr @malloc(i64)  ;".to_string());
        self.scope.insert_top(
            Register::new(Type::Pointer(Box::new(Type::I8))),
            Variable {
                name: "malloc".to_string(),
                var_type: Type::Function {
                    name: "malloc".to_string(),
                    args: vec![Type::I64],
                    ret_type: Box::new(Type::Pointer(Box::new(Type::I8))),
                    is_variadic: false,
                },
            },
        );
        self.prologue.push("declare void @free(ptr)  ;".to_string());
        self.scope.insert_top(
            Register::new(Type::Void),
            Variable {
                name: "free".to_string(),
                var_type: Type::Function {
                    name: "free".to_string(),
                    args: vec![Type::Pointer(Box::new(Type::I8))],
                    ret_type: Box::new(Type::Void),
                    is_variadic: false,
                },
            },
        );
    }

    fn get_class_type(&self, class_name: &str) -> Type {
        // Find class in the class definitions
        let class_def = self.current_class_scope.as_ref().map_or_else(
            || {
                self.class_definitions.get(class_name).unwrap_or_else(|| {
                    panic!("Class '{}' not found in class definitions", class_name)
                })
            },
            |class_def| class_def.as_ref(),
        );

        // From the class statement extract parent, fields and methods
        match &class_def.statement {
            Statement::Class {
                parent,
                fields,
                methods,
                ..
            } => {
                // If the parent isn't null, get its type recursively
                let parent_type = parent.as_ref().map(|p| Box::new(self.get_class_type(p)));

                // Extract field types and names
                let field_types = fields
                    .iter()
                    .map(|(var, visibility)| (var.var_type.clone(), *visibility))
                    .collect();

                // Extract method types and visibilities
                let method_types = methods
                    .iter()
                    .map(|(method, visibility)| {
                        if let Statement::Function {
                            name,
                            ret_type,
                            params,
                            ..
                        } = method.as_ref()
                        {
                            let method_type = Type::Function {
                                name: name.clone(),
                                args: params.iter().map(|p| p.var_type.clone()).collect(),
                                ret_type: Box::new(ret_type.clone()),
                                is_variadic: false,
                            };
                            (method_type, *visibility)
                        } else {
                            panic!("Expected method to be a function in class '{}'", class_name);
                        }
                    })
                    .collect();

                Type::Class {
                    name: class_name.to_string(),
                    parent: parent_type,
                    fields: field_types,
                    methods: method_types,
                }
            }
            _ => {
                panic!("Expected class statement for class type");
            }
        }
    }

    fn enter_main(&mut self) {
        self.current_func_scope = Some(Box::new(Function::new(
            "main".to_string(), // name
            None,               // owner
            Statement::Function {
                // statement
                name: "main".to_string(),
                params: vec![],
                ret_type: Type::I32,
                body: Block::new(vec![]),
            },
            vec![],    // args
            Type::I32, // ret_type
        )));
    }
}
