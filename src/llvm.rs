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
                    if let Type::Function { .. } = var.var_type {
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
/// Classes and Structs
/// 
#[derive(Debug, Clone)]
pub struct Class {
    pub id: u64,
    pub name: String,
    pub parent: Option<Box<Class>>,
    pub statement: Statement,
}

static CLASS_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

impl Class {
    pub fn new(name: String, parent: Option<Box<Class>>, class_stm: Statement) -> Self {
        let id = CLASS_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Class { id, name, parent, statement: class_stm }
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
    pub statement: Statement
}

impl Struct {
    pub fn new(name: String, parent: Option<Box<Struct>>, struct_stm: Statement) -> Self {
        let id = STRUCT_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Struct { id, name, parent, statement: struct_stm }
    }

    pub fn undecorated_name(&self) -> String {
        format!("struct{}", self.id)
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }
}

static STRUCT_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

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
    class_definitions: HashMap<String, Class>,
    struct_definitions: HashMap<String, Struct>,
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
            class_definitions: HashMap::new(),
            struct_definitions: HashMap::new(),
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
            Type::Pointer(inner_type) => format!("{}*", self.type_to_llvm(inner_type)),
            _ => "i32".to_string(), // Default fallback
        }
    }

    pub fn compile(&mut self) {
        self.prologue.push(format!("\n; PROLOGUE"));
        self.prologue
            .push(format!("; LLVM IR generated from MM-lang\n"));

        self.scope.enter_scope(); // Enter binding scope
        
        self.generate_c_bindings();

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

        self.scope.exit_scope(); // Exit binding scope
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
        self.transform_block(&statements).unwrap()
    }

    #[track_caller]
    pub fn transform_block(&mut self, statements: &[Statement]) -> Option<Evaluation> {
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
                            &mut eval.epilogue,
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
                        eval.epilogue
                            .instructions
                            .extend(rhs_eval.epilogue.instructions);

                        // Handle type coercion if needed
                        let coerced_register = if rhs_eval.register.var_type != var_info.var_type {
                            self.insert_type_conversion(
                                &mut eval.epilogue,
                                &rhs_eval.register,
                                &var_info.var_type,
                            )
                        } else {
                            rhs_eval.register
                        };

                        eval.register = coerced_register.clone();
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
                    eval.epilogue
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
                    let then_eval = self.transform_block(&then_block.statements).expect("Failed to compile true-statements block");
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
                        let else_eval = self.transform_block(&else_block.statements).expect("Failed to compile false-statements block");
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
                    let mut body_eval = self.transform_block(&body.statements).expect("Failed to compile function body");

                    // Check the body_eval register, if we don't have a match
                    // We need to fix the last instruction.
                    if body_eval.register.var_type != *ret_type {
                        // Make sure the last instruction of the epilogue is a ret
                        body_eval.epilogue.instructions.last().map(|last| {
                            if !last.starts_with("ret") {
                                panic!(
                                    "Function '{}' return type mismatch: expected {}, got {}",
                                    name, ret_type, body_eval.register.var_type
                                );
                            }
                        });
                        // Remove the last instruction
                        body_eval.epilogue.instructions.pop();

                        // Coerce the return value register to the expected return type in a new register
                        let coerced_register = self.insert_type_conversion(
                            &mut body_eval.epilogue,
                            &body_eval.register,
                            &ret_type,
                        );

                        // Return the coerced register
                        body_eval.epilogue.push(format!(
                            "ret {} %{}",
                            self.type_to_llvm(&coerced_register.var_type),
                            coerced_register.to_string()
                        ));
                    }

                    self.code
                        .instructions
                        .extend(body_eval.prologue.instructions);
                    self.code
                        .instructions
                        .extend(body_eval.epilogue.instructions);
                    self.code.push(format!("}}"));

                    self.scope.exit_scope();

                    // Register function in global scope
                    let var_info = Variable {
                        name: name.clone(),
                        var_type: Type::Function {
                            args: params.iter().map(|p| p.var_type.clone()).collect(),
                            ret_type: Box::new(ret_type.clone()),
                            is_variadic: false,
                        },
                    };
                    let func_register = Register::new(ret_type.clone());
                    self.scope.insert_top(func_register, var_info);

                    return None; // No evaluation for function definitions
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

                    let param_types = if let Type::Function { args, .. } = &func_var.var_type {
                        args.clone()
                    } else {
                        panic!("Expected function type for '{}'", func_name);
                    };

                    // Generate function signature
                    let signature = self.generate_function_signature(func_var.name.clone(), &func_var.var_type.clone(), false);

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
                        "%{} = call {}({})",
                        eval.register.to_string(),
                        signature,
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

                    // Set the evaluation register to the return value of this block
                    // This captures the return type information to be used later
                    eval.register = return_eval.register.clone();

                    // Add return statement
                    eval.epilogue.push(format!(
                        "ret {} %{}",
                        return_eval.register.llvm_type(),
                        return_eval.register.to_string()
                    ));
                }
                Statement::Class { name, parent, fields, methods } => {
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
                    let class_def = Class::new(
                        name.clone(),
                        parent_class,
                        statement.clone()
                    );

                    // Generate the class VTable
                    // This is a forward declaration of the class type
                    // TODO: this is incomplete
                    let vt_content = methods.iter().map(|(method, _)| {
                        let method_name = match method.as_ref() {
                            Statement::Function { name, .. } => name.clone(),
                            _ => panic!("Expected function statement for class method"),
                        };
                        format!("i8* (%{})*", class_def.name())
                    }).collect::<Vec<_>>().join(", ");

                    // Forward declare the class type
                    // %AnimalVTable = type { i8* (%Animal*)* }
                    // TODO: this is wrong, we need to generate a proper VTable from method signatures
                    self.prologue.push(format!(
                        "%{}VTable = type {{ i8* (%{})* }}",
                        class_def.name(),
                        class_def.name()
                    ));

                    // Define the class type
                    // %Animal = type { %AnimalVTable*, field_type1, field_type2, ... }
                    // TODO: also incomplete, we need to add all fields
                    self.prologue.push(format!(
                        "%{} = type {{ %{}VTable*, {} }}",
                        class_def.name(),
                        class_def.name(),
                        fields.iter().map(|(f,_)| self.type_to_llvm(&f.var_type)).collect::<Vec<_>>().join(", ")
                    ));



                    // Insert the class definition into the class definitions
                    self.class_definitions
                        .insert(name.clone(), class_def.clone());

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
                    "%{} = getelementptr [{} x i8], [{} x i8]* @.{}, i32 0, i32 0",
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
                eval.epilogue
                    .instructions
                    .extend(left_eval.epilogue.instructions);
                eval.epilogue
                    .instructions
                    .extend(right_eval.epilogue.instructions);

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
                        &mut eval.epilogue,
                        &left_eval.register,
                        &result_type,
                    )
                } else {
                    left_eval.register
                };

                let right_converted = if right_eval.register.var_type != result_type {
                    self.insert_type_conversion(
                        &mut eval.epilogue,
                        &right_eval.register,
                        &result_type,
                    )
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
                        self.expect_signedness_match(
                            &left_converted.var_type,
                            &right_converted.var_type,
                        );

                        let llvm_op = match &left_converted.var_type.is_signed() {
                            true => "sdiv",
                            false => "udiv",
                        };

                        eval.epilogue.push(format!(
                            "%{} = {} {} %{}, %{}",
                            eval.register.to_string(),
                            llvm_op,
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
            }
            Expression::UnaryOp { op, expr } => {
                let inner_eval = self.transform_expression(*expr);
                eval.prologue
                    .instructions
                    .extend(inner_eval.prologue.instructions);
                eval.epilogue
                    .instructions
                    .extend(inner_eval.epilogue.instructions);

                match op.as_str() {
                    "-" => {
                        eval.epilogue.push(format!(
                            "%{} = sub {} 0, %{}",
                            eval.register.to_string(),
                            inner_eval.register.llvm_type(),
                            inner_eval.register.to_string()
                        ));

                        // Update the evaluation register's type
                        eval.register.var_type = inner_eval.register.var_type.clone();
                    }
                    "!" => {
                        // First convert to boolean if not already
                        let bool_register = if inner_eval.register.var_type != Type::Bool {
                            let temp_reg = Register::new(Type::Bool);
                            eval.epilogue.push(format!(
                                "%{} = icmp ne {} %{}, 0",
                                temp_reg.to_string(),
                                inner_eval.register.llvm_type(),
                                inner_eval.register.to_string()
                            ));
                            temp_reg
                        } else {
                            inner_eval.register
                        };

                        // Then negate the boolean
                        eval.epilogue.push(format!(
                            "%{} = xor i1 %{}, true",
                            eval.register.to_string(),
                            bool_register.to_string()
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
        code: &mut IR,
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
                code.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Truncation from i32
            ("i32", "i16") | ("i32", "i8") | ("i32", "i1") => {
                code.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Truncation from i16
            ("i16", "i8") | ("i16", "i1") => {
                code.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Truncation from i8
            ("i8", "i1") => {
                code.push(format!(
                    "%{} = trunc {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Sign extension to i64
            ("i32", "i64") | ("i16", "i64") | ("i8", "i64") | ("i1", "i64") => {
                code.push(format!(
                    "%{} = sext {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Sign extension to i32
            ("i8", "i32") | ("i16", "i32") | ("i1", "i32") => {
                code.push(format!(
                    "%{} = sext {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Sign extension to i16
            ("i8", "i16") | ("i1", "i16") => {
                code.push(format!(
                    "%{} = sext {} %{} to {}",
                    new_register.to_string(),
                    from_llvm,
                    from_register.to_string(),
                    to_llvm
                ));
            }
            // Sign extension to i8
            ("i1", "i8") => {
                code.push(format!(
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

    fn expect_signedness_match(&self, left: &Type, right: &Type) {
        if left.is_signed() != right.is_signed() {
            panic!(
                "Cannot mix signed and unsigned types: {:?} and {:?}",
                left, right
            );
        }
    }

    fn generate_function_signature(&self, func_name: String, function: &Type, no_args: bool) -> String {
        if let Type::Function { args, ret_type, is_variadic } = function {
            let return_type = self.type_to_llvm(ret_type);
            let params = args
                .iter()
                .map(|arg_type| format!("{}", self.type_to_llvm(arg_type)))
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
            if !no_args {
                format!("{} {}@{}", return_type, variadic_str, func_name)
            } else {
                format!("{} {}@{}({})", return_type, variadic_str, func_name, params)
            }
        } else {
            panic!("generate_function_signature called with non-function type");
        }
    }

    fn generate_c_bindings(&mut self) {
        self.prologue.push("\n; C Bindings".to_string());
        self.prologue.push("declare i32 @printf(i8*, ...)   ;".to_string());
        self.scope.insert_top(
            Register::new(Type::I32),
            Variable {
                name: "printf".to_string(),
                var_type: Type::Function {
                    args: vec![Type::String],
                    ret_type: Box::new(Type::I32),
                    is_variadic: true,
                },
            },
        );
        self.prologue.push("declare i32 @scanf(i8*, ...)   ;".to_string());
        self.scope.insert_top(
            Register::new(Type::I32),
            Variable {
                name: "scanf".to_string(),
                var_type: Type::Function {
                    args: vec![Type::String],
                    ret_type: Box::new(Type::I32),
                    is_variadic: true,
                },
            },
        );
        self.prologue.push("declare i8* @malloc(i64)  ;".to_string());
        self.scope.insert_top(
            Register::new(Type::Pointer(Box::new(Type::I8))),
            Variable {
                name: "malloc".to_string(),
                var_type: Type::Function {
                    args: vec![Type::I64],
                    ret_type: Box::new(Type::Pointer(Box::new(Type::I8))),
                    is_variadic: false,
                },
            },
        );
        self.prologue.push("declare void @free(i8*)  ;".to_string());
        self.scope.insert_top(
            Register::new(Type::NoneType),
            Variable {
                name: "free".to_string(),
                var_type: Type::Function {
                    args: vec![Type::Pointer(Box::new(Type::I8))],
                    ret_type: Box::new(Type::NoneType),
                    is_variadic: false,
                },
            },
        );
    }
}
