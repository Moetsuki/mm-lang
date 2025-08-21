//! TargetC backend: emits simplified C code mirroring core IR semantics.

#![allow(dead_code)]

use crate::ast::Ast;
use crate::backend::Backend;
use crate::expression::Expression;
use crate::file::SourceFile;
use crate::statement::Statement;
use crate::types::Type;
use crate::variable::Variable;
use crate::backend_c::type_c::type_to_c;
use std::collections::HashMap;

pub struct TargetC<'a> {
    ast: Ast<'a>,
    _source: &'a SourceFile,
    headers: String,
    decls: String,
    functions: Vec<String>,
    main_body: Vec<String>,
    temp_id: usize,
    code: String,
    classes: HashMap<String, ClassInfo>,
}

#[derive(Debug, Clone)]
struct MethodSig {
    name: String,
    ret_type: Type,
    params: Vec<Variable>,
}

#[derive(Debug, Clone)]
struct ClassInfo {
    name: String,
    fields: Vec<Variable>,
    methods: Vec<MethodSig>,
}

impl<'a> TargetC<'a> {
    pub fn new(ast: Ast<'a>, source: &'a SourceFile) -> Self {
        Self {
            ast,
            _source: source,
            headers: String::new(),
            decls: String::new(),
            functions: Vec::new(),
            main_body: Vec::new(),
            temp_id: 0,
            code: String::new(),
            classes: HashMap::new(),
        }
    }

    fn fresh(&mut self, prefix: &str) -> String {
        self.temp_id += 1;
        format!("{}_{}", prefix, self.temp_id)
    }

    fn transform_statement(&mut self, s: &Statement, _top_level: bool) -> Vec<String> {
        let mut out: Vec<String> = Vec::new();
        match s {
            Statement::VariableDecl {
                identifier, value, ..
            } => {
                // Tensor special-case: arr: tensor[i64] = {a, b, c}; => int64_t arr[3] = {a, b, c};
                if let Type::Tensor {
                    var_type: elem_ty, ..
                } = &identifier.var_type
                    && let Expression::InitializerList { elements, .. } = value {
                        // Emit element expressions in order (for side effects) and declare a fixed-size C array
                        let mut prelude: Vec<String> = Vec::new();
                        let mut parts: Vec<String> = Vec::new();
                        for el in elements {
                            let (c, e) = self.transform_expression(el);
                            prelude.extend(c);
                            parts.push(e);
                        }
                        out.extend(prelude);
                        out.push(format!(
                            "{} {}[{}] = {{{}}};",
                            type_to_c(elem_ty),
                            identifier.name,
                            elements.len(),
                            parts.join(", ")
                        ));
                        return out;
                    }
                // Detect class constructor call: T x = T(args...);
                if let Expression::Call { callee, args, .. } = value {
                    if let Expression::Variable { var, .. } = callee.as_ref() {
                        let hit = self.classes.contains_key(&var.name);
                        if hit {
                            let class = self.classes.get(&var.name).unwrap().clone();
                            // Emit args code and collect exprs
                            let mut arg_exprs: Vec<String> = Vec::new();
                            let args_local = args.clone();
                            for a in args_local {
                                let (c, e) = self.transform_expression(&a);
                                out.extend(c);
                                arg_exprs.push(e);
                            }
                            // Declaration with vtable and zero-initialized fields
                            let mut parts = vec![format!("&k_{}VTable", class.name)];
                            parts.extend(class.fields.iter().map(|_| "0".to_string()));
                            out.push(format!(
                                "{} {} = (struct {}){{{}}};",
                                type_to_c(&identifier.var_type),
                                identifier.name,
                                class.name,
                                parts.join(", ")
                            ));
                            // Call constructor only if args are provided
                            if !arg_exprs.is_empty() {
                                let ctor_call = format!(
                                    "__{}___{}_init(&{}, {})",
                                    class.name,
                                    class.name,
                                    identifier.name,
                                    arg_exprs.join(", ")
                                );
                                out.push(format!("{};", ctor_call));
                            }
                            return out;
                        }
                    }
                    // fallthrough to normal path if not ctor
                    let (rhs_code, rhs_expr) = self.transform_expression(value);
                    out.extend(rhs_code);
                    out.push(format!(
                        "{} {} = {};",
                        type_to_c(&identifier.var_type),
                        identifier.name,
                        rhs_expr
                    ));
                } else {
                    let (rhs_code, rhs_expr) = self.transform_expression(value);
                    out.extend(rhs_code);
                    out.push(format!(
                        "{} {} = {};",
                        type_to_c(&identifier.var_type),
                        identifier.name,
                        rhs_expr
                    ));
                }
            }
            Statement::Assignment {
                identifier, value, ..
            } => {
                let (rhs_code, rhs_expr) = self.transform_expression(value);
                out.extend(rhs_code);
                match identifier {
                    Expression::Variable { var, .. } => {
                        out.push(format!("{} = {};", var.name, rhs_expr));
                    }
                    Expression::FieldAccess { object, field, .. } => {
                        let (_oc, oexpr) = self.transform_expression(object);
                        let lhs = if let Expression::Variable { var, .. } = &**object
                            && var.name == "self"
                        {
                            format!("{}->{}", oexpr, field)
                        } else {
                            format!("{}.{}", oexpr, field)
                        };
                        out.push(format!("{} = {};", lhs, rhs_expr));
                    }
                    Expression::ArrayAccess { .. } => {
                        panic!("TargetC: array assignment not implemented yet")
                    }
                    _ => panic!("TargetC: unsupported assignment LHS: {:?}", identifier),
                }
            }
            Statement::If {
                condition,
                then_block,
                elif,
                else_block,
                ..
            } => {
                let (c_code, c_expr) = self.transform_expression(condition);
                out.extend(c_code);
                out.push(format!("if ({}) {{", c_expr));
                for ss in &then_block.statements {
                    out.extend(self.transform_statement(ss, false));
                }
                out.push("}".into());
                for e in elif {
                    if let Statement::If {
                        condition,
                        then_block,
                        ..
                    } = e
                    {
                        let (ec_code, ec_expr) = self.transform_expression(condition);
                        out.extend(ec_code);
                        out.push(format!("else if ({}) {{", ec_expr));
                        for ss in &then_block.statements {
                            out.extend(self.transform_statement(ss, false));
                        }
                        out.push("}".into());
                    } else {
                        panic!("TargetC: expected elif as If statement");
                    }
                }
                if let Some(else_blk) = else_block {
                    out.push("else {".into());
                    for ss in &else_blk.statements {
                        out.extend(self.transform_statement(ss, false));
                    }
                    out.push("}".into());
                }
            }
            Statement::Return { value, .. } => {
                let (code, expr) = self.transform_expression(value);
                out.extend(code);
                out.push(format!("return {};", expr));
            }
            Statement::Function {
                name,
                ret_type,
                params,
                body,
                ..
            } => {
                // Emit a full C function
                let sig = format!(
                    "{} {}({})",
                    type_to_c(ret_type),
                    name,
                    params
                        .iter()
                        .map(|p| format!("{} {}", type_to_c(&p.var_type), p.name))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                let mut body_lines = Vec::<String>::new();
                for ss in &body.statements {
                    body_lines.extend(self.transform_statement(ss, false));
                }
                let mut func = String::new();
                func.push_str(&format!("{} {{\n", sig));
                for l in body_lines {
                    func.push_str("  ");
                    func.push_str(&l);
                    func.push('\n');
                }
                func.push_str("}\n\n");
                self.functions.push(func);
            }
            Statement::Call { callee, args, .. } => {
                // Support simple calls like printf(x)
                if let Expression::Variable { var, .. } = callee {
                    let arg_exprs = args
                        .iter()
                        .map(|a| {
                            let (c, e) = self.transform_expression(a);
                            out.extend(c);
                            e
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    out.push(format!("{}({});", var.name, arg_exprs));
                } else {
                    panic!("TargetC: unsupported callee expression: {:?}", callee)
                }
            }
            // Class lowering to C using struct + vtable
            Statement::Class {
                name,
                parent: _,
                fields,
                methods,
                ..
            } => {
                // Record class info for constructor detection and field count
                let class_fields: Vec<Variable> = fields.iter().map(|(v, _)| v.clone()).collect();
                let class_methods: Vec<MethodSig> = methods
                    .iter()
                    .filter_map(|(m, _)| {
                        if let Statement::Function {
                            name,
                            ret_type,
                            params,
                            ..
                        } = m.as_ref()
                        {
                            Some(MethodSig {
                                name: name.clone(),
                                ret_type: ret_type.clone(),
                                params: params.clone(),
                            })
                        } else {
                            None
                        }
                    })
                    .collect();
                self.classes.insert(
                    name.clone(),
                    ClassInfo {
                        name: name.clone(),
                        fields: class_fields.clone(),
                        methods: class_methods.clone(),
                    },
                );

                // Decls: forward decls
                self.decls
                    .push_str(&format!("typedef struct {} {};\n", name, name));
                self.decls
                    .push_str(&format!("typedef struct {}VTable {}VTable;\n", name, name));

                // Decls: vtable struct
                self.decls.push_str(&format!("struct {}VTable {{\n", name));
                for ms in &class_methods {
                    let params_sig = std::iter::once(format!(
                        "{}* self",
                        type_to_c(&Type::Class {
                            name: name.clone(),
                            parent: None,
                            fields: vec![],
                            methods: vec![]
                        })
                    ))
                    .chain(
                        ms.params
                            .iter()
                            .map(|p| format!("{} {}", type_to_c(&p.var_type), p.name)),
                    )
                    .collect::<Vec<_>>()
                    .join(", ");
                    self.decls.push_str(&format!(
                        "  {} (*{})({});\n",
                        type_to_c(&ms.ret_type),
                        ms.name,
                        params_sig
                    ));
                }
                self.decls.push_str("};\n");

                // Decls: class struct (vtable pointer first, then fields)
                self.decls.push_str(&format!(
                    "struct {} {{\n  const {}VTable* vtable;\n",
                    name, name
                ));
                for f in &class_fields {
                    self.decls.push_str(&format!(
                        "  {} {};\n",
                        type_to_c(&f.var_type),
                        f.name
                    ));
                }
                self.decls.push_str("};\n\n");

                // Functions: method implementations as free functions __Class_method(self, ...)
                for (m, _) in methods {
                    if let Statement::Function {
                        name: mname,
                        ret_type,
                        params,
                        body,
                        ..
                    } = m.as_ref()
                    {
                        // Build a synthetic function signature including self
                        let mut sig = format!(
                            "{} __{}_{}({}* self",
                            type_to_c(ret_type),
                            name,
                            mname,
                            type_to_c(&Type::Class {
                                name: name.clone(),
                                parent: None,
                                fields: vec![],
                                methods: vec![]
                            })
                        );
                        for p in params {
                            sig.push_str(&format!(", {} {}", type_to_c(&p.var_type), p.name));
                        }
                        sig.push(')');

                        // Emit body
                        let mut body_lines = Vec::<String>::new();
                        for ss in &body.statements {
                            body_lines.extend(self.transform_statement(ss, false));
                        }
                        let mut func = String::new();
                        func.push_str(&format!("{} {{\n", sig));
                        for l in body_lines {
                            func.push_str("  ");
                            func.push_str(&l);
                            func.push('\n');
                        }
                        func.push_str("}\n\n");
                        self.functions.push(func);
                    }
                }

                // Decls: forward prototypes for methods (needed before vtable init)
                for ms in &class_methods {
                    let mut proto = format!(
                        "{} __{}_{}({}* self",
                        type_to_c(&ms.ret_type),
                        name,
                        ms.name,
                        type_to_c(&Type::Class {
                            name: name.clone(),
                            parent: None,
                            fields: vec![],
                            methods: vec![]
                        })
                    );
                    for p in &ms.params {
                        proto.push_str(&format!(", {} {}", type_to_c(&p.var_type), p.name));
                    }
                    proto.push_str(");\n");
                    self.decls.push_str(&proto);
                }

                // Decls: instantiate vtable with method function addresses
                self.decls.push_str(&format!(
                    "static const {}VTable k_{}VTable = {{\n",
                    name, name
                ));
                for (i, ms) in class_methods.iter().enumerate() {
                    let comma = if i + 1 < class_methods.len() { "," } else { "" };
                    self.decls
                        .push_str(&format!("  __{}_{}{}\n", name, ms.name, comma));
                }
                self.decls.push_str("};\n\n");
            }
            // Simple struct definition
            Statement::Struct { name, fields, .. } => {
                self.decls
                    .push_str(&format!("typedef struct {} {};\n", name, name));
                self.decls.push_str(&format!("struct {} {{\n", name));
                for f in fields {
                    self.decls.push_str(&format!(
                        "  {} {};\n",
                        type_to_c(&f.var_type),
                        f.name
                    ));
                }
                self.decls.push_str("};\n\n");
            }
            Statement::Block { body, .. } => {
                out.push("{".into());
                for ss in &body.statements {
                    out.extend(self.transform_statement(ss, false));
                }
                out.push("}".into());
            }
        }
        out
    }

    // Emit expression into a C expression string, returning any prelude statements and the expr
    fn transform_expression(&mut self, e: &Expression) -> (Vec<String>, String) {
        match e {
            Expression::Number { value, .. } => (vec![], format!("{}", value)),
            Expression::Float { value, .. } => {
                // Ensure decimal point
                let s = if value.fract() == 0.0 {
                    format!("{:.1}", value)
                } else {
                    format!("{}", value)
                };
                (vec![], s)
            }
            Expression::Boolean { value, .. } => (
                vec![],
                if *value {
                    "true".into()
                } else {
                    "false".into()
                },
            ),
            Expression::StringLiteral { value, .. } => (vec![], Self::escape_c_string(value)),
            Expression::Variable {
                var: Variable { name, .. },
                ..
            } => (vec![], name.clone()),
            Expression::BinaryOp {
                op, left, right, ..
            } => {
                let (lc, le) = self.transform_expression(left);
                let (rc, re) = self.transform_expression(right);
                let mut code = lc;
                code.extend(rc);
                (code, format!("({} {} {})", le, op, re))
            }
            Expression::UnaryOp { op, expr, .. } => {
                let (c, e1) = self.transform_expression(expr);
                (c, format!("({}{})", op, e1))
            }
            Expression::Cast {
                expr, target_type, ..
            } => {
                let (c, e1) = self.transform_expression(expr);
                (c, format!("(({})({}))", type_to_c(target_type), e1))
            }
            Expression::Call { callee, args, .. } => {
                if let Expression::Variable { var, .. } = callee.as_ref() {
                    // Constructor call pattern: ClassName(...)
                    let class_hit = self.classes.get(&var.name).cloned();
                    if let Some(class) = class_hit {
                        // Emit any arg code for side effects and build struct literal with vtable pointer first
                        let mut code = Vec::new();
                        let args_local = args.clone();
                        for a in args_local {
                            let (c, _e) = self.transform_expression(&a);
                            code.extend(c);
                        }
                        let class_name = class.name;
                        let field_count = class.fields.len();
                        let mut parts = vec![format!("&k_{}VTable", class_name)];
                        parts.extend((0..field_count).map(|_| "0".to_string()));
                        return (
                            code,
                            format!("(struct {}){{{}}}", class_name, parts.join(", ")),
                        );
                    }
                    let mut code = Vec::new();
                    let exprs = args
                        .iter()
                        .map(|a| {
                            let (c, e) = self.transform_expression(a);
                            code.extend(c);
                            e
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    (code, format!("{}({})", var.name, exprs))
                } else {
                    panic!(
                        "TargetC: unsupported call callee in expression: {:?}",
                        callee
                    )
                }
            }
            Expression::InitializerList { elements, .. } => {
                // Minimal support: transform to a compound literal of ints/doubles/strings if used inline
                let mut code = Vec::new();
                let parts = elements
                    .iter()
                    .map(|el| {
                        let (c, e) = self.transform_expression(el);
                        code.extend(c);
                        e
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                (code, format!("{{{}}}", parts))
            }
            Expression::MethodCall {
                object,
                method,
                args,
                span,
            } => {
                // Debug aid: mirror LLVM's caret diagnostics to validate parser spans
                println!(">>> TargetC::Expression::MethodCall");
                self._source.caret(*span);

                let (mut code, obj_expr) = self.transform_expression(object);
                let mut arg_exprs: Vec<String> = Vec::new();
                let args_local = args.clone();
                for a in args_local {
                    let (c, ex) = self.transform_expression(&a);
                    code.extend(c);
                    arg_exprs.push(ex);
                }

                // If calling on `self` (pointer), use ->; otherwise, use . and take & for self arg
                let (vt_expr, self_arg) = if let Expression::Variable { var, .. } = &**object
                    && var.name == "self"
                {
                    (format!("{}->vtable", obj_expr), obj_expr.clone())
                } else {
                    (format!("{}.vtable", obj_expr), format!("&{}", obj_expr))
                };
                let args_joined = if arg_exprs.is_empty() {
                    String::new()
                } else {
                    format!(", {}", arg_exprs.join(", "))
                };
                (
                    code,
                    format!("{}->{}({}{})", vt_expr, method, self_arg, args_joined),
                )
            }
            Expression::FieldAccess { object, field, .. } => {
                let (c, oexpr) = self.transform_expression(object);
                if let Expression::Variable { var, .. } = &**object
                    && var.name == "self"
                {
                    (c, format!("{}->{}", oexpr, field))
                } else {
                    (c, format!("{}.{}", oexpr, field))
                }
            }
            Expression::StructLiteral { name, fields, .. } => {
                // Build designated initializer: (struct Name){ .field = expr, ... }
                let mut code = Vec::new();
                let mut parts: Vec<String> = Vec::new();
                for (fname, fexpr) in fields {
                    let (c, ex) = self.transform_expression(fexpr);
                    code.extend(c);
                    parts.push(format!(".{} = {}", fname, ex));
                }
                (code, format!("(struct {}){{{}}}", name, parts.join(", ")))
            }
            Expression::ArrayAccess { .. } => {
                if let Expression::ArrayAccess { array, index, .. } = e {
                    let (mut code, arr_expr) = self.transform_expression(array);
                    let (ic, idx_expr) = self.transform_expression(index);
                    code.extend(ic);
                    (code, format!("{}[{}]", arr_expr, idx_expr))
                } else {
                    unreachable!()
                }
            }
        }
    }

    fn escape_c_string(s: &str) -> String {
        let escaped = s
            .replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\t', "\\t");
        format!("\"{}\"", escaped)
    }
}

impl<'a> Backend<'a> for TargetC<'a> {
    fn compile(&mut self) {
        self.headers
            .push_str("#include <stdio.h>\n#include <stdint.h>\n#include <stdbool.h>\n\n");

        // Walk top-level statements
        let statements = self.ast.get().unwrap().statements.clone();
        for s in &statements {
            match s {
                Statement::Function { .. } => {
                    // emit into function store
                    let _ = self.transform_statement(s, true);
                }
                _ => {
                    let lines = self.transform_statement(s, true);
                    self.main_body.extend(lines);
                }
            }
        }

        // Assemble final C program
        let mut out = String::new();
        out.push_str(&self.headers);
        if !self.decls.is_empty() {
            out.push_str(&self.decls);
            out.push('\n');
        }
        for f in &self.functions {
            out.push_str(f);
        }
        out.push_str("int main() {\n");
        for l in &self.main_body {
            out.push_str("  ");
            out.push_str(l);
            out.push('\n');
        }
        out.push_str("  return 0;\n}\n");
        self.decls.clear();
        self.functions.shrink_to_fit();
        self.code = out;
    }

    fn output(&self) -> String {
        self.code.clone()
    }

    fn from_ast(ast: Ast<'a>, source: &'a SourceFile) -> Self {
        Self::new(ast, source)
    }
}
