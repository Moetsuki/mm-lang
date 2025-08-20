use crate::block::Block;
use crate::expression::Expression;
use crate::file::SourceFile;
use crate::span::Span;
use crate::statement::Statement;
use crate::statement::Visibility;
use crate::tokenizer::LexicalToken;
use crate::tokenizer::OPERATORS;
use crate::tokenizer::Token;
use crate::types::Type;
use crate::variable::Variable;
use std::collections::HashSet;
use std::str::FromStr;

pub struct Ast<'a> {
    tokens: Vec<LexicalToken>,
    pos: usize,
    tree: Option<Block>,
    source_file: &'a SourceFile,
    // Track known type names to disambiguate constructs during parsing
    class_names: HashSet<String>,
    struct_names: HashSet<String>,
}

impl<'a> Ast<'a> {
    pub fn new(tokens: Vec<LexicalToken>, source_file: &'a SourceFile) -> Self {
        Ast {
            tokens,
            pos: 0,
            tree: None,
            source_file,
            class_names: HashSet::new(),
            struct_names: HashSet::new(),
        }
    }

    pub fn get(&self) -> Option<&Block> {
        self.tree.as_ref()
    }

    pub fn backtrack(&mut self) {
        if self.pos > 0 {
            self.pos -= 1;
        }
    }

    pub fn next_token(&mut self) -> Option<&LexicalToken> {
        if self.pos < self.tokens.len() {
            let token = &self.tokens[self.pos];
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }
    pub fn peek_token(&self) -> Option<&LexicalToken> {
        if self.pos < self.tokens.len() {
            Some(&self.tokens[self.pos])
        } else {
            None
        }
    }

    pub fn expect(&mut self, expected: &Token) -> Span {
        if let Some(lexical_token) = self.next_token().as_ref() {
            if &lexical_token.token != expected {
                panic!(
                    "Expected {:?}, found {:?} at line {}, column {}",
                    expected, lexical_token.token, lexical_token.line, lexical_token.column
                );
            }
            lexical_token.span
        } else {
            panic!("Expected {:?}, but no more tokens available", expected);
        }
    }

    pub fn expect_operator(&mut self) {
        if let Some(lexical_token) = self.next_token()
            && !OPERATORS.contains(&lexical_token.token.to_string().as_str())
        {
            panic!(
                "Expected an operator, found {:?} at line {}, column {}",
                lexical_token.token, lexical_token.line, lexical_token.column
            );
        }
    }

    pub fn parse(&mut self) -> Block {
        let mut statements = Vec::new();

        while let Some(lexical_token) = self.next_token() {
            let lexical_token_span = lexical_token.span;
            match &lexical_token.token {
                //
                // Handle Identifiers:
                //
                //  ---- Assignment: `identifier : type = value;`
                //  -------- Example: `x : type = 42;`
                //
                //  ---- Function calls: `identifier(arg1, arg2);`
                //  -------- Example: `print("Hello, World!");`
                //
                //  ---- Method calls: `object.method(arg1, arg2);`
                //  -------- Example: `my_list.append(5);`
                Token::Identifier(name) => {
                    let name_clone = name.clone();

                    // Peek next token to match if its an operator, a function call, or a method call
                    if let Some(next_token) = self.peek_token() {
                        match &next_token.token {
                            //
                            // Declaration case
                            //
                            Token::Punctuation(p) if p == ":" => {
                                let mut var_info = Variable {
                                    name: name_clone,
                                    var_type: Type::ToBeEvaluated("invalid".to_string()), // Default type, will be set later
                                };

                                // Get next token and make sure its an identifier and a valid type
                                self.next_token();
                                if let Some(type_token) = self.peek_token() {
                                    match &type_token.token {
                                        Token::Identifier(type_name) => {
                                            var_info.var_type = Type::from_str(type_name)
                                                .unwrap_or(Type::ToBeEvaluated(type_name.clone()));
                                            self.next_token(); // consume the type identifiers
                                            self.expect_operator(); // consume the = sign
                                            let expr = self.parse_expr();
                                            statements.push(Statement::VariableDecl {
                                                identifier: var_info,
                                                value: expr.clone(),
                                                span: lexical_token_span.join(expr.span()),
                                            });
                                        }
                                        Token::Keyword(key) if key == "tensor" => {
                                            self.next_token(); // consume the `array` keyword
                                            self.expect(&Token::Punctuation("[".to_string()));
                                            let arr_type = self.parse_type();
                                            self.expect(&Token::Punctuation("]".to_string()));
                                            // TODO: continue this ....
                                            var_info.var_type = Type::Tensor {
                                                var_type: Box::new(arr_type.clone()),
                                                dimensions: vec![], // Dimensions will be set later
                                                                    // or automatically evaluated during compilation
                                            };
                                            self.expect_operator(); // consume the = sign
                                            let expr = self.parse_expr();
                                            statements.push(Statement::VariableDecl {
                                                identifier: var_info,
                                                value: expr.clone(),
                                                span: lexical_token_span.join(expr.span()),
                                            })
                                        }
                                        _ => {
                                            panic!(
                                                "Unexpected token `{}`, expected a type identifier!",
                                                type_token.token
                                            )
                                        }
                                    }
                                }
                            }
                            //
                            // Assignment case:
                            //
                            Token::Operator(_) => {
                                // Handle operator case
                                self.expect_operator();
                                let expr = self.parse_expr();
                                statements.push(Statement::Assignment {
                                    identifier: Expression::Variable {
                                        var: Variable {
                                            name: name_clone,
                                            var_type: Type::ToBeEvaluated("invalid".to_string()),
                                        },
                                        span: expr.span().join(lexical_token_span),
                                    },
                                    value: expr.clone(),
                                    span: lexical_token_span.join(expr.span()),
                                });
                            }
                            //
                            // Function call case:
                            //
                            Token::Punctuation(p) if p == "(" => {
                                // Handle function call case
                                let callee = Expression::Variable {
                                    var: Variable {
                                        name: name_clone,
                                        var_type: Type::ToBeEvaluated("invalid".to_string()),
                                    },
                                    span: lexical_token_span,
                                };
                                let call_statement = self.parse_call(callee);
                                statements.push(call_statement);
                            }
                            Token::Punctuation(p) if p == "[" => {
                                // Handle array access case (e.g., arr[0] = value;)
                                self.next_token(); // consume '['
                                let index = self.parse_expr();

                                let end_bracket_span = if let Some(end_bracket_token) =
                                    self.peek_token()
                                    && end_bracket_token.token
                                        == Token::Punctuation("]".to_string())
                                {
                                    let eb = end_bracket_token.span;
                                    self.next_token(); // consume ']'
                                    eb
                                } else {
                                    panic!(
                                        "Expected ']', but found end of input or unexpected token"
                                    );
                                };

                                // Expect an assignment operator after the array access
                                if let Some(next) = self.peek_token()
                                    && let Token::Operator(op) = &next.token
                                    && op == "="
                                {
                                    self.next_token(); // consume '='
                                    let value = self.parse_expr();
                                    statements.push(Statement::Assignment {
                                        identifier: Expression::ArrayAccess {
                                            array: Box::new(Expression::Variable {
                                                var: Variable {
                                                    name: name_clone,
                                                    var_type: Type::ToBeEvaluated(
                                                        "invalid".to_string(),
                                                    ),
                                                },
                                                span: lexical_token_span,
                                            }),
                                            index: Box::new(index),
                                            span: lexical_token_span.join(end_bracket_span),
                                        },
                                        value: value.clone(),
                                        span: lexical_token_span.join(value.span()),
                                    });
                                    continue;
                                } else {
                                    panic!("Expected assignment after array access!");
                                }
                            }
                            //
                            // Method call or field access case (e.g., self.id = x; or obj.method(...);)
                            //
                            Token::Punctuation(p) if p == "." => {
                                // Backtrack to the identifier token to parse the full lhs (e.g., self.id)
                                self.backtrack();

                                // Parse the full postfix expression (handles chained . and calls)
                                let lhs = self.parse_postfix();

                                // If this is an assignment to a field (e.g., self.id = expr;)
                                if let Some(next) = self.peek_token()
                                    && let Token::Operator(op) = &next.token
                                    && op == "="
                                {
                                    self.next_token(); // consume '='
                                    let rhs = self.parse_expr();
                                    statements.push(Statement::Assignment {
                                        identifier: lhs,
                                        value: rhs.clone(),
                                        span: lexical_token_span.join(rhs.span()),
                                    });
                                    continue;
                                }

                                // Not an assignment; at this point we might have a pure method call like obj.method(...);
                                match lhs {
                                    // If it's a function-style call like foo(...), turn it into a Call statement.
                                    Expression::Call { callee, args, .. } => {
                                        let total_span = if args.is_empty() {
                                            lexical_token_span
                                        } else {
                                            lexical_token_span.join(args.last().unwrap().span())
                                        };
                                        statements.push(Statement::Call {
                                            callee: *callee,
                                            args,
                                            span: total_span,
                                        });
                                    }
                                    Expression::MethodCall { .. } => {
                                        panic!(
                                            "Method call statements are not yet supported at this point. Consider adding Statement::MethodCall or lowering to Call on a field access."
                                        );
                                    }
                                    other => {
                                        panic!(
                                            "Unexpected expression after '.', expected assignment or call, found {:?}",
                                            other
                                        );
                                    }
                                }
                            }
                            Token::Identifier(_) => {
                                // Handle method call case
                            }
                            _ => {}
                        }
                    }
                }
                Token::Keyword(keyword) if keyword == "if" => {
                    // Make sure we can handle nested if statements
                    let condition = self.parse_expr();

                    let mut total_span = lexical_token_span;
                    self.expect(&Token::Punctuation("{".to_string()));
                    let then_block = self.parse();
                    total_span = then_block.span().unwrap_or(total_span).join(total_span);
                    self.expect(&Token::Punctuation("}".to_string())); // consume the closing brace

                    let mut elif: Vec<Statement> = vec![];

                    // We return the else block if available and push all `else if` blocks into the
                    // vector above to deduplicate logic.
                    let mut else_block: Option<Block> = None;

                    while let Some(lexical_token) = self.peek_token() {
                        if lexical_token.token == Token::Keyword("else".to_string()) {
                            self.next_token(); // consume 'else'

                            println!("dsd");

                            // Check if we have an else_if statement
                            let (is_elif, else_if_cond) = if let Some(lexical_token_2) =
                                self.peek_token()
                                && lexical_token_2.token == Token::Keyword("if".to_string())
                            {
                                self.next_token(); // consume 'if'
                                (true, Some(self.parse_expr()))
                            } else {
                                (false, None)
                            };

                            // Parse the else - else_if block
                            self.expect(&Token::Punctuation("{".to_string()));
                            let else_elif_block = self.parse();
                            self.expect(&Token::Punctuation("}".to_string())); // consume the closing brace

                            let sub_total_span =
                                total_span.join(else_elif_block.span().unwrap_or(total_span));

                            if is_elif {
                                elif.push(Statement::If {
                                    condition: else_if_cond.unwrap(),
                                    then_block: else_elif_block,
                                    elif: vec![],
                                    else_block: None,
                                    span: sub_total_span,
                                });
                                continue; // continue parsing for more else else_if blocks
                            }

                            total_span = sub_total_span;
                            else_block = Some(else_elif_block);

                            break; // found else block, finished ---- don't allow else_if statemtns after else
                        }
                        break; // no more else blocks to parse
                    }

                    statements.push(Statement::If {
                        condition,
                        then_block,
                        elif,
                        else_block,
                        span: total_span,
                    });
                }
                Token::Keyword(keyword) if keyword == "function" => {
                    let mut stm = self.parse_function();
                    let sp = stm.span().join(lexical_token_span);
                    *stm.span_mut() = sp;
                    statements.push(stm);
                }
                Token::Keyword(keyword) if keyword == "return" => {
                    //
                    // Handle return statements
                    //
                    let expr = self.parse_expr();
                    statements.push(Statement::Return {
                        value: expr.clone(),
                        span: lexical_token_span.join(expr.span()),
                    });
                }
                Token::Keyword(keyword) if keyword == "class" => {
                    //
                    // Handle class definitions
                    //
                    let class_name = if let Some(lexical_token) = self.next_token() {
                        if let Token::Identifier(name) = &lexical_token.token {
                            name.clone()
                        } else {
                            panic!(
                                "Expected class name, found {:?} at line {}, column {}",
                                lexical_token.token, lexical_token.line, lexical_token.column
                            );
                        }
                    } else {
                        panic!("Expected class name, but no more tokens available");
                    };

                    // Record known class name early for downstream disambiguation
                    self.class_names.insert(class_name.clone());

                    //
                    // Check if we have a parent class
                    //
                    let parent_class: Option<String> = if let Some(colon_token) = self.peek_token()
                    {
                        if colon_token.token == Token::Punctuation(":".to_string()) {
                            self.next_token(); // consume ':'

                            // Next token should be the parent class identifier
                            if let Some(parent_token) = self.next_token() {
                                if let Token::Identifier(parent_name) = &parent_token.token {
                                    Some(parent_name.clone())
                                } else {
                                    panic!(
                                        "Expected parent class name, found {:?} at line {}, column {}",
                                        parent_token.token, parent_token.line, parent_token.column
                                    );
                                }
                            } else {
                                panic!("Expected parent class name, but no more tokens available");
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    //
                    // Consume the opening brace for the class body
                    //

                    self.expect(&Token::Punctuation("{".to_string()));

                    let mut fields = Vec::new();
                    let mut methods = Vec::new();

                    let mut constructor_defined = false;
                    let mut destructor_defined = false;

                    // Each field or method must be prefixed with a visibility keyword
                    while let Some(class_token) = self.peek_token() {
                        // println!("Parsing class token: {:?}", class_token.token);
                        match &class_token.token {
                            Token::Keyword(vis)
                                if vis == "public" || vis == "private" || vis == "protected" =>
                            {
                                let visibility = match vis.as_str() {
                                    "public" => Visibility::Public,
                                    "private" => Visibility::Private,
                                    "protected" => Visibility::Protected,
                                    _ => unreachable!(),
                                };
                                self.next_token(); // consume the visibility keyword

                                if let Some(field_token) = self.peek_token() {
                                    match &field_token.token {
                                        Token::Identifier(field_name) => {
                                            // Extract field before mutable borrow on self
                                            let field_name = field_name.clone();
                                            self.next_token(); // consume the identifier
                                            self.expect(&Token::Punctuation(":".to_string()));
                                            let field_type = self.parse_type();
                                            fields.push((
                                                Variable {
                                                    name: field_name,
                                                    var_type: field_type,
                                                },
                                                visibility,
                                            ));
                                            self.expect(&Token::Punctuation(";".to_string())); // consume the semicolon
                                        }
                                        Token::Keyword(method_keyword)
                                            if method_keyword == "function" =>
                                        {
                                            // Handle method declaration
                                            self.next_token(); // consume 'function'
                                            let method = self.parse_function();
                                            methods.push((Box::new(method), visibility));
                                        }
                                        _ => {
                                            panic!(
                                                "Expected a field or method after visibility keyword"
                                            );
                                        }
                                    }
                                } else {
                                    panic!("Expected a field or method after visibility keyword");
                                }
                            }
                            Token::Keyword(keyword) if keyword == "init" => {
                                // Handle special init/destroy methods
                                constructor_defined = true;
                                let method = self.parse_constructor_destructor(class_name.clone());
                                methods.insert(0, (Box::new(method), Visibility::Public));
                            }
                            Token::Keyword(keyword) if keyword == "destroy" => {
                                // Handle special init/destroy methods
                                destructor_defined = true;
                                let method = self.parse_constructor_destructor(class_name.clone());
                                methods.insert(
                                    // Insert it at index 0 if we have only a destructor
                                    // The default constructor will be pushed in front of it anyway later.
                                    if methods.is_empty() { 0 } else { 1 },
                                    (Box::new(method), Visibility::Public),
                                );
                            }
                            Token::Newline => {
                                // Ignore newlines
                                self.next_token(); // consume the newline
                            }
                            _ => {
                                // Any other keyword other than a visibility keyword means we are done with the class definition
                                break;
                            }
                        }
                    }

                    // If no constructor is defined, we define a default one
                    if !constructor_defined {
                        methods.insert(
                            0,
                            (
                                Box::new(Statement::Function {
                                    name: format!("__{}_init", class_name),
                                    ret_type: Type::Void,
                                    params: Vec::new(),
                                    body: Block::new(Vec::new()),
                                    span: Span::new(lexical_token_span.file, 0, 0),
                                }),
                                Visibility::Public,
                            ),
                        );
                    }

                    // If no destructor is defined, we define a default one
                    if !destructor_defined {
                        methods.insert(
                            1,
                            (
                                Box::new(Statement::Function {
                                    name: format!("__{}_destroy", class_name),
                                    ret_type: Type::Void,
                                    params: Vec::new(),
                                    body: Block::new(Vec::new()),
                                    span: Span::new(lexical_token_span.file, 0, 0),
                                }),
                                Visibility::Public,
                            ),
                        );
                    }

                    self.expect(&Token::Punctuation("}".to_string())); // consume the closing brace

                    let total_span = {
                        if let Some(peek_tok) = self.peek_token() {
                            peek_tok.span.join(lexical_token_span)
                        } else {
                            lexical_token_span
                        }
                    };

                    statements.push(Statement::Class {
                        name: class_name,
                        parent: parent_class,
                        fields,
                        methods,
                        span: total_span,
                    });

                    self.expect(&Token::Punctuation(";".to_string())); // consume the semicolon
                }
                Token::Keyword(keyword) if keyword == "struct" => {
                    // Parse a simple struct definition: struct Name { field: type; ... };
                    let struct_name = if let Some(tok) = self.next_token() {
                        if let Token::Identifier(name) = &tok.token {
                            name.clone()
                        } else {
                            panic!(
                                "Expected struct name, found {:?} at line {}, column {}",
                                tok.token, tok.line, tok.column
                            );
                        }
                    } else {
                        panic!("Expected struct name, but no more tokens available");
                    };

                    // Record known struct name early so expressions like `Point { ... }` can be recognized,
                    // while avoiding mis-parsing identifiers before a block start (e.g., `if x < y {`).
                    self.struct_names.insert(struct_name.clone());

                    // Optional inheritance: struct Name : Parent { ... } not used yet but parsed
                    let _parent_struct: Option<String> = if let Some(colon_token) =
                        self.peek_token()
                    {
                        if colon_token.token == Token::Punctuation(":".to_string()) {
                            self.next_token(); // consume ':'
                            if let Some(parent_token) = self.next_token() {
                                if let Token::Identifier(parent_name) = &parent_token.token {
                                    Some(parent_name.clone())
                                } else {
                                    panic!(
                                        "Expected parent struct name, found {:?} at line {}, column {}",
                                        parent_token.token, parent_token.line, parent_token.column
                                    );
                                }
                            } else {
                                panic!("Expected parent struct name, but no more tokens available");
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    self.expect(&Token::Punctuation("{".to_string()));

                    let mut fields: Vec<Variable> = Vec::new();
                    loop {
                        if let Some(tok) = self.peek_token() {
                            match &tok.token {
                                Token::Identifier(field_name) => {
                                    let field_name = field_name.clone();
                                    self.next_token();
                                    self.expect(&Token::Punctuation(":".to_string()));
                                    let field_type = self.parse_type();
                                    fields.push(Variable {
                                        name: field_name,
                                        var_type: field_type,
                                    });

                                    // Accept ',' or ';' as separators and allow trailing comma before '}'
                                    if matches!(self.peek_token().map(|t| &t.token), Some(Token::Punctuation(p)) if p == "," || p == ";")
                                    {
                                        self.next_token();
                                    }
                                    continue;
                                }
                                Token::Newline => {
                                    self.next_token();
                                    continue;
                                }
                                Token::Punctuation(p) if p == "}" => {
                                    self.next_token();
                                    break;
                                }
                                _ => panic!("Unexpected token in struct body: {:?}", tok.token),
                            }
                        } else {
                            panic!("Unterminated struct body");
                        }
                    }

                    let total_span = {
                        if let Some(peek_tok) = self.peek_token() {
                            peek_tok.span.join(lexical_token_span)
                        } else {
                            lexical_token_span
                        }
                    };

                    statements.push(Statement::Struct {
                        id: 0,
                        name: struct_name,
                        parent: None,
                        fields,
                        span: total_span,
                    });

                    // Optional trailing semicolon to be consistent with class style
                    if matches!(self.peek_token().map(|t| &t.token), Some(Token::Punctuation(p)) if p == ";")
                    {
                        self.next_token();
                    }
                }
                Token::Newline => {
                    //
                    // Ignore newlines
                    //
                }
                Token::Punctuation(p) if p == ";" => {
                    //
                    // Ignore semicolons
                    //
                }
                Token::Punctuation(p) if p == "}" => {
                    //
                    // End of block - put the token back and break
                    //
                    self.pos -= 1; // Put the token back for the caller to handle
                    break;
                }
                Token::Punctuation(p) if p == "{" => {
                    let block = self.parse();

                    self.expect(&Token::Punctuation("}".to_string()));

                    statements.push(Statement::Block {
                        body: block.clone(),
                        span: block.span().unwrap_or(lexical_token_span),
                    });
                }
                _ => {
                    //
                    // Handle other tokens
                    //
                    panic!(
                        "Unexpected token: {:?} at line {} column {}",
                        lexical_token.token, lexical_token.line, lexical_token.column
                    );
                }
            }
        }

        let result = Block::new(statements);
        self.tree = Some(result.clone());

        result
    }

    // Parses a type declaration (e.g., `i32`, `f64`, `String`)
    fn parse_type(&mut self) -> Type {
        if let Some(lexical_token) = self.next_token() {
            match &lexical_token.token {
                Token::Identifier(type_name) => Type::from_str(type_name).unwrap_or_else(|_| {
                    panic!(
                        "Unknown type `{}` at line {}, column {}",
                        type_name, lexical_token.line, lexical_token.column
                    )
                }),
                _ => panic!(
                    "Expected a type identifier, found {:?} at line {}, column {}",
                    lexical_token.token, lexical_token.line, lexical_token.column
                ),
            }
        } else {
            panic!("Expected a type identifier, but no more tokens available");
        }
    }

    // Handle function call, parenthesis after an identifier
    fn parse_call(&mut self, callee: Expression) -> Statement {
        self.expect(&Token::Punctuation("(".to_string()));
        let args = self.parse_args();
        let total_span = {
            if let Some(peek_tok) = self.peek_token() {
                peek_tok.span.join(callee.span())
            } else {
                callee.span()
            }
        };
        self.expect(&Token::Punctuation(")".to_string()));
        Statement::Call {
            callee,
            args,
            span: total_span,
        }
    }

    // Parses a list of arguments for a function call
    fn parse_args(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();
        while let Some(lexical_token) = self.peek_token() {
            match &lexical_token.token {
                Token::Identifier(_)
                | Token::Number(_)
                | Token::NumberFloat(_)
                | Token::StringLiteral(_) => {
                    args.push(self.parse_expr());
                    if let Some(next_token) = self.peek_token() {
                        if next_token.token == Token::Punctuation(",".to_string()) {
                            self.next_token(); // consume ','
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                // Unary Minus Case
                Token::Operator(p) if p == "-" => {
                    args.push(self.parse_expr());
                    if let Some(next_token) = self.peek_token() {
                        if next_token.token == Token::Punctuation(",".to_string()) {
                            self.next_token(); // consume ','
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        args
    }

    // Handles literals and identifiers
    fn parse_factor(&mut self) -> Expression {
        if let Some(lexical_token) = self.peek_token() {
            let lexical_token_span = lexical_token.span;
            match &lexical_token.token {
                Token::Number(n) => {
                    let value = *n;
                    self.next_token(); // consume the value
                    Expression::Number {
                        value,
                        span: lexical_token_span,
                    }
                }
                Token::NumberFloat(f) => {
                    let value = *f;
                    self.next_token();
                    Expression::Float {
                        value,
                        span: lexical_token_span,
                    }
                }
                Token::Keyword(key) if key == "true" || key == "false" => {
                    let value = key == "true";
                    self.next_token(); // consume the value
                    Expression::Boolean {
                        value,
                        span: lexical_token_span,
                    }
                }
                Token::Operator(p) if p == "-" => {
                    self.next_token(); // consume the unary minus operator
                    let next_tok = self.peek_token();
                    if let Some(next_tok) = next_tok {
                        match &next_tok.token {
                            Token::Number(n) => {
                                let result = Expression::Number {
                                    value: -(*n),
                                    span: lexical_token_span.join(next_tok.span),
                                };
                                self.next_token(); // consume the number
                                result
                            }
                            Token::NumberFloat(f) => {
                                let result = Expression::Float {
                                    value: -(*f),
                                    span: lexical_token_span.join(next_tok.span),
                                };
                                self.next_token(); // consume the number
                                result
                            }
                            _ => {
                                let operand = self.parse_factor();
                                Expression::UnaryOp {
                                    op: "-".to_string(),
                                    expr: Box::new(operand.clone()),
                                    span: lexical_token_span.join(operand.span()),
                                }
                            }
                        }
                    } else {
                        panic!("Unexpected end of input after unary minus");
                    }
                }
                Token::StringLiteral(s) => {
                    let value = s.clone();
                    self.next_token();
                    Expression::StringLiteral {
                        value,
                        span: lexical_token_span,
                    }
                }
                Token::Identifier(name) => {
                    // Could be variable or struct literal like Point { x: 1, y: 2 }
                    let name_clone = name.clone();
                    self.next_token();

                    // If next token is '{' AND identifier is a known struct name, parse struct literal.
                    // This avoids mis-parsing cases like `if x < y {` where `y {` begins a block.
                    let mut is_struct_literal = false;
                    let obr_span_opt = if let Some(tok) = self.peek_token() {
                        if tok.token == Token::Punctuation("{".to_string())
                            && self.struct_names.contains(&name_clone)
                        {
                            is_struct_literal = true;
                            Some(tok.span)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    if is_struct_literal {
                        let _obr_span =
                            obr_span_opt.expect("brace span must exist for struct literal");
                        self.next_token(); // consume '{'
                        let mut fields: Vec<(String, Expression)> = Vec::new();
                        let end_span = loop {
                            if let Some(t) = self.peek_token() {
                                match &t.token {
                                    Token::Punctuation(p) if p == "}" => {
                                        let s = t.span;
                                        self.next_token(); // consume '}'
                                        break s;
                                    }
                                    Token::Identifier(field_name) => {
                                        let field_name = field_name.clone();
                                        self.next_token(); // consume identifier
                                        self.expect(&Token::Punctuation(":".to_string()));
                                        let value_expr = self.parse_expr();
                                        fields.push((field_name, value_expr.clone()));
                                        // optional comma
                                        if matches!(self.peek_token().map(|t| &t.token), Some(Token::Punctuation(p)) if p == ",")
                                        {
                                            self.next_token();
                                        }
                                    }
                                    Token::Newline => {
                                        self.next_token();
                                    }
                                    _ => panic!("Unexpected token in struct literal"),
                                }
                            } else {
                                panic!("Unterminated struct literal");
                            }
                        };
                        Expression::StructLiteral {
                            name: name_clone,
                            fields,
                            span: lexical_token_span.join(end_span),
                        }
                    } else {
                        Expression::Variable {
                            var: Variable {
                                name: name_clone,
                                var_type: Type::ToBeEvaluated("invalid".to_string()),
                            },
                            span: lexical_token_span,
                        }
                    }
                }
                Token::Punctuation(p) if p == "(" => {
                    self.next_token();
                    let mut expr = self.parse_expr();
                    let tok_span = self.expect(&Token::Punctuation(")".to_string()));
                    *expr.span_mut() = lexical_token_span.join(tok_span);
                    expr
                }
                other => panic!("Unexpected token: {:?}", other),
            }
        } else {
            panic!("Unexpected end of input while parsing factor");
        }
    }

    // Handles postfix operators like method/field access and function calls with high precedence
    fn parse_postfix(&mut self) -> Expression {
        let mut expr = self.parse_factor();

        while let Some(lexical_token) = self.peek_token() {
            match &lexical_token.token {
                // Indexing: expr[ index ]
                Token::Punctuation(p) if p == "[" => {
                    self.next_token(); // consume '['
                    let index_expr = self.parse_expr();

                    // Expect closing ']'
                    let end_span = self.expect(&Token::Punctuation("]".to_string()));

                    let new_span = expr.span().join(end_span);
                    expr = Expression::ArrayAccess {
                        array: Box::new(expr.clone()),
                        index: Box::new(index_expr),
                        span: new_span,
                    };
                }
                Token::Punctuation(p) if p == "." => {
                    self.next_token(); // consume the '.'

                    // After the dot we expect an identifier
                    if let Some(tok) = self.next_token() {
                        let member_name = if let Token::Identifier(name) = &tok.token {
                            name.clone()
                        } else {
                            panic!(
                                "Expected member name after '.', found {:?} at line {}, column {}",
                                tok.token, tok.line, tok.column
                            );
                        };

                        let tok_span = tok.span;

                        // Determine if this is a method call (next token is '(') or a field access
                        let is_method_call = matches!(
                            self.peek_token(),
                            Some(next_next_token)
                                if next_next_token.token == Token::Punctuation("(".to_string())
                        );

                        if is_method_call {
                            self.expect(&Token::Punctuation("(".to_string()));
                            let args = self.parse_args();
                            self.expect(&Token::Punctuation(")".to_string()));
                            expr = Expression::MethodCall {
                                object: Box::new(expr.clone()),
                                method: member_name,
                                args: args.clone(),
                                span: expr.span().join(
                                    args.iter()
                                        .map(|arg| arg.span())
                                        .reduce(|a, b| a.join(b))
                                        .unwrap_or(expr.span().join(tok_span)),
                                ),
                            };
                        } else {
                            expr = Expression::FieldAccess {
                                object: Box::new(expr.clone()),
                                field: member_name,
                                span: expr.span().join(tok_span),
                            };
                        }
                    } else {
                        panic!("Expected member name after '.', but no more tokens available");
                    }
                }
                Token::Punctuation(p) if p == "(" => {
                    // Function call on the current expression
                    self.next_token(); // consume '('
                    let args = self.parse_args();
                    self.expect(&Token::Punctuation(")".to_string()));
                    expr = Expression::Call {
                        callee: Box::new(expr.clone()),
                        args: args.clone(),
                        span: expr.span().join(
                            args.iter()
                                .map(|arg| arg.span())
                                .reduce(|a, b| a.join(b))
                                .unwrap_or(expr.span()),
                        ),
                    };
                }
                _ => break,
            }
        }

        expr
    }

    // Entry point for expressions (lowest precedence)
    fn parse_expr(&mut self) -> Expression {
        // Precedence climbing from lowest to highest:
        // initializer_list -> logical_or -> logical_and -> comparison -> additive -> term -> cast -> postfix -> factor
        self.parse_initializer_list()
    }

    fn parse_initializer_list(&mut self) -> Expression {
        let mut elements = Vec::new();

        let lexical_token_span = if let Some(lexical_token) = self.peek_token()
            && lexical_token.token == Token::Punctuation("{".to_string())
        {
            let sp = lexical_token.span;
            self.next_token(); // consume '{'
            sp
        } else {
            return self.parse_logical_or(); // If not an initializer list, continue parsing 
            // next syntactical construct in precedence
        };

        let mut token_span = lexical_token_span;

        while let Some(lexical_token) = self.peek_token() {
            if lexical_token.token == Token::Punctuation("}".to_string()) {
                self.next_token(); // consume '}'
                break;
            }
            let expr = self.parse_expr();
            elements.push(expr.clone());
            token_span = token_span.join(expr.span());

            if let Some(next_token) = self.peek_token() {
                if next_token.token == Token::Punctuation(",".to_string()) {
                    self.next_token(); // consume ','
                } else if next_token.token == Token::Punctuation("}".to_string()) {
                    self.next_token(); // consume ']'
                    break;
                } else {
                    panic!("Expected ',' or '}}', found {:?}", next_token.token);
                }
            } else {
                panic!("Unexpected end of input while parsing initializer list");
            }
        }
        Expression::InitializerList {
            elements,
            span: token_span,
        }
    }

    // New: logical OR (||)
    fn parse_logical_or(&mut self) -> Expression {
        let mut expr = self.parse_logical_and();

        while let Some(tok) = self.peek_token() {
            if let Token::Operator(op) = &tok.token
                && op == "||"
            {
                self.next_token();
                let rhs = self.parse_logical_and();
                expr = Expression::BinaryOp {
                    op: "||".to_string(),
                    left: Box::new(expr.clone()),
                    right: Box::new(rhs.clone()),
                    span: expr.span().join(rhs.span()),
                };
                continue;
            }
            break;
        }

        expr
    }

    // New: logical AND (&&)
    fn parse_logical_and(&mut self) -> Expression {
        let mut expr = self.parse_comparison();

        while let Some(tok) = self.peek_token() {
            if let Token::Operator(op) = &tok.token
                && op == "&&"
            {
                self.next_token();
                let rhs = self.parse_comparison();
                expr = Expression::BinaryOp {
                    op: "&&".to_string(),
                    left: Box::new(expr.clone()),
                    right: Box::new(rhs.clone()),
                    span: expr.span().join(rhs.span()),
                };
                continue;
            }
            break;
        }

        expr
    }

    // New: comparisons (==, !=, <, >, <=, >=)
    fn parse_comparison(&mut self) -> Expression {
        let mut expr = self.parse_additive();

        while let Some(tok) = self.peek_token() {
            if let Token::Operator(op) = &tok.token {
                match op.as_str() {
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        let op = op.clone();
                        self.next_token();
                        let rhs = self.parse_additive();
                        expr = Expression::BinaryOp {
                            op,
                            left: Box::new(expr.clone()),
                            right: Box::new(rhs.clone()),
                            span: expr.span().join(rhs.span()),
                        };
                        continue;
                    }
                    _ => {}
                }
            }
            break;
        }

        expr
    }

    fn parse_additive(&mut self) -> Expression {
        let mut expr = self.parse_term();

        while let Some(tok) = self.peek_token() {
            if let Token::Operator(op) = &tok.token
                && (op == "+" || op == "-")
            {
                let op = op.clone();
                self.next_token();
                let rhs = self.parse_term();
                expr = Expression::BinaryOp {
                    op,
                    left: Box::new(expr.clone()),
                    right: Box::new(rhs.clone()),
                    span: expr.span().join(rhs.span()),
                };
                continue;
            }
            break;
        }

        expr
    }

    fn parse_term(&mut self) -> Expression {
        let mut expr = self.parse_cast();

        while let Some(lexical_token) = self.peek_token() {
            if let Token::Operator(op) = &lexical_token.token {
                if op == "*" || op == "/" {
                    let op = op.clone();
                    self.next_token();
                    let rhs = self.parse_cast();
                    expr = Expression::BinaryOp {
                        op,
                        left: Box::new(expr.clone()),
                        right: Box::new(rhs.clone()),
                        span: expr.span().join(rhs.span()),
                    };
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        expr
    }

    // This method should have high precedence in the expression parsing
    fn parse_cast(&mut self) -> Expression {
        let mut expr = self.parse_unary();

        while let Some(lexical_token) = self.peek_token() {
            if let Token::Keyword(keyword) = &lexical_token.token {
                let lexical_token_span = lexical_token.span;
                if keyword == "as" {
                    self.next_token(); // consume 'as'
                    let token_span = if let Some(next_tok) = self.peek_token() {
                        next_tok.span
                    } else {
                        lexical_token_span
                    };
                    let target_type = self.parse_type();
                    expr = Expression::Cast {
                        expr: Box::new(expr.clone()),
                        target_type,
                        span: expr.span().join(token_span),
                    };
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        expr
    }

    // Handle unary operators with higher precedence than cast target binder
    fn parse_unary(&mut self) -> Expression {
        if let Some(lexical_token) = self.peek_token() {
            let lexical_token_span = lexical_token.span;
            if let Token::Operator(op) = &lexical_token.token
                && op == "!"
            {
                self.next_token();
                let operand = self.parse_unary();
                return Expression::UnaryOp {
                    op: "!".to_string(),
                    expr: Box::new(operand.clone()),
                    span: lexical_token_span.join(operand.span()),
                };
            }
        }
        // Fallback to postfix (which includes factors)
        self.parse_postfix()
    }

    // Parses a function definition
    fn parse_function(&mut self) -> Statement {
        let (name, _lexical_token_span) = if let Some(lexical_token) = self.next_token() {
            if let Token::Identifier(name) = &lexical_token.token {
                (name.clone(), lexical_token.span)
            } else {
                panic!(
                    "Expected function name, found {:?} at line {}, column {}",
                    lexical_token.token, lexical_token.line, lexical_token.column
                );
            }
        } else {
            panic!("Expected function name, but no more tokens available");
        };
        self.expect(&Token::Punctuation("(".to_string()));
        let params = self.parse_function_call_params();
        self.expect(&Token::Punctuation(")".to_string()));

        let mut ret_type: Type;
        if let Some(arrow_token) = self.peek_token() {
            if arrow_token.token == Token::Punctuation("->".to_string()) {
                self.next_token(); // consume '->'
                ret_type = self.parse_type();
            } else {
                ret_type = Type::Void;
            }
        } else {
            ret_type = Type::Void;
        }
        self.expect(&Token::Punctuation("{".to_string()));
        let body = self.parse();

        let total_span = {
            if let Some(peek_tok) = self.peek_token() {
                peek_tok.span.join(_lexical_token_span)
            } else {
                _lexical_token_span
            }
        };

        self.expect(&Token::Punctuation("}".to_string())); // consume the closing brace

        Statement::Function {
            name,
            ret_type,
            params,
            body,
            span: total_span,
        }
    }

    fn parse_constructor_destructor(&mut self, class_name: String) -> Statement {
        let mut is_constructor;
        let mut is_destructor;

        let (name, lexical_token_span) = if let Some(lexical_token) = self.next_token() {
            if let Token::Keyword(keyword) = &lexical_token.token {
                is_constructor = keyword == "init";
                is_destructor = keyword == "destroy";

                if is_constructor || is_destructor {
                    (format!("__{}_{}", class_name, keyword), lexical_token.span)
                } else {
                    panic!(
                        "Expected 'init' or 'destroy', found {:?} at line {}, column {}",
                        lexical_token.token, lexical_token.line, lexical_token.column
                    );
                }
            } else {
                panic!(
                    "Expected constructor/destructor name, found {:?} at line {}, column {}",
                    lexical_token.token, lexical_token.line, lexical_token.column
                );
            }
        } else {
            panic!("Expected constructor/destructor, but no more tokens available");
        };

        let mut params = Vec::new();

        // Check if the next token is a parenthesis, which means we have parameters
        if let Some(next_token) = self.peek_token()
            && is_constructor
            && next_token.token == Token::Punctuation("(".to_string())
        {
            self.next_token(); // consume '('
            params = self.parse_function_call_params();
            self.expect(&Token::Punctuation(")".to_string())); // consume ')'
        }

        self.expect(&Token::Punctuation("{".to_string()));
        let body = self.parse();
        let total_span = {
            if let Some(peek_tok) = self.peek_token() {
                peek_tok.span.join(lexical_token_span)
            } else {
                lexical_token_span
            }
        };
        self.expect(&Token::Punctuation("}".to_string())); // consume the closing brace

        Statement::Function {
            name,
            ret_type: Type::Void, // Constructors and destructors do not return a value
            params,
            body,
            span: total_span,
        }
    }

    fn parse_function_call_params(&mut self) -> Vec<Variable> {
        let mut params = Vec::new();
        while let Some(lexical_token) = self.peek_token() {
            if let Token::Identifier(param_name) = &lexical_token.token {
                let param_name = param_name.clone(); // Clone the name
                self.next_token(); // consume the identifier
                self.expect(&Token::Punctuation(":".to_string()));
                let param_type = self.parse_type();
                params.push(Variable {
                    name: param_name,
                    var_type: param_type,
                });
                if let Some(next_token) = self.peek_token() {
                    if next_token.token == Token::Punctuation(",".to_string()) {
                        self.next_token(); // consume ','
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        params
    }
}
