use crate::block::Block;
use crate::expression::Expression;
use crate::statement::Statement;
use crate::statement::Visibility;
use crate::tokenizer::LexicalToken;
use crate::tokenizer::OPERATORS;
use crate::tokenizer::Token;
use crate::types::Type;
use crate::variable::Variable;
use std::str::FromStr;

pub struct Ast {
    tokens: Vec<LexicalToken>,
    pos: usize,
    tree: Option<Block>,
}

impl Ast {
    pub fn new(tokens: Vec<LexicalToken>) -> Self {
        Ast {
            tokens,
            pos: 0,
            tree: None,
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

    pub fn expect(&mut self, expected: &Token) {
        if let Some(lexical_token) = self.next_token() {
            if &lexical_token.token != expected {
                panic!(
                    "Expected {:?}, found {:?} at line {}, column {}",
                    expected, lexical_token.token, lexical_token.line, lexical_token.column
                );
            }
        } else {
            panic!("Expected {:?}, but no more tokens available", expected);
        }
    }

    pub fn expect_operator(&mut self) {
        if let Some(lexical_token) = self.next_token() {
            if !OPERATORS.contains(&lexical_token.token.to_string().as_str()) {
                panic!(
                    "Expected an operator, found {:?} at line {}, column {}",
                    lexical_token.token, lexical_token.line, lexical_token.column
                );
            }
        }
    }

    pub fn parse(&mut self) -> Block {
        let mut statements = Vec::new();

        while let Some(lexical_token) = self.next_token() {
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
                                    var_type: Type::ToBeEvaluated, // Default type, will be set later
                                };

                                // Get next token and make sure its an identifier and a valid type
                                self.next_token();
                                if let Some(type_token) = self.peek_token() {
                                    match &type_token.token {
                                        Token::Identifier(type_name) => {
                                            var_info.var_type = Type::from_str(type_name)
                                                .unwrap_or(Type::ToBeEvaluated);
                                            self.next_token(); // consume the type identifiers
                                            self.expect_operator(); // consume the = sign
                                            let expr = self.parse_expr();
                                            statements.push(Statement::VariableDecl {
                                                identifier: var_info,
                                                value: expr,
                                            });
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
                                    identifier: Expression::Variable(Variable {
                                        name: name_clone,
                                        var_type: Type::ToBeEvaluated,
                                    }),
                                    value: expr,
                                });
                            }
                            //
                            // Function call case:
                            //
                            Token::Punctuation(p) if p == "(" => {
                                // Handle function call case
                                let callee = Expression::Variable(Variable {
                                    name: name_clone,
                                    var_type: Type::ToBeEvaluated,
                                });
                                let call_statement = self.parse_call(callee);
                                statements.push(call_statement);
                            }
                            //
                            // Method call case:
                            //
                            Token::Punctuation(p) if p == "." => {
                                // Backtrack to the identifier token
                                self.backtrack();

                                // Parse expression
                                let expr = self.parse_expr();

                                // If its a BinaryOp =, we emit an Assignment statement
                                if let Expression::BinaryOp { op, left, right } = expr {
                                    if op == "=" {
                                        statements.push(Statement::Assignment {
                                            identifier: *left,
                                            value: *right,
                                        });
                                    } else {
                                        panic!("Unexpected binary operator `{}` in method call context", op);
                                    }
                                } else {
                                    panic!("Expected a BinaryOp for method call, found {:?}", expr);
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
                    self.expect(&Token::Punctuation("{".to_string()));
                    let then_block = self.parse();
                    self.expect(&Token::Punctuation("}".to_string())); // consume the closing brace
                    let else_block = if let Some(lexical_token) = self.peek_token() {
                        if lexical_token.token == Token::Keyword("else".to_string()) {
                            self.next_token(); // consume 'else'
                            self.expect(&Token::Punctuation("{".to_string()));
                            let else_block = self.parse();
                            self.expect(&Token::Punctuation("}".to_string())); // consume the closing brace
                            Some(else_block)
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    statements.push(Statement::If {
                        condition,
                        then_block,
                        else_block,
                    });
                }
                Token::Keyword(keyword) if keyword == "function" => {
                    let stm = self.parse_function();
                    statements.push(stm);
                }
                Token::Keyword(keyword) if keyword == "return" => {
                    //
                    // Handle return statements
                    //
                    let expr = self.parse_expr();
                    statements.push(Statement::Return { value: expr });
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

                    self.expect(&Token::Punctuation("}".to_string())); // consume the closing brace

                    statements.push(Statement::Class {
                        name: class_name,
                        parent: parent_class,
                        fields,
                        methods,
                    });

                    self.expect(&Token::Punctuation(";".to_string())); // consume the semicolon
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
                _ => {
                    //
                    // Handle other tokens
                    //
                    println!(
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
                Token::Identifier(type_name) => {
                    Type::from_str(type_name).unwrap_or_else(|_| Type::UserType(type_name.clone()))
                }
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
        self.expect(&Token::Punctuation(")".to_string()));
        Statement::Call { callee, args }
    }

    // Parses a list of arguments for a function call
    fn parse_args(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();
        while let Some(lexical_token) = self.peek_token() {
            match &lexical_token.token {
                Token::Identifier(_) | Token::Number(_) | Token::StringLiteral(_) => {
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
            match &lexical_token.token {
                Token::Number(n) => {
                    let value = *n;
                    self.next_token();
                    Expression::Number(value)
                }
                Token::Operator(p) if p == "-" => {
                    self.next_token(); // consume the unary minus operator
                    let next_tok = self.peek_token();
                    if let Some(next_tok) = next_tok {
                        match &next_tok.token {
                            Token::Number(n) => {
                                let result = Expression::Number(-(*n));
                                self.next_token(); // consume the number
                                result
                            }
                            _ => {
                                let operand = self.parse_factor();
                                Expression::UnaryOp {
                                    op: "-".to_string(),
                                    expr: Box::new(operand),
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
                    Expression::StringLiteral(value)
                }
                Token::Identifier(name) => {
                    let name_clone = name.clone();
                    self.next_token();
                    Expression::Variable(Variable {
                        name: name_clone,
                        var_type: Type::ToBeEvaluated,
                    })
                }
                Token::Punctuation(p) if p == "(" => {
                    self.next_token();
                    let expr = self.parse_expr();
                    self.expect(&Token::Punctuation(")".to_string()));
                    expr
                }
                other => panic!("Unexpected token: {:?}", other),
            }
        } else {
            panic!("Unexpected end of input while parsing factor");
        }
    }

    // Entry point for expressions (lowest precedence)
    fn parse_expr(&mut self) -> Expression {
        let mut expr = self.parse_term();

        while let Some(lexical_token) = self.peek_token() {
            match &lexical_token.token {
                Token::Operator(op) => {
                    if OPERATORS.contains(&op.as_str()) {
                        let op = op.clone();
                        self.next_token();
                        let rhs = self.parse_term();
                        expr = Expression::BinaryOp {
                            op,
                            left: Box::new(expr),
                            right: Box::new(rhs),
                        };
                    } else {
                        break;
                    }
                }
                Token::Newline => {
                    // Ignore newlines and continue
                    self.next_token();
                }
                Token::Punctuation(p) => {
                    // End of expression
                    match p.as_str() {
                        ")" | "}" | "{" | "," => {
                            break; // Do nothing, let caller handle these
                        }
                        ";" => {
                            self.next_token(); // Consume the semicolon
                            break; // Return expression to caller
                        }
                        "." => {
                            self.next_token(); // Consume the '.'

                            // After the dot we expect an identifier
                            if let Some(next_token) = self.next_token() {
                                if let Token::Identifier(method_name) = &next_token.token {
                                    expr = Expression::Method {
                                        object: Box::new(expr),
                                        method: Box::new(Expression::Variable(Variable {
                                            name: method_name.clone(),
                                            var_type: Type::ToBeEvaluated,
                                        })),
                                    };
                                } else {
                                    panic!(
                                        "Expected method name after '.', found {:?} at line {}, column {}",
                                        next_token.token, next_token.line, next_token.column
                                    );
                                }
                            } else {
                                panic!("Expected method name after '.', but no more tokens available");
                            }
                            
                            // Do not give control to the caller by breaking, continue parsing
                        }
                        "(" => {
                            self.next_token(); // Consume the '('

                            // Expr should be a function identifier
                            if let Expression::Variable(var) = expr {
                                let callee = Expression::Variable(var);
                                let args = self.parse_args();
                                expr = Expression::Call {
                                    callee: Box::new(callee),
                                    args,
                                };
                            } else if let Expression::Method { object, method } = expr {
                                expr = Expression::Call {
                                    callee: Box::new(Expression::Method {
                                        object,
                                        method,
                                    }),
                                    args: self.parse_args(),
                                };
                            } else {
                                panic!("Expected a variable before '(', found {:?}", expr);
                            }

                            // Expect the closing parenthesis
                            self.expect(&Token::Punctuation(")".to_string()));

                            // Do not give control to the caller by breaking, continue parsing
                        }
                        _ => {
                            break; // Let caller handle unexpected punctuation
                        }
                    }
                } 
                _ => {
                    break; // End of expression for any other token type
                }
            }
        }

        expr
    }

    // This method should have high precedence in the expression parsing
    fn parse_cast(&mut self) -> Expression {
        let mut expr = self.parse_factor();

        while let Some(lexical_token) = self.peek_token() {
            if let Token::Keyword(keyword) = &lexical_token.token {
                if keyword == "as" {
                    self.next_token(); // consume 'as'
                    let target_type = self.parse_type();
                    expr = Expression::Cast {
                        expr: Box::new(expr),
                        target_type,
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

    // Then modify parse_term to call parse_cast instead of parse_factor
    fn parse_term(&mut self) -> Expression {
        let mut expr = self.parse_cast(); // Changed from parse_factor

        while let Some(lexical_token) = self.peek_token() {
            if let Token::Operator(op) = &lexical_token.token {
                if op == "*" || op == "/" {
                    let op = op.clone();
                    self.next_token();
                    let rhs = self.parse_cast(); // Changed from parse_factor
                    expr = Expression::BinaryOp {
                        op,
                        left: Box::new(expr),
                        right: Box::new(rhs),
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

    // Parses a function definition
    fn parse_function(&mut self) -> Statement {
        let name = if let Some(lexical_token) = self.next_token() {
            if let Token::Identifier(name) = &lexical_token.token {
                name.clone()
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
        let mut params = Vec::new();
        loop {
            if let Some(lexical_token) = self.peek_token() {
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
            } else {
                break;
            }
        }
        self.expect(&Token::Punctuation(")".to_string()));

        let mut ret_type: Type;
        if let Some(arrow_token) = self.peek_token() {
            if arrow_token.token == Token::Punctuation("->".to_string()) {
                self.next_token(); // consume '->'
                ret_type = self.parse_type();
            } else {
                ret_type = Type::NoneType;
            }
        } else {
            ret_type = Type::NoneType;
        }
        self.expect(&Token::Punctuation("{".to_string()));
        let body = self.parse();
        self.expect(&Token::Punctuation("}".to_string())); // consume the closing brace

        Statement::Function {
            name,
            ret_type,
            params,
            body,
        }
    }
}
