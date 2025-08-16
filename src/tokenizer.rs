use std::fmt::Display;

pub const PUNCTUATION: [&str; 11] = ["(", ")", "{", "}", "[", "]", ";", ",", ".", ":", "->"];

pub const OPERATORS: [&str; 12] = [
    "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=",
];

pub const KEYWORDS: [&str; 19] = [
    "if", "else", "while", "for", "function", "return", "as", 
    "object", "class", "entity", "component", "system", 
    "static", "dynamic", 
    "public", "private", "protected",
    "init", "destroy"
];

#[derive(Debug)]
pub struct LexicalToken {
    pub token: Token,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Keyword(String),
    Identifier(String),
    Number(i64),
    StringLiteral(String),
    Operator(String),
    Punctuation(String),
    Newline,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = match self {
            Token::Keyword(k) => k.clone(),
            Token::Identifier(s) => s.clone(),
            Token::Number(n) => n.to_string(),
            Token::StringLiteral(s) => format!("\"{}\"", s),
            Token::Operator(op) => op.clone(),
            Token::Punctuation(p) => p.clone(),
            Token::Newline => "\n".to_string(),
        };
        write!(f, "{}", fmtstr)
    }
}

pub fn conv(token: String) -> Token {
    if token.starts_with('"') && token.ends_with('"') {
        Token::StringLiteral(token[1..token.len() - 1].to_string())
    } else if let Ok(num) = token.parse::<i64>() {
        Token::Number(num)
    } else if OPERATORS.contains(&token.as_str()) {
        Token::Operator(token)
    } else if PUNCTUATION.contains(&token.as_str()) {
        Token::Punctuation(token)
    } else if KEYWORDS.contains(&token.as_str()) {
        Token::Keyword(token)
    } else {
        Token::Identifier(token)
    }
}

pub fn emit(token: &mut String, tokens: &mut Vec<LexicalToken>, line: usize, column: usize) {
    if !token.is_empty() {
        tokens.push(LexicalToken {
            token: conv(token.clone()),
            line,
            column,
        });
        token.clear();
    }
}

pub fn tokenize_line(line_string: &str, line: usize) -> Vec<LexicalToken> {
    // Step 1: Read character by character
    let mut tokens = Vec::<LexicalToken>::new();

    let mut inside_string = false;

    let mut matched_slash = false;

    let mut token = String::new();

    let mut column = 0;
    for ch in line_string.chars() {
        column += 1;

        ///////////////////
        // String literals
        ///////////////////
        if ch == '"' {
            inside_string = !inside_string;
            if !inside_string {
                // If we are closing a string, end it and push the token
                token.push(ch);
                emit(&mut token, &mut tokens, line, column);
            }
        }

        if inside_string {
            // If we are inside a string, just add the character to the token
            token.push(ch);
            continue;
        }

        match ch {
            '"' => {
                // Handled elsewhere
                continue;
            }
            //////////////
            // Whitespace
            //////////////
            ' ' | '\t' | '\n' => {
                matched_slash = false;

                // Whitespace forces a token to be emitted
                // Emit the current token if it's not empty
                emit(&mut token, &mut tokens, line, column);
            }

            /////////////////////////
            // Comments and division
            /////////////////////////
            '/' => {
                if matched_slash {
                    // If we have two slashes, it is a comment
                    // so we skip the rest of the line
                    token.clear();
                    break;
                } else {
                    matched_slash = true;
                    token.push(ch);
                }
            }
            _ => {
                matched_slash = false;

                /////////////////////////////
                // Operators and punctuation
                /////////////////////////////

                if OPERATORS.contains(&ch.to_string().as_str())
                    || PUNCTUATION.contains(&ch.to_string().as_str())
                {
                    /////////////////////////////////////////////
                    // Single-character operators or punctuation
                    /////////////////////////////////////////////

                    // If it is, emit the current token and push the operator or punctuation
                    emit(&mut token, &mut tokens, line, column);
                    tokens.push(LexicalToken {
                        token: conv(String::from(ch)),
                        line,
                        column,
                    });

                    ////////////////////////////////////////////
                    // Multi-character operators or punctuation
                    ////////////////////////////////////////////

                    // Combine last two tokens if they are part of a multi-character operator or punctuation
                    if tokens.len() >= 2 {
                        let last_last = &tokens[tokens.len() - 2];
                        let last = tokens.last().unwrap();

                        let last_str =
                            format!("{}{}", last_last.token, last.token);

                        if OPERATORS.contains(&last_str.as_str())
                            || PUNCTUATION.contains(&last_str.as_str())
                        {
                            tokens.pop(); // Remove the last token
                            tokens.pop(); // Remove the second last token
                            tokens.push(LexicalToken {
                                token: conv(last_str),
                                line,
                                column,
                            }); // Push the combined token
                        }
                    }

                    continue;
                }

                ////////////////////////////
                // Identifiers and numbers
                ///////////////////////////

                // If we reach here, it means we are still building an identifier/number token
                token.push(ch);
            }
        }
    }

    // Emit the last token if it's not empty
    if !token.is_empty() {
        tokens.push(LexicalToken {
            token: conv(token.clone()),
            line,
            column,
        });
    }

    // Step 3: Handle newlines
    tokens.push(LexicalToken {
        token: Token::Newline,
        line,
        column,
    });

    tokens
}

pub fn tokenize(source: &str) -> Vec<LexicalToken> {
    // Step 1: Split lines
    let lines: Vec<&str> = source.lines().collect();

    // Step 2: Tokenize each line
    

    lines
        .iter()
        .enumerate()
        .flat_map(|(line, line_str)| tokenize_line(line_str, line))
        .collect()
}
