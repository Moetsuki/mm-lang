use std::fmt::Display;

use crate::{file::SourceFile, span::Span};

pub const PUNCTUATION: [&str; 11] = ["(", ")", "{", "}", "[", "]", ";", ",", ".", ":", "->"];

pub const OPERATORS: [&str; 12] = [
    "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=",
];

pub const KEYWORDS: [&str; 20] = [
    "if",
    "else",
    "while",
    "for",
    "function",
    "return",
    "as",
    "object",
    "class",
    "entity",
    "component",
    "system",
    "static",
    "dynamic",
    "public",
    "private",
    "protected",
    "init",
    "destroy",
    "tensor",
];

#[derive(Debug, Clone)]
pub struct LexicalToken {
    pub token: Token,
    pub line: usize,
    pub column: usize,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
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

pub fn emit(
    token: &mut String,
    tokens: &mut Vec<LexicalToken>,
    line: usize,
    column: usize,
    span: Span,
) {
    if !token.is_empty() {
        tokens.push(LexicalToken {
            token: conv(token.clone()),
            line,
            column,
            span,
        });
        token.clear();
    }
}

pub fn tokenize_line(
    line_string: &str,
    line: usize,
    source_file: &SourceFile,
    cursor: &mut usize,
) -> Vec<LexicalToken> {
    // Step 1: Read character by character
    let mut tokens = Vec::<LexicalToken>::new();

    let mut inside_string = false;

    let mut matched_slash = false;

    let mut token = String::new();

    // Track the absolute byte offset where the current token started
    let mut token_start: Option<usize> = None;

    // Track where this line starts in the global cursor
    let line_start_cursor = *cursor;

    let mut column = 0;
    for ch in line_string.chars() {
        column += 1;
        *cursor += 1;

        // When starting a new token, remember its starting byte (cursor already advanced by ch)
        if !inside_string && token.is_empty() {
            token_start = Some(*cursor - ch.len_utf8());
        }

        ///////////////////
        // String literals
        ///////////////////
        if ch == '"' {
            if !inside_string && token.is_empty() {
                // opening quote is the first char of this token
                token_start = Some(*cursor - ch.len_utf8());
            }

            inside_string = !inside_string;
            if !inside_string {
                // If we are closing a string, end it and push the token
                token.push(ch);
                let start = token_start.take().unwrap();
                let end = start + token.len();
                emit(
                    &mut token,
                    &mut tokens,
                    line,
                    column,
                    Span::new(
                        source_file.id,
                        start,
                        end,
                    ),
                );
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
                let start = token_start.take().unwrap();
                let end = start + token.len();
                emit(&mut token, &mut tokens, line, column, Span::new(
                    source_file.id,
                    start,
                    end,
                ));
            }

            /////////////////////////
            // Comments and division
            /////////////////////////
            '/' => {
                if matched_slash {
                    // If we have two slashes, it is a comment
                    // so we skip the rest of the line
                    token.clear();
                    token_start = None;

                    // Advance cursor and column to the end of the kine so spans stay correct
                    let remaining = *cursor - line_start_cursor - 1;
                    *cursor += remaining;
                    column = line_string.chars().count();

                    break;
                } else {
                    matched_slash = true;
                    if token.is_empty() {
                        token_start = Some(*cursor - ch.len_utf8());
                    }
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

                    // First, emit any pending identifier/number token
                    let start = token_start.take().unwrap();
                    let end = start + token.len();
                    emit(&mut token, &mut tokens, line, column, Span::new(
                        source_file.id,
                        start,
                        end,
                    ));
                    
                    // Then emit the single-character operator/punctuation itself
                    let op_start = *cursor - ch.len_utf8();
                    tokens.push(LexicalToken {
                        token: conv(String::from(ch)),
                        line,
                        column,
                        span: Span::new(
                            source_file.id,
                            op_start,
                            *cursor,
                        ),
                    });

                    ////////////////////////////////////////////
                    // Multi-character operators or punctuation
                    ////////////////////////////////////////////

                    // Combine last two tokens if they are part of a multi-character operator or punctuation
                    if tokens.len() >= 2 {
                        let last_last = &tokens[tokens.len() - 2];
                        let last = tokens.last().unwrap();
                        let last_str = format!("{}{}", last_last.token, last.token);

                        if OPERATORS.contains(&last_str.as_str())
                            || PUNCTUATION.contains(&last_str.as_str())
                        {
                            let merged_start = last_last.span.lo as usize;
                            tokens.pop(); // Remove the last token
                            tokens.pop(); // Remove the second last token
                            tokens.push(LexicalToken {
                                token: conv(last_str.clone()),
                                line,
                                column,
                                span: Span::new(
                                    source_file.id,
                                    merged_start,
                                    merged_start + last_str.len(),
                                )
                            }); // Push the combined token
                        }
                    }

                    continue;
                }

                ////////////////////////////
                // Identifiers and numbers
                ///////////////////////////

                // If we reach here, it means we are still building an identifier/number token
                if token.is_empty() {
                    token_start = Some(*cursor - ch.len_utf8());
                }
                token.push(ch);
            }
        }
    }

    // Emit the last token if it's not empty
    if !token.is_empty() {
        let start = token_start.take().unwrap_or((*cursor).saturating_sub(token.len()));
        let end = start + token.len();
        tokens.push(LexicalToken {
            token: conv(token.clone()),
            line,
            column,
            span: Span::new(
                source_file.id,
                start,
                end,
            ),
        });
    }

    // Step 3: Handle newlines
    tokens.push(LexicalToken {
        token: Token::Newline,
        line,
        column,
        span: Span::new(
            source_file.id,
            *cursor,
            *cursor + 1,
        )
    });
    *cursor += 1;

    tokens
}

pub fn tokenize(source: &str, source_file: &SourceFile) -> Vec<LexicalToken> {
    // Step 1: Split lines
    let lines: Vec<&str> = source.lines().collect();

    let mut cursor: usize = 0;

    // Step 2: Tokenize each line
    lines
        .iter()
        .enumerate()
        .flat_map(|(line, line_str)| tokenize_line(line_str, line, source_file, &mut cursor))
        .collect()
}
