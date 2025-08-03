# Tokenizer Module

[← Back to README](../README.md)

## Overview

The tokenizer module (`tokenizer.rs`) handles lexical analysis, converting raw source code into a stream of tokens that can be processed by the parser. This is the first stage of the compilation pipeline.

## Key Components

### Token Types

```rust
pub enum Token {
    Keyword(String),        // if, else, function, return, etc.
    Identifier(String),     // variable names, function names
    Number(i64),           // integer literals
    StringLiteral(String), // string literals in quotes
    Operator(String),      // +, -, *, /, ==, !=, etc.
    Punctuation(String),   // (, ), {, }, ;, :, etc.
    Newline,              // line breaks
}
```

### Constants

- **KEYWORDS**: `["if", "else", "while", "for", "function", "return"]`
- **OPERATORS**: `["+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">="]`
- **PUNCTUATION**: `["(", ")", "{", "}", "[", "]", ";", ",", ".", ":", "->"]`

### LexicalToken Structure

```rust
pub struct LexicalToken {
    pub token: Token,
    pub line: usize,
    pub column: usize,
}
```

Each token carries position information for error reporting and debugging.

## Functions

### `tokenize(source: &str) -> Vec<LexicalToken>`

Main entry point that tokenizes an entire source string.

**Process:**
1. Splits source into lines
2. Tokenizes each line individually
3. Flattens results into a single token stream

### `tokenize_line(line_string: &str, line: usize) -> Vec<LexicalToken>`

Tokenizes a single line of source code.

**Features:**
- **String literal handling**: Properly handles quoted strings with escaping
- **Comment support**: Ignores everything after `//` 
- **Multi-character operators**: Recognizes operators like `==`, `!=`, `<=`, `>=`
- **Whitespace handling**: Uses whitespace as token delimiters
- **Position tracking**: Records line and column numbers

### `conv(token: String) -> Token`

Converts a raw string into the appropriate token type.

**Classification logic:**
1. String literals (starts and ends with `"`)
2. Numbers (parseable as `i64`)
3. Operators (matches OPERATORS array)
4. Punctuation (matches PUNCTUATION array)
5. Keywords (matches KEYWORDS array)
6. Identifiers (everything else)

### `emit(token: &mut String, tokens: &mut Vec<LexicalToken>, line: usize, column: usize)`

Helper function that converts accumulated characters into a token and adds it to the token stream.

## Tokenization Process

### 1. Character-by-Character Processing

The tokenizer reads each character and maintains state:
- `inside_string`: Tracks if currently inside a string literal
- `matched_slash`: Tracks potential comment starts
- `token`: Accumulates characters for the current token

### 2. String Literal Handling

```rust
if ch == '"' {
    inside_string = !inside_string;
    if !inside_string {
        token.push(ch);
        emit(&mut token, &mut tokens, line, column);
    }
}
```

String literals are processed specially to preserve spaces and special characters.

### 3. Comment Processing

```rust
'/' => {
    if matched_slash {
        // Double slash = comment, skip rest of line
        token.clear();
        break;
    } else {
        matched_slash = true;
        token.push(ch);
    }
}
```

Comments start with `//` and continue to end of line.

### 4. Operator and Punctuation Recognition

Single-character operators and punctuation are immediately tokenized. The system then attempts to combine consecutive operators into multi-character operators (e.g., `=` + `=` → `==`).

## Examples

### Input Source
```mm
function add(x: i64, y: i64) -> i64 {
    return x + y; // Simple addition
}
```

### Generated Tokens
```
Keyword("function")
Identifier("add")
Punctuation("(")
Identifier("x")
Punctuation(":")
Identifier("i64")
Punctuation(",")
Identifier("y")
Punctuation(":")
Identifier("i64")
Punctuation(")")
Punctuation("->")
Identifier("i64")
Punctuation("{")
Newline
Keyword("return")
Identifier("x")
Operator("+")
Identifier("y")
Punctuation(";")
Newline
Punctuation("}")
Newline
```

## Error Handling

The tokenizer is designed to be robust:
- Unrecognized characters become part of identifiers
- Unclosed strings are handled gracefully
- Position information is maintained for error reporting

## Usage in Compilation Pipeline

```rust
use tokenizer::tokenize;

let source = "x = 5 + 10;";
let tokens = tokenize(source);
// tokens is now ready for parsing
```

## Future Enhancements

- **Better error reporting**: More descriptive error messages
- **More operators**: Support for logical operators (`&&`, `||`)
- **Character literals**: Support for single-character literals
- **Float literals**: Support for floating-point number parsing
- **Unicode support**: Better handling of Unicode identifiers
- **Escape sequences**: Support for `\n`, `\t`, etc. in strings

## Testing

The tokenizer can be tested independently:

```rust
#[test]
fn test_basic_tokenization() {
    let source = "x = 42;";
    let tokens = tokenize(source);
    assert_eq!(tokens.len(), 5); // x, =, 42, ;, newline
}
```

## Performance Characteristics

- **Time Complexity**: O(n) where n is the length of source code
- **Space Complexity**: O(n) for storing tokens
- **Memory Usage**: Minimal temporary storage during tokenization

The tokenizer is designed for single-pass processing and maintains good performance even on large source files.
