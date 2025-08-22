use crate::span::Span;

use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u64);

static FILE_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug)]
pub struct SourceFile {
    pub id: FileId,
    pub name: String,
    pub src: String,
    // Byte offsets for the start of each line, 0-based, always starts with 0.
    // No sentinel for EOF is stored; we derive end positions from src.len() or next line start.
    line_starts: Vec<usize>,
}

impl SourceFile {
    // Build a SourceFile and compute line starts by scanning for '\n' bytes.
    // This treats '\n' as a line separator; Windows "\r\n" will leave '\r' at line ends.
    // Positions are byte offsets; multi-byte UTF-8 characters advance by multiple bytes.
    pub fn new(name: impl Into<String>, src: String) -> Self {
        let id = FileId(FILE_ID_COUNTER.fetch_add(1, Ordering::SeqCst));
        let mut line_starts = vec![0];
        // Enumerate every byte and record the offset after each newline as the start of the next line
        let bytes = src.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            match bytes[i] {
                b'\n' => {
                    i += 1;                 // LF
                    line_starts.push(i);
                }
                b'\r' => {
                    if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                        i += 2;             // CRLF
                    } else {
                        i += 1;             // lone CR
                    }
                    line_starts.push(i); 
                }
                _ => i += 1,
            }
        }
        SourceFile { id, name: name.into(), src, line_starts }
    }
    // Convert a byte offset into 1-based (line, column), where column counts bytes since line start.
    // Uses binary search over line_starts to find the greatest line start <= byte.
    // Assumes `byte` is within 0..=src.len(); passing out-of-range may panic elsewhere.
    pub fn line_col(&self, byte: usize) -> (usize, usize) {
        let idx = match self.line_starts.binary_search(&byte) {
            // Exact match means byte is at a line start; idx is that line's index
            Ok(i) => i,
            // Otherwise, use the previous line start (saturates to 0 if before the first line)
            Err(i) => i.saturating_sub(1),
        };
        let line_start = self.line_starts[idx];
        // Return 1-based line and column for human-friendly diagnostics
        (idx + 1, byte - line_start + 1) // 1-based
    }
    // Return the full text of the line that contains span.lo, without the trailing '\n'.
    // If span.lo is at EOF, returns the last line. Works even for empty spans.
    pub fn snippet(&self, span: Span) -> String {
        let (line, _) = self.line_col(span.lo as usize);
        // Compute start of the containing line from precomputed table
        let start = self.line_starts[line - 1];
        // Compute end as the byte before the next line's start (to strip '\n'), or EOF if last line
        let end = if line < self.line_starts.len() {
            self.line_starts[line] - 1
        } else {
            self.src.len()
        };
        self.src[start..end].to_string()
    }
    // Produce a simple caret diagnostic pointing at span:
    // "name:line:col:\n<line text>\n<spaces><^^^^>"
    // The number of carets equals span length, but is clamped to the remaining line width.
    // TODO: spacing is in bytes; tabs and multi-byte glyphs may misalign visually in monospace.
    pub fn caret(&self, span: Span) -> String {
        let (line, col) = self.line_col(span.lo as usize);
        let snippet = self.snippet(span);
        // Ensure at least one caret for empty spans; measure length in bytes
        let len = (span.hi - span.lo).max(1) as usize;
        let res = format!(
            "{}:{}:{}:\n{}\n{}{}",
            self.name,
            line,
            col,
            snippet,
            // Indent by (col - 1) spaces so the first caret aligns under the target byte
            " ".repeat(col - 1),
            // Draw carets, but don't extend past the end of the snippet
            "^".repeat(len.min(snippet.len().saturating_sub(col - 1)))
        );
        println!("{}", res);
        res
    }
    
    pub fn hash(&self) -> u64 {
        let mut hash : u64 = 0xdeadbeef;
        for byte in self.src.bytes() {
            hash = hash.wrapping_mul(0x4F6E9C5).wrapping_add(byte as u64);
            hash ^= hash >> 23;
        }
        hash.wrapping_mul(0x85EBCA77)
    }
}
