use std::ops::Range;

use crate::file::FileId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// Half-open byte span into a specific file: [lo, hi), both measured in bytes
pub struct Span {
    // Which file this span refers to; ensures spans can be compared and joined safely
    pub file: FileId,
    // Inclusive start byte offset into file contents (UTF-8 bytes, not chars)
    pub lo: u64, // inclusive byte offset
    // Exclusive end byte offset; may be equal to lo for empty spans
    pub hi: u64, // exclusive byte offset
}

impl Span {
    // Create a span from usize offsets
    pub fn new(file: FileId, lo: usize, hi: usize) -> Self {
        Span { file, lo: lo as u64, hi: hi as u64 }
    }
    // Join two spans from the same file into their minimal covering span.
    // Panics if files differ to prevent accidental cross-file joins.
    pub fn join(self, other: Span) -> Span {
        assert_eq!(self.file, other.file);
        Span { file: self.file, lo: self.lo.min(other.lo), hi: self.hi.max(other.hi) }
    }
    // Expose the span as a standard Rust half-open byte Range for slicing strings
    pub fn range(self) -> Range<usize> { self.lo as usize..self.hi as usize }
    // True if the span covers no bytes (useful as a sentinel or caret position)
    pub fn is_empty(&self) -> bool { self.lo == self.hi }
}