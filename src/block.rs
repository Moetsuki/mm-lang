use std::sync::atomic::{AtomicU64, Ordering};
use std::fmt::Display;

use crate::span::Span;
use crate::statement::Statement;

static BLOCK_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone)]
pub struct Block {
    pub id: u64,
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn new(statements: Vec<Statement>) -> Self {
        let id = BLOCK_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        Block { id, statements }
    }
    pub fn span(&self) -> Option<Span> {
        if self.statements.is_empty() {
            None
        } else {
            let first_span = self.statements.first().unwrap().span();
            let last_span = self.statements.last().unwrap().span();
            Some(first_span.join(last_span))
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmtstr = format!(
            "Block(id: {}, statements: [{}])",
            self.id,
            self.statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );
        write!(f, "{}", fmtstr)
    }
}