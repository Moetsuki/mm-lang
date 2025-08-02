use std::sync::atomic::{AtomicU64, Ordering};

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
}
