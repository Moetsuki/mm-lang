//! Backend abstraction for code generation targets.

use std::fmt::Display;

use crate::ast::Ast;
use crate::file::SourceFile;

/// Minimal backend interface used by the driver.
pub trait Backend<'a> {
    /// Lower the parsed AST into target code.
    fn compile(&mut self);

    /// Get the produced textual output for this backend.
    fn output(&self) -> String;

    /// Construct a backend instance from AST and source file.
    fn from_ast(ast: Ast<'a>, source: &'a SourceFile) -> Self
    where
        Self: Sized;
}

/// Available targets.
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetKind {
    LLVM,
    C
}

impl Display for TargetKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TargetKind::LLVM => write!(f, "llvm"),
            TargetKind::C => write!(f, "c"),
        }
    }
}