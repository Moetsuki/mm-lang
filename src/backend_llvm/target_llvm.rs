use crate::ast::Ast;
use crate::backend::Backend;
use crate::file::SourceFile;
use crate::backend_llvm::llvm;

/// Thin wrapper that adapts the existing LLVM codegen to the Backend trait.
pub struct TargetLLVM {
    inner: llvm::LLVM,
}

impl TargetLLVM {
    pub fn new(ast: Ast, source: SourceFile) -> Self {
        Self {
            inner: llvm::LLVM::new(ast, source),
        }
    }
}

impl Backend for TargetLLVM {
    fn compile(&mut self) {
        self.inner.compile();
    }

    fn output(&self) -> String {
        self.inner.output()
    }

    fn from_ast(ast: Ast, source: SourceFile) -> Self
    where
        Self: Sized,
    {
        Self::new(ast, source)
    }
}
