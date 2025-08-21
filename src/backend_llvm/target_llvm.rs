use crate::ast::Ast;
use crate::backend::Backend;
use crate::file::SourceFile;
use crate::backend_llvm::llvm;

/// Thin wrapper that adapts the existing LLVM codegen to the Backend trait.
pub struct TargetLLVM<'a> {
    inner: llvm::LLVM<'a>,
}

impl<'a> TargetLLVM<'a> {
    pub fn new(ast: Ast<'a>, source: &'a SourceFile) -> Self {
        Self {
            inner: llvm::LLVM::new(ast, source),
        }
    }
}

impl<'a> Backend<'a> for TargetLLVM<'a> {
    fn compile(&mut self) {
        self.inner.compile();
    }

    fn output(&self) -> String {
        self.inner.output()
    }

    fn from_ast(ast: Ast<'a>, source: &'a SourceFile) -> Self
    where
        Self: Sized,
    {
        Self::new(ast, source)
    }
}
