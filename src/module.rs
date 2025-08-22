use crate::SourceFile;
use crate::ast::Ast;
use crate::tokenizer::tokenize;
use crate::types::Type;

use std::fs;

pub struct ModuleInterface {
    pub module_path: String,
    pub source_file: SourceFile,
    pub exports: Vec<Export>,
    pub reexports: Vec<Reexport>,
    pub imports: Vec<Import>,
    pub hash: u64,
    pub ast: Ast,
}

pub struct Export {
    pub symbol_name: String,
    pub symbol_type: Type,
}

pub struct Import {
    pub symbol_path: String,
    pub symbol_alias: Option<String>,
}

pub struct Reexport {
    pub symbol_path: String,
    pub symbol_alias: String,
}

impl ModuleInterface {
    pub fn new_from_str(
        module_path: String,
        source_path: String,
        source: String,
    ) -> ModuleInterface {
        ModuleInterface::new_impl(module_path, source_path, source)
    }

    pub fn new(module_path: String, source_path: String) -> ModuleInterface {
        let source = fs::read_to_string(&source_path)
            .unwrap_or_else(|_| panic!("Unable to open file {}", source_path));

        ModuleInterface::new_impl(module_path, source_path, source)
    }

    fn new_impl(module_path: String, source_path: String, source: String) -> ModuleInterface {
        let sf = SourceFile::new(source_path, source.to_string());

        let hash = sf.hash();

        let mut tokens = tokenize(&source, &sf);

        let mut ast = Ast::new(tokens, sf.clone());

        let (exports, reexports, imports) = ModuleInterface::parse_ast(&ast);

        ModuleInterface {
            module_path,
            source_file: sf,
            exports,
            reexports,
            imports,
            hash,
            ast,
        }
    }

    fn parse_ast(_ast: &Ast) -> (Vec<Export>, Vec<Reexport>, Vec<Import>) {
        let mut exports = vec![];
        let mut reexports = vec![];
        let mut imports = vec![];

        (exports, reexports, imports)
    }
}
