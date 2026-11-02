use super::rust::RustTranspiler;
use super::transpiler::Transpiler;
use super::ts::TsTranspiler;
use crate::config::TargetLanguage;
use crate::ir::writer_program::WriterProgram;

pub fn transpile(target: TargetLanguage, program: &WriterProgram) -> String {
    match target {
        TargetLanguage::Typescript => TsTranspiler::new().transpile_program(program),
        TargetLanguage::Rust => RustTranspiler::new().transpile_program(program),
    }
}
