pub mod rust;
pub mod transpiler;
pub mod ts;

use pretty::{Arena, DocBuilder};

pub use rust::RustTranspiler;
pub use transpiler::Transpiler;
pub use ts::TsTranspiler;

pub type Doc<'a> = DocBuilder<'a, Arena<'a>>;
