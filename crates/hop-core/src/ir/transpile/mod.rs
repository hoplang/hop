mod core;
mod rust;
mod transpiler;
mod ts;

use pretty::{Arena, DocBuilder};

pub use core::transpile;

type Doc<'a> = DocBuilder<'a, Arena<'a>>;
