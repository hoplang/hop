pub mod doctype_injector;
pub mod html_structure_injector;
pub mod link_rewriter;
pub mod meta_injector;
pub mod script_injector;
pub mod tailwind_injector;

pub use doctype_injector::DoctypeInjector;
pub use html_structure_injector::HtmlStructureInjector;
pub use link_rewriter::LinkRewriter;
pub use meta_injector::MetaInjector;
pub use script_injector::ScriptInjector;
pub use tailwind_injector::{TailwindInjection, TailwindInjector};
