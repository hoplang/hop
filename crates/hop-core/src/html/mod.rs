mod attributes;
mod element_kind;
mod escape;

pub use element_kind::{HtmlElementKind, is_raw_content_tag, is_void_element_tag};
pub use escape::write_escaped_html;
