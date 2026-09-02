mod attributes;
mod element;
mod escape;

pub use element::{HtmlElement, has_raw_content, is_void_element};
pub use escape::write_escaped_html;
