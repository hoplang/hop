mod attributes;
mod element;
mod escape;

pub use element::{HtmlElement, is_void_element};
pub use escape::write_escaped_html;
