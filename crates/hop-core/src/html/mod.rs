mod attributes;
mod element;
mod escape;

pub use element::{HtmlElement, is_raw_content_tag, is_void_element_tag};
pub use escape::write_escaped_html;
