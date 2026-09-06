use crate::document::CheapString;

/// Return true if the string represents a void element.
/// See https://developer.mozilla.org/en-US/docs/Glossary/Void_element
pub fn is_void_element_tag(tag_name: &str) -> bool {
    matches!(
        tag_name,
        "area"
            | "base"
            | "br"
            | "col"
            | "embed"
            | "hr"
            | "img"
            | "input"
            | "link"
            | "meta"
            | "source"
            | "track"
            | "wbr"
    )
}

/// Return true if the element's content is text rather than markup.
pub fn is_raw_content_tag(tag_name: &str) -> bool {
    matches!(tag_name, "script" | "style")
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HtmlElementKind {
    Html,
    Base,
    Head,
    Link,
    Meta,
    Style,
    Title,
    Body,
    Address,
    Article,
    Aside,
    Footer,
    Header,
    H1,
    H2,
    H3,
    H4,
    H5,
    H6,
    Hgroup,
    Main,
    Nav,
    Section,
    Search,
    Blockquote,
    Dd,
    Div,
    Dl,
    Dt,
    Figcaption,
    Figure,
    Hr,
    Li,
    Menu,
    Ol,
    P,
    Pre,
    Ul,
    A,
    Abbr,
    B,
    Bdi,
    Bdo,
    Br,
    Cite,
    Code,
    Data,
    Dfn,
    Em,
    I,
    Kbd,
    Mark,
    Q,
    Rp,
    Rt,
    Ruby,
    S,
    Samp,
    Small,
    Span,
    Strong,
    Sub,
    Sup,
    Time,
    U,
    Var,
    Wbr,
    Area,
    Audio,
    Img,
    Map,
    Track,
    Video,
    Embed,
    Iframe,
    Object,
    Picture,
    Source,
    Canvas,
    Noscript,
    Script,
    Del,
    Ins,
    Caption,
    Col,
    Colgroup,
    Table,
    Tbody,
    Td,
    Tfoot,
    Th,
    Thead,
    Tr,
    Button,
    Datalist,
    Fieldset,
    Form,
    Input,
    Label,
    Legend,
    Meter,
    Optgroup,
    Option,
    Output,
    Progress,
    Select,
    Textarea,
    Details,
    Dialog,
    Summary,
    Slot,
    Template,
    Svg,
    G,
    Path,
    Circle,
    Rect,
    Line,
    Polygon,
    Polyline,
    Ellipse,
    Defs,
    Use,
    Symbol,
    Desc,
    Marker,
    Mask,
    Pattern,
    Stop,
    Switch,
    Text,
    Tspan,
    Image,
    View,
    Animate,
    Set,
    Mpath,
    Metadata,
    Filter,
    Custom(CheapString),
}

impl HtmlElementKind {
    pub fn as_str(&self) -> &str {
        match self {
            HtmlElementKind::Html => "html",
            HtmlElementKind::Base => "base",
            HtmlElementKind::Head => "head",
            HtmlElementKind::Link => "link",
            HtmlElementKind::Meta => "meta",
            HtmlElementKind::Style => "style",
            HtmlElementKind::Title => "title",
            HtmlElementKind::Body => "body",
            HtmlElementKind::Address => "address",
            HtmlElementKind::Article => "article",
            HtmlElementKind::Aside => "aside",
            HtmlElementKind::Footer => "footer",
            HtmlElementKind::Header => "header",
            HtmlElementKind::H1 => "h1",
            HtmlElementKind::H2 => "h2",
            HtmlElementKind::H3 => "h3",
            HtmlElementKind::H4 => "h4",
            HtmlElementKind::H5 => "h5",
            HtmlElementKind::H6 => "h6",
            HtmlElementKind::Hgroup => "hgroup",
            HtmlElementKind::Main => "main",
            HtmlElementKind::Nav => "nav",
            HtmlElementKind::Section => "section",
            HtmlElementKind::Search => "search",
            HtmlElementKind::Blockquote => "blockquote",
            HtmlElementKind::Dd => "dd",
            HtmlElementKind::Div => "div",
            HtmlElementKind::Dl => "dl",
            HtmlElementKind::Dt => "dt",
            HtmlElementKind::Figcaption => "figcaption",
            HtmlElementKind::Figure => "figure",
            HtmlElementKind::Hr => "hr",
            HtmlElementKind::Li => "li",
            HtmlElementKind::Menu => "menu",
            HtmlElementKind::Ol => "ol",
            HtmlElementKind::P => "p",
            HtmlElementKind::Pre => "pre",
            HtmlElementKind::Ul => "ul",
            HtmlElementKind::A => "a",
            HtmlElementKind::Abbr => "abbr",
            HtmlElementKind::B => "b",
            HtmlElementKind::Bdi => "bdi",
            HtmlElementKind::Bdo => "bdo",
            HtmlElementKind::Br => "br",
            HtmlElementKind::Cite => "cite",
            HtmlElementKind::Code => "code",
            HtmlElementKind::Data => "data",
            HtmlElementKind::Dfn => "dfn",
            HtmlElementKind::Em => "em",
            HtmlElementKind::I => "i",
            HtmlElementKind::Kbd => "kbd",
            HtmlElementKind::Mark => "mark",
            HtmlElementKind::Q => "q",
            HtmlElementKind::Rp => "rp",
            HtmlElementKind::Rt => "rt",
            HtmlElementKind::Ruby => "ruby",
            HtmlElementKind::S => "s",
            HtmlElementKind::Samp => "samp",
            HtmlElementKind::Small => "small",
            HtmlElementKind::Span => "span",
            HtmlElementKind::Strong => "strong",
            HtmlElementKind::Sub => "sub",
            HtmlElementKind::Sup => "sup",
            HtmlElementKind::Time => "time",
            HtmlElementKind::U => "u",
            HtmlElementKind::Var => "var",
            HtmlElementKind::Wbr => "wbr",
            HtmlElementKind::Area => "area",
            HtmlElementKind::Audio => "audio",
            HtmlElementKind::Img => "img",
            HtmlElementKind::Map => "map",
            HtmlElementKind::Track => "track",
            HtmlElementKind::Video => "video",
            HtmlElementKind::Embed => "embed",
            HtmlElementKind::Iframe => "iframe",
            HtmlElementKind::Object => "object",
            HtmlElementKind::Picture => "picture",
            HtmlElementKind::Source => "source",
            HtmlElementKind::Canvas => "canvas",
            HtmlElementKind::Noscript => "noscript",
            HtmlElementKind::Script => "script",
            HtmlElementKind::Del => "del",
            HtmlElementKind::Ins => "ins",
            HtmlElementKind::Caption => "caption",
            HtmlElementKind::Col => "col",
            HtmlElementKind::Colgroup => "colgroup",
            HtmlElementKind::Table => "table",
            HtmlElementKind::Tbody => "tbody",
            HtmlElementKind::Td => "td",
            HtmlElementKind::Tfoot => "tfoot",
            HtmlElementKind::Th => "th",
            HtmlElementKind::Thead => "thead",
            HtmlElementKind::Tr => "tr",
            HtmlElementKind::Button => "button",
            HtmlElementKind::Datalist => "datalist",
            HtmlElementKind::Fieldset => "fieldset",
            HtmlElementKind::Form => "form",
            HtmlElementKind::Input => "input",
            HtmlElementKind::Label => "label",
            HtmlElementKind::Legend => "legend",
            HtmlElementKind::Meter => "meter",
            HtmlElementKind::Optgroup => "optgroup",
            HtmlElementKind::Option => "option",
            HtmlElementKind::Output => "output",
            HtmlElementKind::Progress => "progress",
            HtmlElementKind::Select => "select",
            HtmlElementKind::Textarea => "textarea",
            HtmlElementKind::Details => "details",
            HtmlElementKind::Dialog => "dialog",
            HtmlElementKind::Summary => "summary",
            HtmlElementKind::Slot => "slot",
            HtmlElementKind::Template => "template",
            HtmlElementKind::Svg => "svg",
            HtmlElementKind::G => "g",
            HtmlElementKind::Path => "path",
            HtmlElementKind::Circle => "circle",
            HtmlElementKind::Rect => "rect",
            HtmlElementKind::Line => "line",
            HtmlElementKind::Polygon => "polygon",
            HtmlElementKind::Polyline => "polyline",
            HtmlElementKind::Ellipse => "ellipse",
            HtmlElementKind::Defs => "defs",
            HtmlElementKind::Use => "use",
            HtmlElementKind::Symbol => "symbol",
            HtmlElementKind::Desc => "desc",
            HtmlElementKind::Marker => "marker",
            HtmlElementKind::Mask => "mask",
            HtmlElementKind::Pattern => "pattern",
            HtmlElementKind::Stop => "stop",
            HtmlElementKind::Switch => "switch",
            HtmlElementKind::Text => "text",
            HtmlElementKind::Tspan => "tspan",
            HtmlElementKind::Image => "image",
            HtmlElementKind::View => "view",
            HtmlElementKind::Animate => "animate",
            HtmlElementKind::Set => "set",
            HtmlElementKind::Mpath => "mpath",
            HtmlElementKind::Metadata => "metadata",
            HtmlElementKind::Filter => "filter",
            HtmlElementKind::Custom(s) => s.as_str(),
        }
    }

    fn known(name: &str) -> Option<HtmlElementKind> {
        let element = match name {
            "html" => HtmlElementKind::Html,
            "base" => HtmlElementKind::Base,
            "head" => HtmlElementKind::Head,
            "link" => HtmlElementKind::Link,
            "meta" => HtmlElementKind::Meta,
            "style" => HtmlElementKind::Style,
            "title" => HtmlElementKind::Title,
            "body" => HtmlElementKind::Body,
            "address" => HtmlElementKind::Address,
            "article" => HtmlElementKind::Article,
            "aside" => HtmlElementKind::Aside,
            "footer" => HtmlElementKind::Footer,
            "header" => HtmlElementKind::Header,
            "h1" => HtmlElementKind::H1,
            "h2" => HtmlElementKind::H2,
            "h3" => HtmlElementKind::H3,
            "h4" => HtmlElementKind::H4,
            "h5" => HtmlElementKind::H5,
            "h6" => HtmlElementKind::H6,
            "hgroup" => HtmlElementKind::Hgroup,
            "main" => HtmlElementKind::Main,
            "nav" => HtmlElementKind::Nav,
            "section" => HtmlElementKind::Section,
            "search" => HtmlElementKind::Search,
            "blockquote" => HtmlElementKind::Blockquote,
            "dd" => HtmlElementKind::Dd,
            "div" => HtmlElementKind::Div,
            "dl" => HtmlElementKind::Dl,
            "dt" => HtmlElementKind::Dt,
            "figcaption" => HtmlElementKind::Figcaption,
            "figure" => HtmlElementKind::Figure,
            "hr" => HtmlElementKind::Hr,
            "li" => HtmlElementKind::Li,
            "menu" => HtmlElementKind::Menu,
            "ol" => HtmlElementKind::Ol,
            "p" => HtmlElementKind::P,
            "pre" => HtmlElementKind::Pre,
            "ul" => HtmlElementKind::Ul,
            "a" => HtmlElementKind::A,
            "abbr" => HtmlElementKind::Abbr,
            "b" => HtmlElementKind::B,
            "bdi" => HtmlElementKind::Bdi,
            "bdo" => HtmlElementKind::Bdo,
            "br" => HtmlElementKind::Br,
            "cite" => HtmlElementKind::Cite,
            "code" => HtmlElementKind::Code,
            "data" => HtmlElementKind::Data,
            "dfn" => HtmlElementKind::Dfn,
            "em" => HtmlElementKind::Em,
            "i" => HtmlElementKind::I,
            "kbd" => HtmlElementKind::Kbd,
            "mark" => HtmlElementKind::Mark,
            "q" => HtmlElementKind::Q,
            "rp" => HtmlElementKind::Rp,
            "rt" => HtmlElementKind::Rt,
            "ruby" => HtmlElementKind::Ruby,
            "s" => HtmlElementKind::S,
            "samp" => HtmlElementKind::Samp,
            "small" => HtmlElementKind::Small,
            "span" => HtmlElementKind::Span,
            "strong" => HtmlElementKind::Strong,
            "sub" => HtmlElementKind::Sub,
            "sup" => HtmlElementKind::Sup,
            "time" => HtmlElementKind::Time,
            "u" => HtmlElementKind::U,
            "var" => HtmlElementKind::Var,
            "wbr" => HtmlElementKind::Wbr,
            "area" => HtmlElementKind::Area,
            "audio" => HtmlElementKind::Audio,
            "img" => HtmlElementKind::Img,
            "map" => HtmlElementKind::Map,
            "track" => HtmlElementKind::Track,
            "video" => HtmlElementKind::Video,
            "embed" => HtmlElementKind::Embed,
            "iframe" => HtmlElementKind::Iframe,
            "object" => HtmlElementKind::Object,
            "picture" => HtmlElementKind::Picture,
            "source" => HtmlElementKind::Source,
            "canvas" => HtmlElementKind::Canvas,
            "noscript" => HtmlElementKind::Noscript,
            "script" => HtmlElementKind::Script,
            "del" => HtmlElementKind::Del,
            "ins" => HtmlElementKind::Ins,
            "caption" => HtmlElementKind::Caption,
            "col" => HtmlElementKind::Col,
            "colgroup" => HtmlElementKind::Colgroup,
            "table" => HtmlElementKind::Table,
            "tbody" => HtmlElementKind::Tbody,
            "td" => HtmlElementKind::Td,
            "tfoot" => HtmlElementKind::Tfoot,
            "th" => HtmlElementKind::Th,
            "thead" => HtmlElementKind::Thead,
            "tr" => HtmlElementKind::Tr,
            "button" => HtmlElementKind::Button,
            "datalist" => HtmlElementKind::Datalist,
            "fieldset" => HtmlElementKind::Fieldset,
            "form" => HtmlElementKind::Form,
            "input" => HtmlElementKind::Input,
            "label" => HtmlElementKind::Label,
            "legend" => HtmlElementKind::Legend,
            "meter" => HtmlElementKind::Meter,
            "optgroup" => HtmlElementKind::Optgroup,
            "option" => HtmlElementKind::Option,
            "output" => HtmlElementKind::Output,
            "progress" => HtmlElementKind::Progress,
            "select" => HtmlElementKind::Select,
            "textarea" => HtmlElementKind::Textarea,
            "details" => HtmlElementKind::Details,
            "dialog" => HtmlElementKind::Dialog,
            "summary" => HtmlElementKind::Summary,
            "slot" => HtmlElementKind::Slot,
            "template" => HtmlElementKind::Template,
            "svg" => HtmlElementKind::Svg,
            "g" => HtmlElementKind::G,
            "path" => HtmlElementKind::Path,
            "circle" => HtmlElementKind::Circle,
            "rect" => HtmlElementKind::Rect,
            "line" => HtmlElementKind::Line,
            "polygon" => HtmlElementKind::Polygon,
            "polyline" => HtmlElementKind::Polyline,
            "ellipse" => HtmlElementKind::Ellipse,
            "defs" => HtmlElementKind::Defs,
            "use" => HtmlElementKind::Use,
            "symbol" => HtmlElementKind::Symbol,
            "desc" => HtmlElementKind::Desc,
            "marker" => HtmlElementKind::Marker,
            "mask" => HtmlElementKind::Mask,
            "pattern" => HtmlElementKind::Pattern,
            "stop" => HtmlElementKind::Stop,
            "switch" => HtmlElementKind::Switch,
            "text" => HtmlElementKind::Text,
            "tspan" => HtmlElementKind::Tspan,
            "image" => HtmlElementKind::Image,
            "view" => HtmlElementKind::View,
            "animate" => HtmlElementKind::Animate,
            "set" => HtmlElementKind::Set,
            "mpath" => HtmlElementKind::Mpath,
            "metadata" => HtmlElementKind::Metadata,
            "filter" => HtmlElementKind::Filter,
            _ => return None,
        };
        Some(element)
    }

    pub fn is_void(&self) -> bool {
        is_void_element_tag(self.as_str())
    }

    /// Return true for SVG elements (attribute-name validation is skipped for them).
    pub fn is_svg(&self) -> bool {
        matches!(
            self,
            HtmlElementKind::Svg
                | HtmlElementKind::G
                | HtmlElementKind::Path
                | HtmlElementKind::Circle
                | HtmlElementKind::Rect
                | HtmlElementKind::Line
                | HtmlElementKind::Polygon
                | HtmlElementKind::Polyline
                | HtmlElementKind::Ellipse
                | HtmlElementKind::Defs
                | HtmlElementKind::Use
                | HtmlElementKind::Symbol
                | HtmlElementKind::Desc
                | HtmlElementKind::Marker
                | HtmlElementKind::Mask
                | HtmlElementKind::Pattern
                | HtmlElementKind::Stop
                | HtmlElementKind::Switch
                | HtmlElementKind::Text
                | HtmlElementKind::Tspan
                | HtmlElementKind::Image
                | HtmlElementKind::View
                | HtmlElementKind::Animate
                | HtmlElementKind::Set
                | HtmlElementKind::Mpath
                | HtmlElementKind::Metadata
                | HtmlElementKind::Filter
        )
    }

    pub fn parse(name: &str) -> Option<HtmlElementKind> {
        if let Some(el) = Self::known(name) {
            Some(el)
        } else if name.contains('-') {
            Some(HtmlElementKind::Custom(CheapString::new(name.to_string())))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifies_known_html_element() {
        let el = HtmlElementKind::parse("div").unwrap();
        assert_eq!(el, HtmlElementKind::Div);
        assert_eq!(el.as_str(), "div");
    }

    #[test]
    fn classifies_known_svg_element() {
        assert_eq!(HtmlElementKind::parse("path"), Some(HtmlElementKind::Path));
    }

    #[test]
    fn classifies_hyphenated_custom_element() {
        let el = HtmlElementKind::parse("my-widget").unwrap();
        assert_eq!(
            el,
            HtmlElementKind::Custom(CheapString::new("my-widget".to_string()))
        );
        assert_eq!(el.as_str(), "my-widget");
    }

    #[test]
    fn rejects_unknown_bare_name() {
        assert_eq!(HtmlElementKind::parse("dvi"), None);
    }

    #[test]
    fn rejects_mathml() {
        assert_eq!(HtmlElementKind::parse("math"), None);
    }

    #[test]
    fn void_element_check_matches_helper() {
        assert!(HtmlElementKind::parse("br").unwrap().is_void());
        assert!(!HtmlElementKind::parse("div").unwrap().is_void());
        assert!(!HtmlElementKind::parse("my-widget").unwrap().is_void());
    }

    #[test]
    fn is_svg_classifies_svg_elements() {
        assert!(HtmlElementKind::parse("svg").unwrap().is_svg());
        assert!(HtmlElementKind::parse("path").unwrap().is_svg());
        assert!(HtmlElementKind::parse("circle").unwrap().is_svg());
    }

    #[test]
    fn is_svg_rejects_html_elements() {
        assert!(!HtmlElementKind::parse("div").unwrap().is_svg());
        assert!(!HtmlElementKind::parse("a").unwrap().is_svg());
    }
}
