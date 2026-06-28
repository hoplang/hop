use super::element::HtmlElement;

/// Attributes valid on every HTML element.
fn is_global_attribute(name: &str) -> bool {
    matches!(
        name,
        "accesskey"
            | "autocapitalize"
            | "autocorrect"
            | "autofocus"
            | "class"
            | "contenteditable"
            | "dir"
            | "draggable"
            | "enterkeyhint"
            | "exportparts"
            | "hidden"
            | "id"
            | "inert"
            | "inputmode"
            | "is"
            | "itemid"
            | "itemprop"
            | "itemref"
            | "itemscope"
            | "itemtype"
            | "lang"
            | "nonce"
            | "part"
            | "popover"
            | "role"
            | "slot"
            | "spellcheck"
            | "style"
            | "tabindex"
            | "title"
            | "translate"
            | "writingsuggestions"
    )
}

/// Attributes valid on a specific element (beyond the global set).
/// `name` must already be lowercased.
fn element_specific_attribute(element: &HtmlElement, name: &str) -> bool {
    match element {
        HtmlElement::A => matches!(
            name,
            "download"
                | "href"
                | "hreflang"
                | "ping"
                | "referrerpolicy"
                | "rel"
                | "target"
                | "type"
        ),
        HtmlElement::Area => matches!(
            name,
            "alt"
                | "coords"
                | "download"
                | "href"
                | "ping"
                | "referrerpolicy"
                | "rel"
                | "shape"
                | "target"
        ),
        HtmlElement::Audio => matches!(
            name,
            "autoplay" | "controls" | "crossorigin" | "loop" | "muted" | "preload" | "src"
        ),
        HtmlElement::Base => matches!(name, "href" | "target"),
        HtmlElement::Blockquote => matches!(name, "cite"),
        HtmlElement::Button => matches!(
            name,
            "command"
                | "commandfor"
                | "disabled"
                | "form"
                | "formaction"
                | "formenctype"
                | "formmethod"
                | "formnovalidate"
                | "formtarget"
                | "name"
                | "popovertarget"
                | "popovertargetaction"
                | "type"
                | "value"
        ),
        HtmlElement::Canvas => matches!(name, "height" | "width"),
        HtmlElement::Col => matches!(name, "span"),
        HtmlElement::Colgroup => matches!(name, "span"),
        HtmlElement::Data => matches!(name, "value"),
        HtmlElement::Del => matches!(name, "cite" | "datetime"),
        HtmlElement::Details => matches!(name, "name" | "open"),
        HtmlElement::Dialog => matches!(name, "closedby" | "open"),
        HtmlElement::Embed => matches!(name, "height" | "src" | "type" | "width"),
        HtmlElement::Fieldset => matches!(name, "disabled" | "form" | "name"),
        HtmlElement::Form => matches!(
            name,
            "accept-charset"
                | "action"
                | "autocomplete"
                | "enctype"
                | "method"
                | "name"
                | "novalidate"
                | "rel"
                | "target"
        ),
        HtmlElement::Iframe => matches!(
            name,
            "allow"
                | "allowfullscreen"
                | "height"
                | "loading"
                | "name"
                | "referrerpolicy"
                | "sandbox"
                | "src"
                | "srcdoc"
                | "width"
        ),
        HtmlElement::Img => matches!(
            name,
            "alt"
                | "crossorigin"
                | "decoding"
                | "fetchpriority"
                | "height"
                | "ismap"
                | "loading"
                | "referrerpolicy"
                | "sizes"
                | "src"
                | "srcset"
                | "usemap"
                | "width"
        ),
        HtmlElement::Input => matches!(
            name,
            "accept"
                | "alpha"
                | "alt"
                | "autocomplete"
                | "capture"
                | "checked"
                | "colorspace"
                | "dirname"
                | "disabled"
                | "form"
                | "formaction"
                | "formenctype"
                | "formmethod"
                | "formnovalidate"
                | "formtarget"
                | "height"
                | "list"
                | "max"
                | "maxlength"
                | "min"
                | "minlength"
                | "multiple"
                | "name"
                | "pattern"
                | "placeholder"
                | "popovertarget"
                | "popovertargetaction"
                | "readonly"
                | "required"
                | "size"
                | "src"
                | "step"
                | "type"
                | "value"
                | "width"
        ),
        HtmlElement::Ins => matches!(name, "cite" | "datetime"),
        HtmlElement::Label => matches!(name, "for" | "form"),
        HtmlElement::Li => matches!(name, "value"),
        HtmlElement::Link => matches!(
            name,
            "as" | "blocking"
                | "color"
                | "crossorigin"
                | "disabled"
                | "fetchpriority"
                | "href"
                | "hreflang"
                | "imagesizes"
                | "imagesrcset"
                | "integrity"
                | "media"
                | "referrerpolicy"
                | "rel"
                | "sizes"
                | "type"
        ),
        HtmlElement::Map => matches!(name, "name"),
        HtmlElement::Meta => matches!(
            name,
            "charset" | "content" | "http-equiv" | "media" | "name"
        ),
        HtmlElement::Meter => matches!(name, "high" | "low" | "max" | "min" | "optimum" | "value"),
        HtmlElement::Object => matches!(
            name,
            "data" | "form" | "height" | "name" | "type" | "usemap" | "width"
        ),
        HtmlElement::Ol => matches!(name, "reversed" | "start" | "type"),
        HtmlElement::Optgroup => matches!(name, "disabled" | "label"),
        HtmlElement::Option => matches!(name, "disabled" | "label" | "selected" | "value"),
        HtmlElement::Output => matches!(name, "for" | "form" | "name"),
        HtmlElement::Progress => matches!(name, "max" | "value"),
        HtmlElement::Q => matches!(name, "cite"),
        HtmlElement::Script => matches!(
            name,
            "async"
                | "blocking"
                | "crossorigin"
                | "defer"
                | "fetchpriority"
                | "integrity"
                | "nomodule"
                | "referrerpolicy"
                | "src"
                | "type"
        ),
        HtmlElement::Select => matches!(
            name,
            "autocomplete" | "disabled" | "form" | "multiple" | "name" | "required" | "size"
        ),
        HtmlElement::Slot => matches!(name, "name"),
        HtmlElement::Source => matches!(
            name,
            "height" | "media" | "sizes" | "src" | "srcset" | "type" | "width"
        ),
        HtmlElement::Style => matches!(name, "blocking" | "media"),
        HtmlElement::Td => matches!(name, "colspan" | "headers" | "rowspan"),
        HtmlElement::Textarea => matches!(
            name,
            "autocomplete"
                | "cols"
                | "dirname"
                | "disabled"
                | "form"
                | "maxlength"
                | "minlength"
                | "name"
                | "placeholder"
                | "readonly"
                | "required"
                | "rows"
                | "wrap"
        ),
        HtmlElement::Th => matches!(name, "abbr" | "colspan" | "headers" | "rowspan" | "scope"),
        HtmlElement::Time => matches!(name, "datetime"),
        HtmlElement::Track => matches!(name, "default" | "kind" | "label" | "src" | "srclang"),
        HtmlElement::Video => matches!(
            name,
            "autoplay"
                | "controls"
                | "crossorigin"
                | "height"
                | "loop"
                | "muted"
                | "playsinline"
                | "poster"
                | "preload"
                | "src"
                | "width"
        ),

        HtmlElement::Html
        | HtmlElement::Head
        | HtmlElement::Title
        | HtmlElement::Body
        | HtmlElement::Address
        | HtmlElement::Article
        | HtmlElement::Aside
        | HtmlElement::Footer
        | HtmlElement::Header
        | HtmlElement::H1
        | HtmlElement::H2
        | HtmlElement::H3
        | HtmlElement::H4
        | HtmlElement::H5
        | HtmlElement::H6
        | HtmlElement::Hgroup
        | HtmlElement::Main
        | HtmlElement::Nav
        | HtmlElement::Section
        | HtmlElement::Search
        | HtmlElement::Dd
        | HtmlElement::Div
        | HtmlElement::Dl
        | HtmlElement::Dt
        | HtmlElement::Figcaption
        | HtmlElement::Figure
        | HtmlElement::Hr
        | HtmlElement::Menu
        | HtmlElement::P
        | HtmlElement::Pre
        | HtmlElement::Ul
        | HtmlElement::Abbr
        | HtmlElement::B
        | HtmlElement::Bdi
        | HtmlElement::Bdo
        | HtmlElement::Br
        | HtmlElement::Cite
        | HtmlElement::Code
        | HtmlElement::Dfn
        | HtmlElement::Em
        | HtmlElement::I
        | HtmlElement::Kbd
        | HtmlElement::Mark
        | HtmlElement::Rp
        | HtmlElement::Rt
        | HtmlElement::Ruby
        | HtmlElement::S
        | HtmlElement::Samp
        | HtmlElement::Small
        | HtmlElement::Span
        | HtmlElement::Strong
        | HtmlElement::Sub
        | HtmlElement::Sup
        | HtmlElement::U
        | HtmlElement::Var
        | HtmlElement::Wbr
        | HtmlElement::Picture
        | HtmlElement::Noscript
        | HtmlElement::Caption
        | HtmlElement::Table
        | HtmlElement::Tbody
        | HtmlElement::Tfoot
        | HtmlElement::Thead
        | HtmlElement::Tr
        | HtmlElement::Datalist
        | HtmlElement::Legend
        | HtmlElement::Summary => false,

        HtmlElement::Template => matches!(
            name,
            "shadowrootclonable"
                | "shadowrootcustomelementregistry"
                | "shadowrootdelegatesfocus"
                | "shadowrootmode"
                | "shadowrootserializable"
        ),

        HtmlElement::Svg
        | HtmlElement::G
        | HtmlElement::Path
        | HtmlElement::Circle
        | HtmlElement::Rect
        | HtmlElement::Line
        | HtmlElement::Polygon
        | HtmlElement::Polyline
        | HtmlElement::Ellipse
        | HtmlElement::Defs
        | HtmlElement::Use
        | HtmlElement::Symbol
        | HtmlElement::Desc
        | HtmlElement::Marker
        | HtmlElement::Mask
        | HtmlElement::Pattern
        | HtmlElement::Stop
        | HtmlElement::Switch
        | HtmlElement::Text
        | HtmlElement::Tspan
        | HtmlElement::Image
        | HtmlElement::View
        | HtmlElement::Animate
        | HtmlElement::Set
        | HtmlElement::Mpath
        | HtmlElement::Metadata
        | HtmlElement::Filter
        | HtmlElement::Custom(_) => false,
    }
}

impl HtmlElement {
    /// Return true if this element accepts an attribute named `name`
    /// (case-insensitive). SVG and custom elements accept anything;
    /// `data-`/`aria-` prefixes are always accepted; `on*` handlers are never
    /// accepted (they are not in any list).
    pub fn accepts_attribute(&self, name: &str) -> bool {
        if self.is_svg() || matches!(self, HtmlElement::Custom(_)) {
            return true;
        }
        let name = name.to_ascii_lowercase();
        if name.starts_with("data-") || name.starts_with("aria-") {
            return true;
        }
        is_global_attribute(&name) || element_specific_attribute(self, &name)
    }
}

#[cfg(test)]
mod tests {
    use super::HtmlElement;

    #[test]
    fn accepts_global_attribute_on_any_element() {
        let div = HtmlElement::parse("div").unwrap();
        let button = HtmlElement::parse("button").unwrap();
        assert!(div.accepts_attribute("class"));
        assert!(button.accepts_attribute("id"));
    }

    #[test]
    fn accepts_element_specific_attribute_on_its_element() {
        let a = HtmlElement::parse("a").unwrap();
        let input = HtmlElement::parse("input").unwrap();
        assert!(a.accepts_attribute("href"));
        assert!(input.accepts_attribute("value"));
        assert!(input.accepts_attribute("placeholder"));
    }

    #[test]
    fn rejects_element_specific_attribute_on_wrong_element() {
        let button = HtmlElement::parse("button").unwrap();
        let div = HtmlElement::parse("div").unwrap();
        assert!(!button.accepts_attribute("href"));
        assert!(!div.accepts_attribute("value"));
        assert!(!div.accepts_attribute("placeholder"));
    }

    #[test]
    fn accepts_shared_attribute_on_each_owning_element() {
        let button = HtmlElement::parse("button").unwrap();
        let input = HtmlElement::parse("input").unwrap();
        let select = HtmlElement::parse("select").unwrap();
        assert!(button.accepts_attribute("disabled"));
        assert!(input.accepts_attribute("disabled"));
        assert!(select.accepts_attribute("disabled"));
    }

    #[test]
    fn accepts_data_and_aria_prefixes_and_rejects_event_handlers() {
        let div = HtmlElement::parse("div").unwrap();
        assert!(div.accepts_attribute("data-x"));
        assert!(div.accepts_attribute("aria-label"));
        assert!(!div.accepts_attribute("onclick"));
    }

    #[test]
    fn is_case_insensitive() {
        let a = HtmlElement::parse("a").unwrap();
        assert!(a.accepts_attribute("HREF"));
        assert!(a.accepts_attribute("Class"));
    }

    #[test]
    fn accepts_value_on_data_element() {
        let data = HtmlElement::parse("data").unwrap();
        assert!(data.accepts_attribute("value"));
        // a non-owning element still rejects it
        let div = HtmlElement::parse("div").unwrap();
        assert!(!div.accepts_attribute("value"));
    }

    #[test]
    fn accepts_rel_on_form_element() {
        let form = HtmlElement::parse("form").unwrap();
        assert!(form.accepts_attribute("rel"));
    }

    #[test]
    fn bypasses_svg_and_custom_elements() {
        let circle = HtmlElement::parse("circle").unwrap();
        let widget = HtmlElement::parse("my-widget").unwrap();
        assert!(circle.accepts_attribute("foobar"));
        assert!(widget.accepts_attribute("foobar"));
    }

    #[test]
    fn accepts_audit_added_attributes() {
        let cases = [
            ("details", "name"),
            ("dialog", "closedby"),
            ("link", "disabled"),
            ("button", "command"),
            ("button", "commandfor"),
            ("input", "alpha"),
            ("input", "colorspace"),
            ("template", "shadowrootmode"),
            ("template", "shadowrootserializable"),
        ];
        for (tag, attr) in cases {
            let el = HtmlElement::parse(tag).unwrap();
            assert!(
                el.accepts_attribute(attr),
                "expected <{tag}> to accept {attr}"
            );
        }
    }

    #[test]
    fn accepts_autocorrect_global_attribute() {
        let div = HtmlElement::parse("div").unwrap();
        assert!(div.accepts_attribute("autocorrect"));
    }

    #[test]
    fn rejects_form_attribute_on_meter() {
        let meter = HtmlElement::parse("meter").unwrap();
        assert!(!meter.accepts_attribute("form"));
        // still accepted where it belongs
        let button = HtmlElement::parse("button").unwrap();
        assert!(button.accepts_attribute("form"));
    }
}
