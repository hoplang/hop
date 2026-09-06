use super::element_kind::HtmlElementKind;

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
fn element_specific_attribute(element: &HtmlElementKind, name: &str) -> bool {
    match element {
        HtmlElementKind::A => matches!(
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
        HtmlElementKind::Area => matches!(
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
        HtmlElementKind::Audio => matches!(
            name,
            "autoplay" | "controls" | "crossorigin" | "loop" | "muted" | "preload" | "src"
        ),
        HtmlElementKind::Base => matches!(name, "href" | "target"),
        HtmlElementKind::Blockquote => matches!(name, "cite"),
        HtmlElementKind::Button => matches!(
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
        HtmlElementKind::Canvas => matches!(name, "height" | "width"),
        HtmlElementKind::Col => matches!(name, "span"),
        HtmlElementKind::Colgroup => matches!(name, "span"),
        HtmlElementKind::Data => matches!(name, "value"),
        HtmlElementKind::Del => matches!(name, "cite" | "datetime"),
        HtmlElementKind::Details => matches!(name, "name" | "open"),
        HtmlElementKind::Dialog => matches!(name, "closedby" | "open"),
        HtmlElementKind::Embed => matches!(name, "height" | "src" | "type" | "width"),
        HtmlElementKind::Fieldset => matches!(name, "disabled" | "form" | "name"),
        HtmlElementKind::Form => matches!(
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
        HtmlElementKind::Iframe => matches!(
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
        HtmlElementKind::Img => matches!(
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
        HtmlElementKind::Input => matches!(
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
        HtmlElementKind::Ins => matches!(name, "cite" | "datetime"),
        HtmlElementKind::Label => matches!(name, "for" | "form"),
        HtmlElementKind::Li => matches!(name, "value"),
        HtmlElementKind::Link => matches!(
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
        HtmlElementKind::Map => matches!(name, "name"),
        HtmlElementKind::Meta => matches!(
            name,
            "charset" | "content" | "http-equiv" | "media" | "name"
        ),
        HtmlElementKind::Meter => {
            matches!(name, "high" | "low" | "max" | "min" | "optimum" | "value")
        }
        HtmlElementKind::Object => matches!(
            name,
            "data" | "form" | "height" | "name" | "type" | "usemap" | "width"
        ),
        HtmlElementKind::Ol => matches!(name, "reversed" | "start" | "type"),
        HtmlElementKind::Optgroup => matches!(name, "disabled" | "label"),
        HtmlElementKind::Option => matches!(name, "disabled" | "label" | "selected" | "value"),
        HtmlElementKind::Output => matches!(name, "for" | "form" | "name"),
        HtmlElementKind::Progress => matches!(name, "max" | "value"),
        HtmlElementKind::Q => matches!(name, "cite"),
        HtmlElementKind::Script => matches!(
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
        HtmlElementKind::Select => matches!(
            name,
            "autocomplete" | "disabled" | "form" | "multiple" | "name" | "required" | "size"
        ),
        HtmlElementKind::Slot => matches!(name, "name"),
        HtmlElementKind::Source => matches!(
            name,
            "height" | "media" | "sizes" | "src" | "srcset" | "type" | "width"
        ),
        HtmlElementKind::Style => matches!(name, "blocking" | "media"),
        HtmlElementKind::Td => matches!(name, "colspan" | "headers" | "rowspan"),
        HtmlElementKind::Textarea => matches!(
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
        HtmlElementKind::Th => matches!(name, "abbr" | "colspan" | "headers" | "rowspan" | "scope"),
        HtmlElementKind::Time => matches!(name, "datetime"),
        HtmlElementKind::Track => matches!(name, "default" | "kind" | "label" | "src" | "srclang"),
        HtmlElementKind::Video => matches!(
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

        HtmlElementKind::Html
        | HtmlElementKind::Head
        | HtmlElementKind::Title
        | HtmlElementKind::Body
        | HtmlElementKind::Address
        | HtmlElementKind::Article
        | HtmlElementKind::Aside
        | HtmlElementKind::Footer
        | HtmlElementKind::Header
        | HtmlElementKind::H1
        | HtmlElementKind::H2
        | HtmlElementKind::H3
        | HtmlElementKind::H4
        | HtmlElementKind::H5
        | HtmlElementKind::H6
        | HtmlElementKind::Hgroup
        | HtmlElementKind::Main
        | HtmlElementKind::Nav
        | HtmlElementKind::Section
        | HtmlElementKind::Search
        | HtmlElementKind::Dd
        | HtmlElementKind::Div
        | HtmlElementKind::Dl
        | HtmlElementKind::Dt
        | HtmlElementKind::Figcaption
        | HtmlElementKind::Figure
        | HtmlElementKind::Hr
        | HtmlElementKind::Menu
        | HtmlElementKind::P
        | HtmlElementKind::Pre
        | HtmlElementKind::Ul
        | HtmlElementKind::Abbr
        | HtmlElementKind::B
        | HtmlElementKind::Bdi
        | HtmlElementKind::Bdo
        | HtmlElementKind::Br
        | HtmlElementKind::Cite
        | HtmlElementKind::Code
        | HtmlElementKind::Dfn
        | HtmlElementKind::Em
        | HtmlElementKind::I
        | HtmlElementKind::Kbd
        | HtmlElementKind::Mark
        | HtmlElementKind::Rp
        | HtmlElementKind::Rt
        | HtmlElementKind::Ruby
        | HtmlElementKind::S
        | HtmlElementKind::Samp
        | HtmlElementKind::Small
        | HtmlElementKind::Span
        | HtmlElementKind::Strong
        | HtmlElementKind::Sub
        | HtmlElementKind::Sup
        | HtmlElementKind::U
        | HtmlElementKind::Var
        | HtmlElementKind::Wbr
        | HtmlElementKind::Picture
        | HtmlElementKind::Noscript
        | HtmlElementKind::Caption
        | HtmlElementKind::Table
        | HtmlElementKind::Tbody
        | HtmlElementKind::Tfoot
        | HtmlElementKind::Thead
        | HtmlElementKind::Tr
        | HtmlElementKind::Datalist
        | HtmlElementKind::Legend
        | HtmlElementKind::Summary => false,

        HtmlElementKind::Template => matches!(
            name,
            "shadowrootclonable"
                | "shadowrootcustomelementregistry"
                | "shadowrootdelegatesfocus"
                | "shadowrootmode"
                | "shadowrootserializable"
        ),

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
        | HtmlElementKind::Custom(_) => false,
    }
}

impl HtmlElementKind {
    /// Return true if this element accepts an attribute named `name`
    /// (case-insensitive). SVG and custom elements accept anything;
    /// `data-`/`aria-` prefixes are always accepted; `on*` handlers are never
    /// accepted (they are not in any list).
    pub fn accepts_attribute(&self, name: &str) -> bool {
        if self.is_svg() || matches!(self, HtmlElementKind::Custom(_)) {
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
    use super::HtmlElementKind;

    #[test]
    fn accepts_global_attribute_on_any_element() {
        let div = HtmlElementKind::parse("div").unwrap();
        let button = HtmlElementKind::parse("button").unwrap();
        assert!(div.accepts_attribute("class"));
        assert!(button.accepts_attribute("id"));
    }

    #[test]
    fn accepts_element_specific_attribute_on_its_element() {
        let a = HtmlElementKind::parse("a").unwrap();
        let input = HtmlElementKind::parse("input").unwrap();
        assert!(a.accepts_attribute("href"));
        assert!(input.accepts_attribute("value"));
        assert!(input.accepts_attribute("placeholder"));
    }

    #[test]
    fn rejects_element_specific_attribute_on_wrong_element() {
        let button = HtmlElementKind::parse("button").unwrap();
        let div = HtmlElementKind::parse("div").unwrap();
        assert!(!button.accepts_attribute("href"));
        assert!(!div.accepts_attribute("value"));
        assert!(!div.accepts_attribute("placeholder"));
    }

    #[test]
    fn accepts_shared_attribute_on_each_owning_element() {
        let button = HtmlElementKind::parse("button").unwrap();
        let input = HtmlElementKind::parse("input").unwrap();
        let select = HtmlElementKind::parse("select").unwrap();
        assert!(button.accepts_attribute("disabled"));
        assert!(input.accepts_attribute("disabled"));
        assert!(select.accepts_attribute("disabled"));
    }

    #[test]
    fn accepts_data_and_aria_prefixes_and_rejects_event_handlers() {
        let div = HtmlElementKind::parse("div").unwrap();
        assert!(div.accepts_attribute("data-x"));
        assert!(div.accepts_attribute("aria-label"));
        assert!(!div.accepts_attribute("onclick"));
    }

    #[test]
    fn is_case_insensitive() {
        let a = HtmlElementKind::parse("a").unwrap();
        assert!(a.accepts_attribute("HREF"));
        assert!(a.accepts_attribute("Class"));
    }

    #[test]
    fn accepts_value_on_data_element() {
        let data = HtmlElementKind::parse("data").unwrap();
        assert!(data.accepts_attribute("value"));
        // a non-owning element still rejects it
        let div = HtmlElementKind::parse("div").unwrap();
        assert!(!div.accepts_attribute("value"));
    }

    #[test]
    fn accepts_rel_on_form_element() {
        let form = HtmlElementKind::parse("form").unwrap();
        assert!(form.accepts_attribute("rel"));
    }

    #[test]
    fn bypasses_svg_and_custom_elements() {
        let circle = HtmlElementKind::parse("circle").unwrap();
        let widget = HtmlElementKind::parse("my-widget").unwrap();
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
            let el = HtmlElementKind::parse(tag).unwrap();
            assert!(
                el.accepts_attribute(attr),
                "expected <{tag}> to accept {attr}"
            );
        }
    }

    #[test]
    fn accepts_autocorrect_global_attribute() {
        let div = HtmlElementKind::parse("div").unwrap();
        assert!(div.accepts_attribute("autocorrect"));
    }

    #[test]
    fn rejects_form_attribute_on_meter() {
        let meter = HtmlElementKind::parse("meter").unwrap();
        assert!(!meter.accepts_attribute("form"));
        // still accepted where it belongs
        let button = HtmlElementKind::parse("button").unwrap();
        assert!(button.accepts_attribute("form"));
    }
}
