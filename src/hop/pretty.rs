use pretty::RcDoc;

/// A trait for types that can be pretty-printed using the `pretty` crate.
pub trait Pretty {
    /// Convert this value into a pretty-printable document.
    fn to_doc(&self) -> RcDoc<'static>;
}
