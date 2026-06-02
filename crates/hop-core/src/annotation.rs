use crate::document::DocumentRange;

pub trait Annotation {
    fn message(&self) -> String;
    fn range(&self) -> &DocumentRange;
}

impl<T: Annotation> Annotation for &T {
    fn message(&self) -> String {
        (*self).message()
    }
    fn range(&self) -> &DocumentRange {
        (*self).range()
    }
}
