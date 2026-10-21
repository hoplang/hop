use std::fmt;
use std::ops::Deref;

#[derive(PartialEq)]
pub struct BoundedVec<T, const MAX: usize>(Vec<T>);

impl<T, const MAX: usize> BoundedVec<T, MAX> {
    pub fn new(items: impl IntoIterator<Item = T>) -> Option<Self> {
        let items = items.into_iter();
        if items.size_hint().0 > MAX {
            return None;
        }
        let items: Vec<T> = items.take(MAX + 1).collect();
        (items.len() <= MAX).then_some(BoundedVec(items))
    }
}

impl<T, const MAX: usize> Deref for BoundedVec<T, MAX> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.0
    }
}

impl<T: fmt::Debug, const MAX: usize> fmt::Debug for BoundedVec<T, MAX> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
