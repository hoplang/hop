use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

pub trait Idx: Copy {
    fn new(index: usize) -> Self;
    fn index(self) -> usize;
}

#[derive(Debug, Clone)]
pub struct IndexVec<I, T> {
    items: Vec<T>,
    id: PhantomData<I>,
}

impl<I: Idx, T> IndexVec<I, T> {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            id: PhantomData,
        }
    }

    pub fn push(&mut self, item: T) -> I {
        let id = I::new(self.items.len());
        self.items.push(item);
        id
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.items.iter()
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (I, &T)> {
        (self.items.iter().enumerate()).map(|(index, item)| (I::new(index), item))
    }
}

impl<I: Idx, T> Default for IndexVec<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Idx, T> Index<I> for IndexVec<I, T> {
    type Output = T;

    fn index(&self, id: I) -> &T {
        &self.items[id.index()]
    }
}

impl<I: Idx, T> IndexMut<I> for IndexVec<I, T> {
    fn index_mut(&mut self, id: I) -> &mut T {
        &mut self.items[id.index()]
    }
}

impl<I: Idx, T> FromIterator<T> for IndexVec<I, T> {
    fn from_iter<It: IntoIterator<Item = T>>(items: It) -> Self {
        Self {
            items: items.into_iter().collect(),
            id: PhantomData,
        }
    }
}

impl<I, T> IntoIterator for IndexVec<I, T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}
