use std::iter::Peekable;

pub struct ChunkBy<T, K> {
    groups: Vec<(K, Vec<T>)>,
}

pub struct ChunkByIter<T, K> {
    inner: std::vec::IntoIter<(K, Vec<T>)>,
}

impl<T, K> Iterator for ChunkByIter<T, K> {
    type Item = (K, std::vec::IntoIter<T>);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(k, v)| (k, v.into_iter()))
    }
}

impl<T, K> IntoIterator for ChunkBy<T, K> {
    type Item = (K, std::vec::IntoIter<T>);
    type IntoIter = ChunkByIter<T, K>;

    fn into_iter(self) -> Self::IntoIter {
        ChunkByIter {
            inner: self.groups.into_iter(),
        }
    }
}

pub trait ChunkByExt: Iterator + Sized {
    fn chunk_by<K, F>(self, mut key_fn: F) -> ChunkBy<Self::Item, K>
    where
        K: PartialEq,
        F: FnMut(&Self::Item) -> K,
    {
        let mut groups: Vec<(K, Vec<Self::Item>)> = Vec::new();
        for item in self {
            let key = key_fn(&item);
            if groups.last().is_some_and(|(k, _)| *k == key) {
                groups.last_mut().unwrap().1.push(item);
            } else {
                groups.push((key, vec![item]));
            }
        }
        ChunkBy { groups }
    }
}

impl<T: Iterator> ChunkByExt for T {}

pub struct PeekingTakeWhile<'a, I: Iterator, F> {
    iter: &'a mut Peekable<I>,
    predicate: F,
}

impl<I: Iterator, F: FnMut(&I::Item) -> bool> Iterator for PeekingTakeWhile<'_, I, F> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter.peek().is_some_and(|item| (self.predicate)(item)) {
            self.iter.next()
        } else {
            None
        }
    }
}

pub trait PeekingExt<I: Iterator> {
    fn peeking_take_while<F>(&mut self, predicate: F) -> PeekingTakeWhile<'_, I, F>
    where
        F: FnMut(&I::Item) -> bool;
}

impl<I: Iterator> PeekingExt<I> for Peekable<I> {
    fn peeking_take_while<F>(&mut self, predicate: F) -> PeekingTakeWhile<'_, I, F>
    where
        F: FnMut(&I::Item) -> bool,
    {
        PeekingTakeWhile {
            iter: self,
            predicate,
        }
    }
}
