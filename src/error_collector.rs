use std::ops::Deref;

#[derive(Debug)]
pub struct ErrorCollector<E> {
    errors: Vec<E>,
}

impl<E> ErrorCollector<E> {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn push(&mut self, error: E) {
        self.errors.push(error);
    }

    pub fn extend<I: IntoIterator<Item = E>>(&mut self, errors: I) {
        self.errors.extend(errors);
    }

    pub fn ok_or_add<T>(&mut self, result: Result<T, E>) -> Option<T> {
        match result {
            Ok(value) => Some(value),
            Err(err) => {
                self.push(err);
                None
            }
        }
    }

    pub fn clear(&mut self) {
        self.errors.clear();
    }
}

impl<E> Default for ErrorCollector<E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<E> Deref for ErrorCollector<E> {
    type Target = Vec<E>;

    fn deref(&self) -> &Self::Target {
        &self.errors
    }
}

impl<'a, E> IntoIterator for &'a ErrorCollector<E> {
    type Item = &'a E;
    type IntoIter = std::slice::Iter<'a, E>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.iter()
    }
}
