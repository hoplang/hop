//! A utility for accumulating multiple errors during operations that may fail
//! in multiple ways, such as parsing or type checking.

/// Extension methods for error-collecting [`Vec`]s.
pub trait ErrorCollectionExt<E> {
    /// If `result` is `Ok`, returns `Some(value)`. If `Err`, pushes the error
    /// onto `self` and returns `None`.
    fn ok_or_add<T>(&mut self, result: Result<T, E>) -> Option<T>;
}

impl<E> ErrorCollectionExt<E> for Vec<E> {
    fn ok_or_add<T>(&mut self, result: Result<T, E>) -> Option<T> {
        match result {
            Ok(value) => Some(value),
            Err(err) => {
                self.push(err);
                None
            }
        }
    }
}
