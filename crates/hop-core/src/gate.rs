/// Runs a sequence of fallible stages, skipping every stage after the first
/// failure and handing that first failure to each of them instead.
#[derive(Debug)]
pub struct Gate<E>(Option<E>);

impl<E> Default for Gate<E> {
    fn default() -> Self {
        Self(None)
    }
}

impl<E: Copy> Gate<E> {
    /// Run a stage, unless an earlier one failed. The first failure is
    /// latched, and returned by this and every later stage.
    pub fn run<T>(&mut self, stage: impl FnOnce() -> Result<T, E>) -> Result<T, E> {
        if let Some(failure) = self.0 {
            return Err(failure);
        }
        stage().inspect_err(|failure| self.0 = Some(*failure))
    }
}
