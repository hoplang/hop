use std::collections::HashMap;

/// The Environment models variable scope.
#[derive(Debug, Clone)]
pub struct Environment<T> {
    entries: HashMap<String, EnvironmentEntry<T>>,
    operations: Vec<String>,
}

/// Environment entry that holds both value and a boolean indicating
/// whether the variable has been accessed.
#[derive(Debug, Clone)]
struct EnvironmentEntry<T> {
    value: T,
    accessed: bool,
}

impl<V> Environment<V> {
    pub fn new() -> Self {
        Environment {
            entries: HashMap::new(),
            operations: Vec::new(),
        }
    }

    /// Bind the key to the given value in the environment.
    ///
    /// Returns an error if the variable is already defined.
    pub fn push(&mut self, key: String, value: V) -> Result<(), ()> {
        if self.entries.contains_key(&key) {
            return Err(());
        }
        self.entries.insert(
            key.clone(),
            EnvironmentEntry {
                value,
                accessed: false,
            },
        );
        self.operations.push(key);
        Ok(())
    }

    /// Undo the latest push operation.
    ///
    /// Returns an error if the variable was never accessed.
    pub fn pop(&mut self) -> Result<(), ()> {
        if let Some(key) = self.operations.pop() {
            self.entries
                .remove(&key)
                .map(|entry| match entry.accessed {
                    true => Ok(()),
                    false => Err(()),
                })
                .unwrap_or(Err(()))
        } else {
            Err(())
        }
    }

    /// Access the value behind a key in the environment.
    pub fn lookup(&mut self, key: &str) -> Option<&V> {
        if let Some(entry) = self.entries.get_mut(key) {
            entry.accessed = true;
            Some(&entry.value)
        } else {
            None
        }
    }
}

impl<V> Default for Environment<V> {
    fn default() -> Self {
        Self::new()
    }
}
