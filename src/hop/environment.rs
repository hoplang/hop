use std::collections::HashMap;

// Environment entry that holds both value and access status
#[derive(Debug, Clone)]
struct EnvironmentEntry<T> {
    value: T,
    accessed: bool,
}

// Environment class for managing variable scope
#[derive(Debug, Clone)]
pub struct Environment<T> {
    entries: HashMap<String, EnvironmentEntry<T>>,
    operations: Vec<String>,
}

impl<V> Environment<V> {
    pub fn new() -> Self {
        Environment {
            entries: HashMap::new(),
            operations: Vec::new(),
        }
    }

    // Bind the key to the given value
    // Returns true if successful, false if the variable already exists (shadowing not allowed)
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

    // Undo the latest push operation and return whether the variable was accessed
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

    // Look up a key in the environment
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
