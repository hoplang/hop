use crate::{annotation::Annotation, document::DocumentRange};

#[derive(Debug, Clone)]
pub struct ConfigError {
    message: String,
    range: DocumentRange,
}

impl ConfigError {
    pub(crate) fn new(message: String, range: DocumentRange) -> Self {
        ConfigError { message, range }
    }
}

impl Annotation for ConfigError {
    fn message(&self) -> String {
        self.message.clone()
    }

    fn range(&self) -> &DocumentRange {
        &self.range
    }
}
