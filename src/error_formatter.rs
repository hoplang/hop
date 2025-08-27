use crate::common::RangeError;
use crate::source_annotator::SourceAnnotator;

#[derive(Debug)]
struct ModuleInfo {
    module_name: String,
    source_code: String,
    errors: Vec<RangeError>,
}

pub struct ErrorFormatter {
    modules: Vec<ModuleInfo>,
    show_location: bool,
}

impl ErrorFormatter {
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
            show_location: true, // Default to showing location info
        }
    }

    pub fn add_errors(
        &mut self,
        module_name: String,
        source_code: String,
        errors: Vec<RangeError>,
    ) {
        if errors.is_empty() {
            panic!("add_errors called with empty errors array");
        }

        if let Some(module_info) = self
            .modules
            .iter_mut()
            .find(|m| m.module_name == module_name)
        {
            module_info.errors.extend(errors);
        } else {
            self.modules.push(ModuleInfo {
                module_name,
                source_code,
                errors,
            });
        }
    }

    pub fn without_location_info(mut self) -> Self {
        self.show_location = false;
        self
    }

    pub fn has_errors(&self) -> bool {
        !self.modules.is_empty()
    }

    pub fn format_all_errors(&self) -> String {
        let mut formatted_errors = String::new();
        let mut first_module = true;

        for module_info in &self.modules {
            if module_info.errors.is_empty() {
                continue;
            }

            if !first_module {
                formatted_errors.push('\n');
            }
            first_module = false;

            let mut annotator = SourceAnnotator::new()
                .with_label("error")
                .with_underline('^')
                .with_lines_before(1);

            if self.show_location {
                let filename = format!("{}.hop", module_info.module_name);
                annotator = annotator.with_location().with_filename(filename);
            }

            let output = annotator.annotate(&module_info.source_code, &module_info.errors);

            formatted_errors.push_str(&output);
        }

        formatted_errors
    }
}
