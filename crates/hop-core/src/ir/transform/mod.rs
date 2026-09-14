mod eliminate_unused_variable_declarations;
mod inline_function_calls;
mod normalize_html;
mod perform_partial_evaluation;
mod propagate_variable_names;
mod retain_reachable;

pub use eliminate_unused_variable_declarations::eliminate_unused_variable_declarations;
pub use inline_function_calls::inline_function_calls;
pub use normalize_html::normalize_html;
pub use perform_partial_evaluation::perform_partial_evaluation;
pub use propagate_variable_names::propagate_variable_names;
pub use retain_reachable::retain_reachable;
