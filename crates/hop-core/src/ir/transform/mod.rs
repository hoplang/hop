mod eliminate_unused_variable_declarations;
mod inline_function_calls;
mod normalize_fragments;
mod perform_partial_evaluation;
mod retain_reachable;

pub use eliminate_unused_variable_declarations::eliminate_unused_variable_declarations;
pub use inline_function_calls::inline_function_calls;
pub use normalize_fragments::normalize_fragments;
pub use perform_partial_evaluation::perform_partial_evaluation;
pub use retain_reachable::retain_reachable;
