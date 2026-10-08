mod eliminate_dead_bindings;
mod inline_function_calls;
mod perform_partial_evaluation;

pub use eliminate_dead_bindings::eliminate_dead_bindings;
pub use inline_function_calls::inline_function_calls;
pub use perform_partial_evaluation::perform_partial_evaluation;
