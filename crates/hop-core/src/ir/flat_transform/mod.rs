mod eliminate_dead_bindings;
mod freshener;
mod inline_function_calls;
mod lower_matches;
mod perform_partial_evaluation;

pub use eliminate_dead_bindings::eliminate_dead_bindings;
pub use inline_function_calls::inline_function_calls;
pub use lower_matches::lower_matches;
pub use perform_partial_evaluation::perform_partial_evaluation;
