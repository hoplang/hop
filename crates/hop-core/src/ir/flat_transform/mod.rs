mod eliminate_dead_bindings;
mod expand_closure_matches;
pub mod freshener;
mod inline_function_calls;
mod perform_partial_evaluation;

pub use eliminate_dead_bindings::eliminate_dead_bindings;
pub use expand_closure_matches::expand_closure_matches;
pub use inline_function_calls::inline_function_calls;
pub use perform_partial_evaluation::perform_partial_evaluation;
