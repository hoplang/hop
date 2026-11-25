use crate::ir::assertion_id::AssertionId;
use crate::ir::runtime::value::Value;

/// A run of a test: the assertions that have failed so far, in the order
/// they failed. A failure does not end the run.
#[derive(Default)]
pub struct TestRun<'a> {
    pub failures: Vec<AssertionFailure<'a>>,
}

/// An assertion that failed, with the values that are not equal.
pub struct AssertionFailure<'a> {
    pub assertion: AssertionId,
    pub left: Value<'a>,
    pub right: Value<'a>,
}
