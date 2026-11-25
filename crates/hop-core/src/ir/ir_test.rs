use crate::document::DocumentRange;
use crate::ir::function_id::FunctionId;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::function_name::FunctionName;

/// The tests of a program, and the assertions of the source by the id an
/// AssertEq points at them with.
#[derive(Debug, Clone, Default)]
pub struct IrTests {
    pub tests: Vec<IrTest>,
    pub assertions: Vec<IrAssertion>,
}

/// A test: a function without parameters that tests.
#[derive(Debug, Clone)]
pub struct IrTest {
    pub module: RootContainedFilePath,
    pub name: FunctionName,
    pub function: FunctionId,
}

/// An assertion as it is written in the source.
#[derive(Debug, Clone)]
pub struct IrAssertion {
    pub range: DocumentRange,
    /// Whether it is an `assert_eq!`, whose operands a failure reports. An
    /// `assert!` compiles to an AssertEq of its condition and true, and a
    /// failure of it reports neither.
    pub reports_operands: bool,
}
