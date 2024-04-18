use std::collections::HashMap;

use super::TestValue;

/// The primary crosscheck interface, allowing us to run test Python code through multiple
/// interpreter engines and compare the output.
pub trait InterpreterTest {
    fn execute(&self, code: &str) -> TestValue;
    fn execute_and_return(&self, code: &str, var: &str) -> TestValue;
    fn execute_and_return_vars(&self, code: &str, vars: Vec<&str>) -> Vec<TestValue>;

    fn assert_expr_expected(&self, code: &str, expected: TestValue) {
        let result = self.execute(code);
        assert_eq!(result, expected);
    }

    fn assert_var_expected(&self, code: &str, var: &str, expected: TestValue) {
        let result = self.execute_and_return(code, var);
        assert_eq!(result, expected);
    }

    fn assert_vars_expected(&self, code: &str, vars: HashMap<&str, TestValue>) {
        let result = self.execute_and_return_vars(code, vars.keys().cloned().collect());
        for (index, var) in vars.iter().enumerate() {
            assert_eq!(&result[index], var.1);
        }
    }
}
