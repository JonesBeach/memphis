use crate::init::MemphisContext;

use super::{test_value::TestValue, traits::InterpreterTest};

pub struct BytecodeVmAdapter;

impl InterpreterTest for BytecodeVmAdapter {
    fn execute(&self, code: &str) -> TestValue {
        let mut context = MemphisContext::from_text(code);

        match context.run_vm() {
            Ok(r) => r.into(),
            Err(e) => panic!("{}", e),
        }
    }

    fn execute_and_return(&self, code: &str, var: &str) -> TestValue {
        let mut context = MemphisContext::from_text(code);

        match context.run_vm_and_return() {
            Ok(vm) => vm
                .take(var)
                .unwrap_or_else(|| panic!("Variable {} not found", var))
                .into(),
            Err(e) => panic!("{}", e),
        }
    }

    fn execute_and_return_vars(&self, code: &str, vars: Vec<&str>) -> Vec<TestValue> {
        let mut context = MemphisContext::from_text(code);

        match context.run_vm_and_return() {
            Ok(vm) => vars
                .iter()
                .map(|var| {
                    vm.take(var)
                        .unwrap_or_else(|| panic!("Variable {} not found", var))
                        .into()
                })
                .collect(),
            Err(e) => panic!("{}", e),
        }
    }
}

pub struct TreewalkAdapter;

impl InterpreterTest for TreewalkAdapter {
    fn execute(&self, code: &str) -> TestValue {
        let mut context = MemphisContext::from_text(code);

        match context.run() {
            Ok(r) => r.into(),
            Err(e) => panic!("{}", e),
        }
    }

    fn execute_and_return(&self, code: &str, var: &str) -> TestValue {
        let mut context = MemphisContext::from_text(code);

        match context.run_and_return_interpreter() {
            Ok(interpreter) => interpreter
                .state
                .read(var)
                .unwrap_or_else(|| panic!("Variable {} not found", var))
                .into(),
            Err(e) => panic!("{}", e),
        }
    }

    fn execute_and_return_vars(&self, code: &str, vars: Vec<&str>) -> Vec<TestValue> {
        let mut context = MemphisContext::from_text(code);

        match context.run_and_return_interpreter() {
            Ok(interpreter) => vars
                .iter()
                .map(|var| {
                    interpreter
                        .state
                        .read(var)
                        .unwrap_or_else(|| panic!("Variable {} not found", var))
                        .into()
                })
                .collect(),
            Err(e) => panic!("{}", e),
        }
    }
}
