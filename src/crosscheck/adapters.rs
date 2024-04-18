use crate::core::InterpreterEntrypoint;
use crate::init::Builder;

use super::test_value::TestValue;
use super::traits::InterpreterTest;

pub struct BytecodeVmAdapter;

impl InterpreterTest for BytecodeVmAdapter {
    fn execute(&self, code: &str) -> TestValue {
        let (mut parser, mut interpreter) = Builder::new().text(code).build_vm_expl();

        match interpreter.run(&mut parser) {
            Ok(r) => r.into(),
            Err(e) => panic!("{}", e),
        }
    }

    fn execute_and_return(&self, code: &str, var: &str) -> TestValue {
        let (mut parser, mut interpreter) = Builder::new().text(code).build_vm_expl();

        match interpreter.run(&mut parser) {
            Ok(_) => interpreter
                .take(var)
                .unwrap_or_else(|| panic!("Variable {} not found", var))
                .into(),
            Err(e) => panic!("{}", e),
        }
    }

    fn execute_and_return_vars(&self, code: &str, vars: Vec<&str>) -> Vec<TestValue> {
        let (mut parser, mut interpreter) = Builder::new().text(code).build_vm_expl();

        match interpreter.run(&mut parser) {
            Ok(_) => vars
                .iter()
                .map(|var| {
                    interpreter
                        .take(var)
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
        let (mut parser, mut interpreter) = Builder::new().text(code).build_treewalk_expl();

        match interpreter.run(&mut parser) {
            Ok(r) => r.into(),
            Err(e) => panic!("{}", e),
        }
    }

    fn execute_and_return(&self, code: &str, var: &str) -> TestValue {
        let (mut parser, mut interpreter) = Builder::new().text(code).build_treewalk_expl();

        match interpreter.run(&mut parser) {
            Ok(_) => interpreter
                .state
                .read(var)
                .unwrap_or_else(|| panic!("Variable {} not found", var))
                .into(),
            Err(e) => panic!("{}", e),
        }
    }

    fn execute_and_return_vars(&self, code: &str, vars: Vec<&str>) -> Vec<TestValue> {
        let (mut parser, mut interpreter) = Builder::new().text(code).build_treewalk_expl();

        match interpreter.run(&mut parser) {
            Ok(_) => vars
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
