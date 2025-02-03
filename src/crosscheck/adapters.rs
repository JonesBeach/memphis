use crate::init::MemphisContext;

use super::{test_value::TestValue, traits::InterpreterTest};

pub struct BytecodeVmAdapter {
    context: Option<MemphisContext>,
}

impl BytecodeVmAdapter {
    pub fn new() -> Self {
        Self { context: None }
    }
}

impl Default for BytecodeVmAdapter {
    fn default() -> Self {
        Self::new()
    }
}

impl InterpreterTest for BytecodeVmAdapter {
    fn evaluate(&mut self, code: &str) -> TestValue {
        let mut context = MemphisContext::from_text(code);

        let result = match context.run_vm() {
            Ok(r) => r.into(),
            Err(e) => panic!("{}", e),
        };
        self.context = Some(context);
        result
    }

    fn read(&mut self, var: &str) -> TestValue {
        let context = self.context.as_mut().expect("no context!");
        let vm = context.ensure_vm();
        vm.take(var)
            .unwrap_or_else(|| panic!("Variable {} not found", var))
            .into()
    }
}

pub struct TreewalkAdapter {
    context: Option<MemphisContext>,
}

impl TreewalkAdapter {
    pub fn new() -> Self {
        Self { context: None }
    }
}

impl Default for TreewalkAdapter {
    fn default() -> Self {
        Self::new()
    }
}

impl InterpreterTest for TreewalkAdapter {
    fn evaluate(&mut self, code: &str) -> TestValue {
        let mut context = MemphisContext::from_text(code);

        let result = match context.run() {
            Ok(r) => r.into(),
            Err(e) => panic!("{}", e),
        };

        self.context = Some(context);
        result
    }

    fn read(&mut self, var: &str) -> TestValue {
        let context = self.context.as_ref().expect("no context!");
        let interpreter = context.ensure_treewalk();
        interpreter
            .state
            .read(var)
            .unwrap_or_else(|| panic!("Variable {} not found", var))
            .into()
    }
}
