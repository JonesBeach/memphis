use crate::{init::MemphisContext, types::errors::MemphisError};

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
    fn evaluate(&mut self, code: &str) -> Result<TestValue, MemphisError> {
        let mut context = MemphisContext::from_text(code);

        let result = context.run_vm()?;
        self.context = Some(context);
        Ok(result.into())
    }

    fn read(&mut self, var: &str) -> Option<TestValue> {
        let context = self.context.as_mut()?;
        let vm = context.ensure_vm();
        Some(vm.take(var)?.into())
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
    fn evaluate(&mut self, code: &str) -> Result<TestValue, MemphisError> {
        let mut context = MemphisContext::from_text(code);

        let result = context.run()?;
        self.context = Some(context);
        Ok(result.into())
    }

    fn read(&mut self, var: &str) -> Option<TestValue> {
        let context = self.context.as_ref()?;
        let interpreter = context.ensure_treewalk();
        Some(interpreter.state.read(var)?.into())
    }
}
