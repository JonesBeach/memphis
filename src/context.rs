use crate::{
    bytecode_vm::VmContext,
    core::Interpreter,
    domain::{MemphisValue, Source},
    errors::MemphisResult,
    treewalk::TreewalkContext,
    Engine,
};

pub struct MemphisContext {
    context: Box<dyn Interpreter>,
}

impl MemphisContext {
    pub fn new(engine: Engine, source: Source) -> Self {
        let context: Box<dyn Interpreter> = match engine {
            Engine::Treewalk => Box::new(TreewalkContext::new(source)),
            Engine::BytecodeVm => Box::new(VmContext::new(source)),
            #[cfg(feature = "llvm_backend")]
            Engine::LlvmBackend => todo!(),
        };
        Self { context }
    }

    pub fn run(&mut self) -> MemphisResult<MemphisValue> {
        self.context.run()
    }

    pub fn read(&mut self, name: &str) -> Option<MemphisValue> {
        self.context.read(name)
    }

    pub fn add_line(&mut self, line: &str) {
        self.context.add_line(line);
    }
}
