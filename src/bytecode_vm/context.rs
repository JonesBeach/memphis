use crate::{
    bytecode_vm::{Runtime, VmInterpreter, VmResult, VmValue},
    core::Container,
    domain::Source,
    errors::MemphisResult,
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
};

use super::runtime::Module;

pub struct VmContext {
    lexer: Lexer,
    interpreter: VmInterpreter,
}

impl Default for VmContext {
    fn default() -> Self {
        Self::new(Source::default())
    }
}

impl VmContext {
    pub fn new(source: Source) -> Self {
        let lexer = Lexer::new(&source);
        let state = MemphisState::from_source(&source);
        let runtime = Container::new(Runtime::new());
        let interpreter = VmInterpreter::new(state, runtime, source);

        Self { lexer, interpreter }
    }

    pub fn import(
        source: Source,
        state: Container<MemphisState>,
        runtime: Container<Runtime>,
    ) -> VmResult<Container<Module>> {
        let module_name = source.name().to_owned();
        let mut context = VmContext::from_state(source, state.clone(), runtime.clone());

        // TODO we shouldn't squash this error, but it's currently a MemphisError
        let _ = context.run();

        // TODO same with this error
        let module = runtime
            .borrow()
            .read_module(&module_name)
            .expect("Failed to read newly created module");
        Ok(module)
    }

    /// Initialize a context from a [`Source`] and existing treewalk state.
    fn from_state(
        source: Source,
        state: Container<MemphisState>,
        runtime: Container<Runtime>,
    ) -> Self {
        let lexer = Lexer::new(&source);

        Self {
            lexer,
            interpreter: VmInterpreter::new(state, runtime, source),
        }
    }

    pub fn run(&mut self) -> MemphisResult<VmValue> {
        // Destructure to break the borrow into disjoint pieces
        let VmContext {
            lexer, interpreter, ..
        } = self;

        let mut parser = Parser::new(lexer);
        interpreter.execute(&mut parser)
    }
}

#[cfg(test)]
use super::compiler::CodeObject;

#[cfg(test)]
impl VmContext {
    pub fn add_line(&mut self, line: &str) {
        self.lexer
            .add_line(line)
            .expect("Failed to add line to lexer");
    }

    pub fn compile(&mut self) -> MemphisResult<CodeObject> {
        let VmContext {
            lexer, interpreter, ..
        } = self;

        let mut parser = Parser::new(lexer);
        interpreter.compile(&mut parser)
    }

    pub fn read(&mut self, name: &str) -> Option<VmValue> {
        self.interpreter.read_global(name)
    }

    pub fn interpreter(&self) -> &VmInterpreter {
        &self.interpreter
    }
}
