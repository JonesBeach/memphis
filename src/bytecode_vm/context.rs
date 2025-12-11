use crate::{
    bytecode_vm::{Runtime, VmInterpreter, VmValue},
    core::{Container, Interpreter},
    domain::{MemphisValue, ModuleName, Source},
    errors::MemphisResult,
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
};

pub struct VmContext {
    lexer: Lexer,
    interpreter: VmInterpreter,
}

impl Default for VmContext {
    fn default() -> Self {
        Self::new(Source::from_text(""))
    }
}

impl VmContext {
    pub fn new(source: Source) -> Self {
        Self::new_at_module(ModuleName::main(), source)
    }

    pub fn new_at_module(module_name: ModuleName, source: Source) -> Self {
        let lexer = Lexer::new(&source);
        let state = MemphisState::from_source(&source);
        let runtime = Container::new(Runtime::new());
        let interpreter = VmInterpreter::new(module_name, state, runtime, source);

        Self { lexer, interpreter }
    }

    /// Initialize a context from a [`Source`] and existing treewalk state.
    pub fn from_state(
        module_name: ModuleName,
        source: Source,
        state: Container<MemphisState>,
        runtime: Container<Runtime>,
    ) -> Self {
        let lexer = Lexer::new(&source);

        Self {
            lexer,
            interpreter: VmInterpreter::new(module_name, state, runtime, source),
        }
    }

    pub fn run_inner(&mut self) -> MemphisResult<VmValue> {
        // Destructure to break the borrow into disjoint pieces
        let VmContext {
            lexer, interpreter, ..
        } = self;

        let mut parser = Parser::new(lexer);
        interpreter.execute(&mut parser)
    }

    pub fn read_inner(&self, name: &str) -> Option<VmValue> {
        self.interpreter.read_global(name)
    }

    pub fn add_line_inner(&mut self, line: &str) {
        self.lexer.add_line(line);
    }

    #[cfg(test)]
    pub fn interpreter(&self) -> &VmInterpreter {
        &self.interpreter
    }
}

impl Interpreter for VmContext {
    fn run(&mut self) -> MemphisResult<MemphisValue> {
        self.run_inner().map(Into::into)
    }

    fn read(&mut self, name: &str) -> Option<MemphisValue> {
        self.read_inner(name).map(Into::into)
    }

    fn add_line(&mut self, line: &str) {
        self.add_line_inner(line);
    }
}

#[cfg(any(test, feature = "wasm"))]
mod wasm_support {
    use super::*;

    use crate::bytecode_vm::compiler::CodeObject;

    impl VmContext {
        pub fn compile(&mut self) -> MemphisResult<CodeObject> {
            let VmContext {
                lexer, interpreter, ..
            } = self;

            let mut parser = Parser::new(lexer);
            interpreter.compile(&mut parser)
        }
    }
}
