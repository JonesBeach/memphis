use crate::{
    bytecode_vm::{Runtime, VmInterpreter, VmValue},
    core::Container,
    domain::Source,
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

    /// Initialize a context from a [`Source`] and existing treewalk state.
    pub fn from_state(
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
impl VmContext {
    pub fn read(&self, name: &str) -> Option<VmValue> {
        self.interpreter.read_global(name)
    }

    pub fn add_line(&mut self, line: &str) {
        self.lexer
            .add_line(line)
            .expect("Failed to add line to lexer");
    }

    pub fn interpreter(&self) -> &VmInterpreter {
        &self.interpreter
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
