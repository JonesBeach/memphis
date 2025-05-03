use crate::{
    bytecode_vm::{compiler::CodeObject, VmInterpreter, VmValue},
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
        let interpreter = VmInterpreter::new(state.clone(), source.clone());

        Self { lexer, interpreter }
    }

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

    pub fn run(&mut self) -> MemphisResult<VmValue> {
        // Destructure to break the borrow into disjoint pieces
        let VmContext {
            lexer, interpreter, ..
        } = self;

        let mut parser = Parser::new(lexer);
        interpreter.execute(&mut parser)
    }

    pub fn read(&mut self, name: &str) -> Option<VmValue> {
        self.interpreter.read_global(name)
    }

    pub fn interpreter(&self) -> &VmInterpreter {
        &self.interpreter
    }

    pub fn interpreter_mut(&mut self) -> &mut VmInterpreter {
        &mut self.interpreter
    }
}
