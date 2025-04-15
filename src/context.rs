use crate::{
    bytecode_vm::VmInterpreter,
    core::Interpreter,
    domain::{MemphisValue, Source},
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
    treewalk::{TreewalkInterpreter, TreewalkState},
    types::errors::MemphisError,
    Engine,
};

pub struct MemphisContext {
    pub lexer: Lexer,
    interpreter: Box<dyn Interpreter>,
}

impl Default for MemphisContext {
    fn default() -> Self {
        Self::new(Engine::DEFAULT_ENGINE, Source::default())
    }
}

impl MemphisContext {
    /// Given a [`Source`], setup a context which can be used for any execution engines.
    pub fn new(engine: Engine, source: Source) -> Self {
        let lexer = Lexer::new(&source);
        let interpreter = init_interpreter(engine, source.clone());

        Self { lexer, interpreter }
    }

    pub fn run(&mut self) -> Result<MemphisValue, MemphisError> {
        // Destructure to break the borrow into disjoint pieces
        let MemphisContext {
            lexer, interpreter, ..
        } = self;

        let mut parser = Parser::new(lexer);
        interpreter.run(&mut parser)
    }

    pub fn read(&mut self, name: &str) -> Option<MemphisValue> {
        self.interpreter.read(name)
    }
}

fn init_interpreter(engine: Engine, source: Source) -> Box<dyn Interpreter> {
    let state = MemphisState::from_source(&source);
    match engine {
        Engine::Treewalk => {
            let treewalk_state = TreewalkState::from_source_state(state.clone(), source.clone());
            Box::new(TreewalkInterpreter::new(treewalk_state))
        }
        Engine::BytecodeVm => Box::new(VmInterpreter::new(state.clone(), source.clone())),
        #[cfg(feature = "llvm_backend")]
        Engine::LlvmBackend => unimplemented!("LLVM Backend not yet supported by MemphisContext."),
    }
}
