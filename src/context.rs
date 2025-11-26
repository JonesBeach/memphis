use crate::{
    bytecode_vm::{Runtime, VmInterpreter},
    core::{Container, Interpreter},
    domain::{MemphisValue, ModuleName, Source},
    errors::MemphisResult,
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
    treewalk::{types::Module, TreewalkInterpreter, TreewalkState},
    Engine,
};

pub struct MemphisContext {
    pub lexer: Lexer,
    interpreter: Box<dyn Interpreter>,
}

impl MemphisContext {
    /// Given a [`Source`], setup a context which can be used for any execution engines.
    pub fn new(engine: Engine, source: Source) -> Self {
        let lexer = Lexer::new(&source);
        let interpreter = init_interpreter(engine, source.clone());

        Self { lexer, interpreter }
    }

    pub fn run(&mut self) -> MemphisResult<MemphisValue> {
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
            let treewalk_state = Container::new(TreewalkState::new(state));
            let module = Module::new(ModuleName::main(), source);
            treewalk_state.enter_module(module);
            Box::new(TreewalkInterpreter::new(treewalk_state))
        }
        Engine::BytecodeVm => {
            let runtime = Container::new(Runtime::new());
            Box::new(VmInterpreter::new(
                ModuleName::main(),
                state,
                runtime,
                source.clone(),
            ))
        }
        #[cfg(feature = "llvm_backend")]
        Engine::LlvmBackend => unimplemented!("LLVM Backend not yet supported by MemphisContext."),
    }
}
