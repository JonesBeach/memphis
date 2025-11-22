use crate::{
    core::Container,
    domain::Source,
    errors::MemphisResult,
    lexer::Lexer,
    parser::Parser,
    treewalk::{TreewalkInterpreter, TreewalkState, TreewalkValue},
};

#[cfg(test)]
use crate::{domain::ModuleName, runtime::MemphisState, treewalk::types::Module};

pub struct TreewalkContext {
    lexer: Lexer,
    interpreter: TreewalkInterpreter,
}

impl TreewalkContext {
    #[cfg(test)]
    pub fn new(source: Source) -> Self {
        let lexer = Lexer::new(&source);
        let state = MemphisState::from_source(&source);
        let treewalk_state = Container::new(TreewalkState::new(state));
        let module = Module::new(ModuleName::main(), source);
        treewalk_state.enter_module(module);
        let interpreter = TreewalkInterpreter::new(treewalk_state);

        Self { lexer, interpreter }
    }

    /// Initialize a context from a [`Source`] and existing treewalk state.
    pub fn from_state(source: Source, treewalk_state: Container<TreewalkState>) -> Self {
        let lexer = Lexer::new(&source);

        Self {
            lexer,
            interpreter: TreewalkInterpreter::new(treewalk_state),
        }
    }

    pub fn run(&mut self) -> MemphisResult<TreewalkValue> {
        // Destructure to break the borrow into disjoint pieces
        let TreewalkContext {
            lexer, interpreter, ..
        } = self;

        let mut parser = Parser::new(lexer);
        interpreter.execute(&mut parser)
    }

    #[cfg(test)]
    pub fn read(&self, name: &str) -> Option<TreewalkValue> {
        self.interpreter.read_global(name)
    }

    #[cfg(test)]
    pub fn interpreter(&self) -> &TreewalkInterpreter {
        &self.interpreter
    }
}
