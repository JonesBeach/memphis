use crate::{
    core::Container,
    domain::Source,
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
    treewalk::{TreewalkInterpreter, TreewalkState, TreewalkValue},
    MemphisError,
};

pub struct TreewalkContext {
    lexer: Lexer,
    interpreter: TreewalkInterpreter,
}

impl Default for TreewalkContext {
    fn default() -> Self {
        Self::new(Source::default())
    }
}

impl TreewalkContext {
    pub fn new(source: Source) -> Self {
        let lexer = Lexer::new(&source);
        let state = MemphisState::from_source(&source);
        let treewalk_state = TreewalkState::from_source_state(state.clone(), source.clone());
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

    pub fn run(&mut self) -> Result<TreewalkValue, MemphisError> {
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
