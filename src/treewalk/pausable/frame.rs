use crate::parser::types::{Ast, Statement};

/// An association between a [`Ast`] of code and the current statement.
#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    program_counter: usize,
    ast: Ast,
}

impl Frame {
    /// Initialize a [`Frame`].
    pub fn new(ast: Ast) -> Self {
        Self {
            ast,
            program_counter: 0,
        }
    }

    /// Return a boolean indicating whether we have instructions left in the block to evaluate.
    pub fn is_finished(&self) -> bool {
        self.len() <= self.program_counter
    }

    /// Mutably access the next [`Statement`] of the block, incrementing the program counter.
    pub fn next_statement(&mut self) -> Statement {
        let statement = self
            .ast
            .get(self.program_counter)
            .expect("No statement!")
            .clone();
        self.program_counter += 1;
        statement
    }

    /// If we encountered an await during while evaluating an instruction, we need to reset the
    /// program counter so that we can rerun this instruction again.
    pub fn step_back(&mut self) {
        self.program_counter -= 1;
    }

    /// Reset the program counter to the start of the block. This is useful to simulate loops.
    pub fn restart(&mut self) {
        self.program_counter = 0;
    }

    /// Return the length of the block held by this frame.
    fn len(&self) -> usize {
        self.ast.len()
    }
}
