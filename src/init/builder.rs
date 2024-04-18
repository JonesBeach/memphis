use std::path::PathBuf;
use std::process;

use crate::bytecode_vm::VmInterpreter;
use crate::core::{Container, InterpreterEntrypoint};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::treewalk::{Interpreter, LoadedModule, StackFrame, State};

pub struct Builder {
    text: Option<String>,
    state: Option<Container<State>>,
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}

impl Builder {
    pub fn new() -> Self {
        Builder {
            text: None,
            state: None,
        }
    }

    pub fn path(&mut self, filename: &str) -> &mut Self {
        self.init_state();
        let input = match self
            .state
            .clone()
            .unwrap()
            .load_root(PathBuf::from(filename))
        {
            Some(c) => c,
            None => {
                eprintln!("Error reading file {}", filename);
                process::exit(1);
            }
        };
        self.text = input.text();
        self.state
            .clone()
            .unwrap()
            .push_context(StackFrame::new_root(input.path()));
        self
    }

    pub fn text(&mut self, text: &str) -> &mut Self {
        self.init_state();

        // hmm this shouldn't be necessary, especially for VM runs
        let stack_frame = StackFrame::new_module(LoadedModule::new_virtual(text));
        self.state.clone().unwrap().push_context(stack_frame);
        self.text = Some(text.into());
        self
    }

    pub fn module(&mut self, module: LoadedModule) -> &mut Self {
        self.init_state();
        let stack_frame = StackFrame::new_module(module.clone());
        self.state.clone().unwrap().push_context(stack_frame);
        self.text = module.text();
        self
    }

    pub fn state(&mut self, state: Container<State>) -> &mut Self {
        if self.state.is_some() {
            panic!("State already set! Must call `state` before `text` or `path`.");
        }
        self.state = Some(state);
        self
    }

    fn init_state(&mut self) {
        self.state = match self.state.clone() {
            Some(s) => Some(s),
            None => Some(Container::new(State::new())),
        };
    }

    pub fn parser(&mut self) -> Parser {
        if self.state.is_none() {
            panic!("State never set! Did you forget to call `text` or `path`?");
        }
        if self.text.is_none() {
            panic!("Text never set! Did you forget to call `text` or `path`?");
        }
        let lexer = Lexer::new(&self.text.clone().unwrap());
        Parser::new(lexer.tokens(), self.state.clone().unwrap())
    }

    pub fn build(&mut self) -> (Parser, impl InterpreterEntrypoint) {
        (self.parser(), Interpreter::new(self.state.clone().unwrap()))
    }

    pub fn build_vm(&mut self) -> (Parser, impl InterpreterEntrypoint) {
        (self.parser(), VmInterpreter::new())
    }

    pub fn build_treewalk_expl(&mut self) -> (Parser, Interpreter) {
        (self.parser(), Interpreter::new(self.state.clone().unwrap()))
    }

    /// When we test the [`VmInterpreter`] we know what type it will be.
    pub fn build_vm_expl(&mut self) -> (Parser, VmInterpreter) {
        (self.parser(), VmInterpreter::new())
    }
}
