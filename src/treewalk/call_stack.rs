use std::fmt::{Display, Error, Formatter};
use std::path::PathBuf;

use crate::treewalk::types::function::Function;

use super::LoadedModule;

#[derive(Debug, PartialEq, Clone)]
pub struct StackFrame {
    pub function_name: Option<String>,
    pub file_path: Option<PathBuf>,
    pub line_number: usize,
}

impl StackFrame {
    pub fn new_root(file_path: PathBuf) -> Self {
        Self {
            function_name: None,
            file_path: Some(file_path),
            line_number: 1,
        }
    }

    pub fn new_module(module: LoadedModule) -> Self {
        Self {
            function_name: Some(module.name()),
            file_path: Some(module.path()),
            line_number: 1,
        }
    }

    pub fn new_function(function: Function) -> Self {
        Self {
            function_name: Some(function.name),
            file_path: Some(function.module.borrow().path()),
            line_number: function.line_number,
        }
    }

    fn empty_path() -> String {
        "<stdin>".into()
    }

    fn file_path_str(&self) -> String {
        match &self.file_path {
            Some(path) => path.to_str().unwrap_or(&Self::empty_path()).to_string(),
            None => Self::empty_path(),
        }
    }

    fn empty_function_name() -> String {
        "<module>".into()
    }

    fn function_name(&self) -> String {
        self.function_name
            .clone()
            .unwrap_or(Self::empty_function_name())
    }

    fn set_line(&mut self, line: usize) {
        self.line_number = line;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallStack {
    pub frames: Vec<StackFrame>,
}

// Example from Python:
//
//  File [1] "/Users/tyler/Documents/repos/memphis/examples/test.py", [3] line 37, in [3] <module>
//    [3] other.something()
//  File [2] "/Users/tyler/Documents/repos/memphis/examples/other.py", [4] line 4, in [4] something
//    [4] third()
//  File [2] "/Users/tyler/Documents/repos/memphis/examples/other.py", [5] line 7, in [5] third
//    [5] fourth()
//
// Events:
// [1] root module loaded
// [2] other.py imported
// [3] other.something() called
// [4] third() called
// [5] fourth() called unsuccessfully, error thrown

impl Default for CallStack {
    fn default() -> Self {
        Self::new()
    }
}

impl CallStack {
    pub fn new() -> Self {
        Self { frames: vec![] }
    }

    pub fn push_context(&mut self, stack_frame: StackFrame) {
        self.frames.push(stack_frame);
    }

    pub fn pop_context(&mut self) -> Option<StackFrame> {
        self.frames.pop()
    }

    pub fn set_line(&mut self, line: usize) {
        self.frames.last_mut().unwrap().set_line(line);
    }

    /// This is useful for stack traces, so that you know what line number to begin counting from
    /// when executing a block.
    pub fn line_number(&self) -> usize {
        self.frames.last().unwrap().line_number
    }

    /// This is useful for relative imports, so that you know where a path is relative from.
    pub fn current_path(&self) -> Option<PathBuf> {
        self.frames.last().unwrap().file_path.clone()
    }
}

impl Display for CallStack {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "Traceback (most recent call last):")?;
        for frame in &self.frames {
            writeln!(
                f,
                "  File \"{}\", line {}, in {}",
                frame.file_path_str(),
                frame.line_number,
                frame.function_name()
            )?;
        }
        Ok(())
    }
}
