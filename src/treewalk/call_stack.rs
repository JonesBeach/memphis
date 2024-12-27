use std::{
    fmt::{Display, Error, Formatter},
    path::PathBuf,
};

use crate::treewalk::types::function::Function;

use super::ModuleSource;

#[derive(Debug, PartialEq, Clone)]
pub struct StackFrame {
    function_name: String,
    file_path: PathBuf,
    line_number: usize,
}

impl StackFrame {
    pub fn from_module(module: ModuleSource) -> Self {
        Self {
            function_name: module.name().to_string(),
            file_path: module.path().to_path_buf(),
            line_number: 1,
        }
    }

    pub fn from_function(function: Function) -> Self {
        Self {
            function_name: function.name,
            file_path: function.module.borrow().path().to_path_buf(),
            line_number: function.line_number,
        }
    }

    pub fn file_path_str(&self) -> &str {
        self.file_path.to_str().expect("Path is invalid unicode!")
    }

    pub fn function_name(&self) -> &str {
        &self.function_name
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn set_line_number(&mut self, line: usize) {
        self.line_number = line;
    }
}

impl Display for StackFrame {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(
            f,
            "File \"{}\", line {}, in {}",
            self.file_path_str(),
            self.line_number,
            self.function_name()
        )
    }
}

/// A call stack, which is independent of the execution engine Memphis is using for evaluation.
///
/// Example from Python:
///
///  File [1] "/Users/tyler/Documents/repos/memphis/examples/test.py", [3] line 37, in [3] <module>
///    [3] other.something()
///  File [2] "/Users/tyler/Documents/repos/memphis/examples/other.py", [4] line 4, in [4] something
///    [4] third()
///  File [2] "/Users/tyler/Documents/repos/memphis/examples/other.py", [5] line 7, in [5] third
///    [5] fourth()
///
/// Events:
/// [1] root module loaded
/// [2] other.py imported
/// [3] other.something() called
/// [4] third() called
/// [5] fourth() called unsuccessfully, error thrown
#[derive(Debug, PartialEq, Clone)]
pub struct CallStack {
    frames: Vec<StackFrame>,
}

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
        self.frames
            .last_mut()
            .expect("No stack frame! Did you properly set the state?")
            .set_line_number(line);
    }

    /// This is useful for stack traces, so that you know what line number to begin counting from
    /// when executing a block.
    pub fn line_number(&self) -> usize {
        self.frames.last().expect("No stack frame!").line_number
    }

    /// This is useful for relative imports, so that you know where a path is relative from.
    pub fn current_path(&self) -> &PathBuf {
        &self.frames.last().expect("No stack frame!").file_path
    }

    #[cfg(test)]
    pub fn get(&self, index: usize) -> &StackFrame {
        self.frames.get(index).expect("Index out of bounds!")
    }

    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.frames.len()
    }
}

impl Display for CallStack {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "Traceback (most recent call last):")?;
        for frame in &self.frames {
            writeln!(f, "  {}", frame)?;
        }
        Ok(())
    }
}
