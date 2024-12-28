use std::{
    fmt::{Display, Error, Formatter},
    path::PathBuf,
};

use super::DebugStackFrame;

/// Represents a call stack used for debugging purposes.
///
/// A `DebugCallStack` is a collection of `DebugStackFrame` instances that represent
/// the nested function calls or module contexts during execution. It is independent
/// of the specific execution engine.
///
/// Example call stack in Python-like format:
///
/// ```text
/// Traceback (most recent call last):
///   File "/path/to/module.py", line 10, in <module>
///     some_function()
///   File "/path/to/another_module.py", line 20, in some_function
///     another_function()
///   ...
/// ```
///
/// Events in a call stack can include module imports, function calls, and errors.
#[derive(Debug, PartialEq, Clone)]
pub struct DebugCallStack {
    /// A vector of stack frames representing the call history.
    frames: Vec<DebugStackFrame>,
}

impl Default for DebugCallStack {
    /// Provides a default, empty call stack.
    fn default() -> Self {
        Self::new()
    }
}

impl DebugCallStack {
    /// Creates a new, empty call stack.
    pub fn new() -> Self {
        Self { frames: vec![] }
    }

    /// Adds a new stack frame to the top of the call stack.
    pub fn push_context(&mut self, stack_frame: DebugStackFrame) {
        self.frames.push(stack_frame);
    }

    /// Removes and returns the top stack frame from the call stack.
    pub fn pop_context(&mut self) -> Option<DebugStackFrame> {
        self.frames.pop()
    }

    /// Updates the line number of the top stack frame.
    ///
    /// # Panics
    /// Panics if the call stack is empty.
    pub fn set_line(&mut self, line: usize) {
        self.frames
            .last_mut()
            .expect("No stack frame! Did you properly set the state?")
            .set_line_number(line);
    }

    /// Retrieves the line number of the top stack frame.
    ///
    /// # Panics
    /// Panics if the call stack is empty.
    pub fn line_number(&self) -> usize {
        self.frames.last().expect("No stack frame!").line_number()
    }

    /// Retrieves the file path of the top stack frame.
    ///
    /// This is useful for relative imports or locating the current execution context.
    ///
    /// # Panics
    /// Panics if the call stack is empty.
    pub fn current_path(&self) -> &PathBuf {
        self.frames.last().expect("No stack frame!").file_path()
    }

    /// Retrieves a specific stack frame by its index.
    ///
    /// # Panics
    /// Panics if the index is out of bounds. This method is available only during tests.
    #[cfg(test)]
    pub fn get(&self, index: usize) -> &DebugStackFrame {
        self.frames.get(index).expect("Index out of bounds!")
    }

    /// Returns the number of frames in the call stack. This method is available only during tests.
    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.frames.len()
    }
}

impl Display for DebugCallStack {
    /// Formats the entire call stack for display, emulating a traceback format.
    ///
    /// Example output:
    /// ```text
    /// Traceback (most recent call last):
    ///   File "path/to/file.py", line 42, in function_name
    ///   File "another_path.py", line 15, in another_function
    /// ```
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "Traceback (most recent call last):")?;
        for frame in &self.frames {
            writeln!(f, "  {}", frame)?;
        }
        Ok(())
    }
}
