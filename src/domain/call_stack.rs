use std::fmt::{Debug, Display, Error, Formatter};

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
#[derive(PartialEq, Clone)]
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
    /// Initializes an empty call stack.
    pub fn new() -> Self {
        Self { frames: vec![] }
    }

    /// Adds a new stack frame to the top of the call stack.
    pub fn push_stack_frame(&mut self, stack_frame: DebugStackFrame) {
        self.frames.push(stack_frame);
    }

    /// Removes and returns the top stack frame from the call stack.
    pub fn pop_stack_frame(&mut self) -> Option<DebugStackFrame> {
        self.frames.pop()
    }

    pub fn update_line_number(&mut self, line_number: usize) {
        self.frames
            .last_mut()
            .expect("Empty call stack!")
            .update_line_number(line_number);
    }

    /// Retrieves a specific stack frame by its index.
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn get(&self, index: usize) -> &DebugStackFrame {
        self.frames.get(index).expect("Index out of bounds!")
    }

    /// Returns the number of frames in the call stack.
    pub fn len(&self) -> usize {
        self.frames.len()
    }

    /// Return true if the number of frames in the call stack is 0, false otherwise.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
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
            writeln!(f, "  {frame}")?;
        }
        Ok(())
    }
}

impl Debug for DebugCallStack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}
