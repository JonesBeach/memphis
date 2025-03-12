use std::{
    fmt::{Display, Error, Formatter},
    path::PathBuf,
};

/// A trait that defines the ability to convert an object into a `DebugStackFrame`.
/// This is used to represent individual stack frames in the debugging call stack.
pub trait ToDebugStackFrame {
    fn to_stack_frame(&self) -> DebugStackFrame;
}

/// Represents a single stack frame in the call stack during debugging.
///
/// Each frame includes:
/// - The name of the function or module being executed.
/// - The file path where the code resides.
/// - The line number where the execution is currently occurring.
#[derive(Debug, PartialEq, Clone)]
pub struct DebugStackFrame {
    /// The name of the function or module.
    name: String,
    /// The file path of the script or module associated with this frame.
    file_path: PathBuf,
    /// The line number in the file where the current execution is located.
    line_number: usize,
}

impl DebugStackFrame {
    /// Creates a new `DebugStackFrame`.
    ///
    /// # Arguments
    /// - `name`: The name of the function or module.
    /// - `file_path`: The path to the file associated with this stack frame.
    /// - `line_number`: The line number in the file where the current execution is located.
    pub fn new(name: &str, file_path: PathBuf, line_number: usize) -> Self {
        Self {
            name: name.to_string(),
            file_path,
            line_number,
        }
    }

    /// Returns the name of the function or module.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the file path associated with this stack frame.
    pub fn file_path(&self) -> &PathBuf {
        &self.file_path
    }

    /// Returns the file path as a string.
    ///
    /// # Panics
    /// Panics if the file path is not valid Unicode.
    pub fn file_path_str(&self) -> &str {
        self.file_path.to_str().expect("Path is invalid unicode!")
    }

    /// Returns the current line number in the file.
    pub fn line_number(&self) -> usize {
        self.line_number
    }

    /// Returns the current line number in the file.
    pub fn update_line_number(&mut self, line_number: usize) {
        self.line_number = line_number;
    }
}

impl Display for DebugStackFrame {
    /// Formats the stack frame for display in a traceback-like format.
    ///
    /// Example output:
    /// `File "path/to/file.py", line 42, in function_name`
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(
            f,
            "File \"{}\", line {}, in {}",
            self.file_path_str(),
            self.line_number(),
            self.name()
        )
    }
}
