use std::path::{Path, PathBuf};

use crate::domain::{DebugStackFrame, Dunder, ToDebugStackFrame};

/// Represents a Python source, whether it comes from a file, a string, or an embedded Rust module.
///
/// An empty module occurs when there is no Python code for a module. This can occur for a few
/// reasons:
/// 1) Rust-backed module
/// 2) a module created as a layer in an import such as `import mypackage.mymodule`.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Source {
    name: Option<String>,
    path: Option<PathBuf>,
    text: Option<String>,
}

impl Source {
    const DEFAULT_NAME: Dunder = Dunder::Main;
    const DEFAULT_CONTEXT: &str = "<module>";
    const DEFAULT_PATH: &str = "<stdin>";
    const DEFAULT_TEXT: &str = "<module with no Python code>";

    pub fn from_path(name: &str, path: PathBuf, text: String) -> Self {
        Self {
            name: Some(name.to_string()),
            path: Some(path),
            text: Some(text),
        }
    }

    pub fn from_named_path(path: PathBuf, text: String) -> Self {
        Self {
            name: None,
            path: Some(path),
            text: Some(text),
        }
    }

    /// When code is provided directly as a string without reading from the file system.
    pub fn from_text(text: &str) -> Self {
        Self {
            name: None,
            path: None,
            text: Some(text.to_string()),
        }
    }

    pub fn path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }

    pub fn display_path(&self) -> &Path {
        self.path
            .as_deref()
            .unwrap_or(Path::new(Self::DEFAULT_PATH))
    }

    pub fn name(&self) -> &str {
        self.name.as_deref().unwrap_or(Self::DEFAULT_NAME.into())
    }

    pub fn context(&self) -> &str {
        self.name.as_deref().unwrap_or(Self::DEFAULT_CONTEXT)
    }

    pub fn has_text(&self) -> bool {
        self.text.is_some()
    }

    pub fn text(&self) -> &str {
        self.text.as_deref().unwrap_or(Self::DEFAULT_TEXT)
    }
}

impl ToDebugStackFrame for Source {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new(self.context(), self.display_path().to_path_buf(), 1)
    }
}
