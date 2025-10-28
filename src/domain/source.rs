use std::{
    fmt::Display,
    path::{Path, PathBuf},
    process,
};

use crate::domain::{resolve, DebugStackFrame, Dunder, ImportPath, ToDebugStackFrame};

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

    pub fn from_path<P>(filepath: P) -> Self
    where
        P: AsRef<Path> + Display,
    {
        let text = std::fs::read_to_string(&filepath).unwrap_or_else(|_| {
            eprintln!("Error reading file: {filepath}");
            process::exit(1);
        });

        let absolute_path = filepath.as_ref().canonicalize().unwrap_or_else(|_| {
            eprintln!("Error resolving path: {filepath}");
            process::exit(1);
        });

        Self::with_path(absolute_path, text)
    }

    pub fn from_import_path(
        import_path: &ImportPath,
        current_path: &Path,
        search_paths: &[PathBuf],
    ) -> Option<Self> {
        let resolved_path = resolve(import_path, current_path, search_paths)?;

        let text = std::fs::read_to_string(&resolved_path).ok()?;
        Some(Self::with_named_path(
            &import_path.as_str(),
            resolved_path,
            text,
        ))
    }

    /// Provide code directly as a string without reading from the file system.
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

    fn with_named_path(name: &str, path: PathBuf, text: String) -> Self {
        Self {
            name: Some(name.to_string()),
            path: Some(path),
            text: Some(text),
        }
    }

    fn with_path(path: PathBuf, text: String) -> Self {
        Self {
            name: None,
            path: Some(path),
            text: Some(text),
        }
    }
}

impl ToDebugStackFrame for Source {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new(self.context(), self.display_path().to_path_buf(), 1)
    }
}
