use std::{
    fmt::Display,
    fs, io,
    path::{Path, PathBuf},
    process,
};

use crate::domain::{Dunder, ModuleName};

/// Represents a Python source, whether it comes from a file, a string, or an embedded Rust module.
///
/// An empty module occurs when there is no Python code for a module. This can occur for a few
/// reasons:
/// 1) Rust-backed module
/// 2) a module created as a layer in an import such as `import mypackage.mymodule`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Source {
    name: Option<String>,
    path: PathBuf,
    text: Option<String>,
}

impl Source {
    const DEFAULT_NAME: Dunder = Dunder::Main;
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

    pub fn from_path_and_name(module_name: &ModuleName, path: PathBuf) -> io::Result<Self> {
        let text = fs::read_to_string(&path)?;
        Ok(Self::with_named_path(&module_name.as_str(), path, text))
    }

    /// Provide code directly as a string without reading from the file system.
    pub fn from_text(text: &str) -> Self {
        Self {
            name: None,
            path: "<stdin>".into(),
            text: Some(text.to_string()),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn name(&self) -> &str {
        self.name.as_deref().unwrap_or(Self::DEFAULT_NAME.into())
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
            path,
            text: Some(text),
        }
    }

    fn with_path(path: PathBuf, text: String) -> Self {
        Self {
            name: None,
            path,
            text: Some(text),
        }
    }
}
