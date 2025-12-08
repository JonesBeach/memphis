use std::{
    io,
    path::{Path, PathBuf},
};

/// Represents a Python source, whether it comes from a file, a string, or an embedded Rust module.
///
/// An empty module occurs when there is no Python code for a module. This can occur for a few
/// reasons:
/// 1) Rust-backed module
/// 2) a module created as a layer in an import such as `import mypackage.mymodule`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Source {
    path: PathBuf,
    text: String,
}

impl Source {
    pub fn from_path<P>(filepath: P) -> io::Result<Self>
    where
        P: AsRef<Path>,
    {
        let text = std::fs::read_to_string(&filepath)?;
        let absolute_path = filepath.as_ref().canonicalize()?;
        Ok(Self::with_path(absolute_path, text))
    }

    /// Provide code directly as a string without reading from the file system.
    pub fn from_text(text: &str) -> Self {
        Self {
            path: "<stdin>".into(),
            text: text.to_string(),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    fn with_path(path: PathBuf, text: String) -> Self {
        Self { path, text }
    }
}
