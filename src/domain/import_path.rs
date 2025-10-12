use std::fmt::{Display, Error, Formatter};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum ImportPath {
    Absolute(Vec<String>),
    /// Any import path which _starts_ with a dot. The `usize` represents the number of levels up
    /// to look for the path. This means that this example is in the same directory.
    ///
    /// ```python
    /// from .package.module import symbol
    /// ```
    ///
    /// While this would look one directory up
    ///
    /// ```python
    /// from ..package.module import symbol
    /// ```
    Relative(usize, Vec<String>),
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.as_str())
    }
}

impl ImportPath {
    pub fn as_str(&self) -> String {
        match self {
            ImportPath::Absolute(path) => path.join("."),
            ImportPath::Relative(levels, path) => ".".repeat(*levels) + &path.join("."),
        }
    }

    pub fn segments(&self) -> &[String] {
        match self {
            ImportPath::Absolute(path) => path,
            ImportPath::Relative(_, path) => path,
        }
    }
}
