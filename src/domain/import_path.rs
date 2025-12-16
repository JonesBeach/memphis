use crate::domain::Identifier;

/// A raw module path exactly as written by the user.
/// No validation, no resolution, no filesystem meaning.
/// Used only during parsing.
#[derive(Default, Debug, PartialEq, Clone, Eq, Hash)]
pub struct ModulePath(Vec<Identifier>);

impl ModulePath {
    pub fn new(segments: Vec<Identifier>) -> Self {
        Self(segments)
    }

    pub fn as_str(&self) -> String {
        self.segments_as_str().join(".")
    }

    pub fn segments(&self) -> &[Identifier] {
        &self.0
    }

    pub fn segments_as_str(&self) -> Vec<String> {
        self.0.iter().map(|s| s.to_string()).collect::<Vec<_>>()
    }

    pub fn head(&self) -> Option<&str> {
        self.0.first().map(|s| s.as_str())
    }

    #[cfg(test)]
    pub fn from(path_str: &str) -> Self {
        let segments = if path_str.is_empty() {
            vec![]
        } else {
            path_str
                .split('.')
                .map(|s| Identifier::new(s).expect("Invalid identifier"))
                .collect()
        };
        ModulePath(segments)
    }
}

/// Parser representation of the module path that is found in selective import statements.
/// This specifically holds inside the brackets:
/// from [a.b.c] import *
/// from [.a.b] import *
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum FromImportPath {
    /// When an import begins with no dots, it is an absolute import and this just wraps a
    /// ModulePath.
    Absolute(ModulePath),
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
    Relative(usize, ModulePath),
}

impl FromImportPath {
    #[cfg(test)]
    pub fn from(path_str: &str) -> Self {
        // Count leading dots (if any)
        let mut dot_count = 0;
        for c in path_str.chars() {
            if c == '.' {
                dot_count += 1;
            } else {
                break;
            }
        }

        // Split remaining text by '.'
        let rest = &path_str[dot_count..];
        let path = ModulePath::from(rest);

        if dot_count == 0 {
            FromImportPath::Absolute(path)
        } else {
            FromImportPath::Relative(dot_count, path)
        }
    }
}

#[cfg(test)]
mod test_module_path {
    use super::*;

    #[test]
    fn module_path_as_str() {
        let p = ModulePath::from("a.b");
        assert_eq!(p.as_str(), "a.b");
    }

    #[test]
    fn module_path_head() {
        let p = ModulePath::from("a.b");
        assert_eq!(p.head(), Some("a"));
    }
}

#[cfg(test)]
mod test_import_path {
    use super::*;

    #[test]
    fn parses_absolute_single() {
        let path = FromImportPath::from("os");
        assert_eq!(path, FromImportPath::Absolute(ModulePath::from("os")));
    }

    #[test]
    fn parses_absolute_nested() {
        let path = FromImportPath::from("package.module");
        assert_eq!(
            path,
            FromImportPath::Absolute(ModulePath::from("package.module"))
        );
    }

    #[test]
    fn parses_relative_same_dir() {
        let path = FromImportPath::from(".package.module");
        assert_eq!(
            path,
            FromImportPath::Relative(1, ModulePath::from("package.module"))
        );
    }

    #[test]
    fn parses_relative_parent_dir() {
        let path = FromImportPath::from("..package.module");
        assert_eq!(
            path,
            FromImportPath::Relative(2, ModulePath::from("package.module"))
        );
    }

    #[test]
    fn parses_relative_only_dots() {
        let path = FromImportPath::from(".");
        assert_eq!(path, FromImportPath::Relative(1, ModulePath::default()));

        let path = FromImportPath::from("..");
        assert_eq!(path, FromImportPath::Relative(2, ModulePath::default()));
    }

    #[test]
    fn parses_empty_path_as_absolute_empty() {
        let path = FromImportPath::from("");
        assert_eq!(path, FromImportPath::Absolute(ModulePath::default()));
    }
}
