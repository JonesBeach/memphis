/// A raw module path exactly as written by the user.
/// No validation, no resolution, no filesystem meaning.
/// Used only during parsing.
#[derive(Default, Debug, PartialEq, Clone, Eq, Hash)]
pub struct ModulePath(Vec<String>);

impl ModulePath {
    pub fn new(segments: Vec<String>) -> Self {
        Self(segments)
    }

    pub fn as_str(&self) -> String {
        self.0.join(".")
    }

    pub fn segments(&self) -> &[String] {
        &self.0
    }

    pub fn head(&self) -> Option<&str> {
        self.0.first().map(|s| s.as_str())
    }

    #[cfg(test)]
    pub fn from(path_str: &str) -> Self {
        let segments = if path_str.is_empty() {
            vec![]
        } else {
            path_str.split('.').map(|s| s.to_string()).collect()
        };
        ModulePath(segments)
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum ImportPath {
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

impl ImportPath {
    pub fn as_str(&self) -> String {
        match self {
            ImportPath::Absolute(path) => path.as_str(),
            ImportPath::Relative(levels, path) => ".".repeat(*levels) + &path.as_str(),
        }
    }

    pub fn head(&self) -> Option<&str> {
        match self {
            ImportPath::Absolute(path) | ImportPath::Relative(_, path) => path.head(),
        }
    }

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
            ImportPath::Absolute(path)
        } else {
            ImportPath::Relative(dot_count, path)
        }
    }
}

#[cfg(test)]
mod test_module_path {
    use super::*;

    #[test]
    fn module_path_as_str() {
        let p = ModulePath::new(vec!["a".into(), "b".into()]);
        assert_eq!(p.as_str(), "a.b");
    }

    #[test]
    fn module_path_head() {
        let p = ModulePath::new(vec!["a".into(), "b".into()]);
        assert_eq!(p.head(), Some("a"));
    }
}

#[cfg(test)]
mod test_import_path {
    use super::*;

    #[test]
    fn parses_absolute_single() {
        let path = ImportPath::from("os");
        assert_eq!(path, ImportPath::Absolute(ModulePath::from("os")));
    }

    #[test]
    fn parses_absolute_nested() {
        let path = ImportPath::from("package.module");
        assert_eq!(
            path,
            ImportPath::Absolute(ModulePath::from("package.module"))
        );
    }

    #[test]
    fn parses_relative_same_dir() {
        let path = ImportPath::from(".package.module");
        assert_eq!(
            path,
            ImportPath::Relative(1, ModulePath::from("package.module"))
        );
    }

    #[test]
    fn parses_relative_parent_dir() {
        let path = ImportPath::from("..package.module");
        assert_eq!(
            path,
            ImportPath::Relative(2, ModulePath::from("package.module"))
        );
    }

    #[test]
    fn parses_relative_only_dots() {
        let path = ImportPath::from(".");
        assert_eq!(path, ImportPath::Relative(1, ModulePath::default()));

        let path = ImportPath::from("..");
        assert_eq!(path, ImportPath::Relative(2, ModulePath::default()));
    }

    #[test]
    fn parses_empty_path_as_absolute_empty() {
        let path = ImportPath::from("");
        assert_eq!(path, ImportPath::Absolute(ModulePath::default()));
    }
}
