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

    pub fn from_segments(segments: &[String]) -> Self {
        ImportPath::Absolute(segments.to_vec())
    }

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
        let segments: Vec<String> = if rest.is_empty() {
            Vec::new()
        } else {
            rest.split('.').map(|s| s.to_string()).collect()
        };

        if dot_count == 0 {
            ImportPath::Absolute(segments)
        } else {
            ImportPath::Relative(dot_count, segments)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_absolute_single() {
        let path = ImportPath::from("os");
        assert_eq!(path, ImportPath::Absolute(vec!["os".to_string()]));
    }

    #[test]
    fn parses_absolute_nested() {
        let path = ImportPath::from("package.module");
        assert_eq!(
            path,
            ImportPath::Absolute(vec!["package".into(), "module".into()])
        );
    }

    #[test]
    fn parses_relative_same_dir() {
        let path = ImportPath::from(".package.module");
        assert_eq!(
            path,
            ImportPath::Relative(1, vec!["package".into(), "module".into()])
        );
    }

    #[test]
    fn parses_relative_parent_dir() {
        let path = ImportPath::from("..package.module");
        assert_eq!(
            path,
            ImportPath::Relative(2, vec!["package".into(), "module".into()])
        );
    }

    #[test]
    fn parses_relative_only_dots() {
        let path = ImportPath::from("..");
        assert_eq!(path, ImportPath::Relative(2, vec![]));
    }

    #[test]
    fn parses_empty_path_as_absolute_empty() {
        let path = ImportPath::from("");
        assert_eq!(path, ImportPath::Absolute(vec![]));
    }
}
