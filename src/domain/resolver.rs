use std::path::{Path, PathBuf};

use crate::domain::{Dunder, ImportPath, ModuleName, ModulePath};

pub enum ImportResolutionError {
    NoParentPackage,
    BeyondTopLevel,
}

pub type ImportResult<T> = Result<T, ImportResolutionError>;

impl ImportResolutionError {
    pub fn message(&self) -> &'static str {
        match self {
            Self::NoParentPackage => "attempted relative import with no known parent package",
            Self::BeyondTopLevel => "attempted relative import beyond top-level package",
        }
    }
}

pub fn resolve_import_path(
    import_path: &ImportPath,
    current_module: &ModuleName,
) -> ImportResult<ModuleName> {
    match import_path {
        ImportPath::Absolute(mp) => Ok(resolve_absolute_path(mp)),
        ImportPath::Relative(levels, tail) => {
            if current_module.is_main() {
                return Err(ImportResolutionError::NoParentPackage);
            }
            let base = current_module
                .strip_last(*levels)
                .ok_or(ImportResolutionError::BeyondTopLevel)?;
            Ok(base.join(tail.segments()))
        }
    }
}

// Convert from a parser `ModulePath` into a runtime `ModuleName`. For absolute paths, this is a
// direct mapping.
pub fn resolve_absolute_path(module_path: &ModulePath) -> ModuleName {
    ModuleName::from_segments(module_path.segments())
}

/// Finds a module but does not read it (returns absolute path).
pub fn resolve(module_name: &ModuleName, search_paths: &[PathBuf]) -> Option<PathBuf> {
    let path = search_paths
        .iter()
        .flat_map(|filepath| expand_path(filepath, module_name.segments()))
        .find(|filepath| filepath.exists())?;

    path.canonicalize().ok()
}

/// For a given path and segments, this returns both the `../base.py` and `../base/__init__.py`
/// versions.
fn expand_path(path: &Path, segments: &[String]) -> [PathBuf; 2] {
    // Split the slice into the last segment and the rest
    let (last, rest) = match segments.split_last() {
        Some((last, rest)) => (last, rest),
        None => panic!("Path segments must not be empty!"),
    };

    let append_segment = |mut acc: PathBuf, segment: &String| {
        acc.push(segment);
        acc
    };

    // Build the `../base/segment_one/segment_two.py` path
    let base_path = rest
        .iter()
        .fold(path.to_path_buf(), append_segment)
        .join(format!("{last}.py"));

    // Build the `../base/segment_one/segment_two/__init__.py` path
    let init_path = segments
        .iter()
        .fold(path.to_path_buf(), append_segment)
        .join(format!("{}.py", Dunder::Init));

    [base_path, init_path]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_path_with_multiple_segments() {
        let path = Path::new("/base");
        let segments = ["subdir".to_string(), "file".to_string()];

        let [base_path, init_path] = expand_path(path, &segments);

        assert_eq!(base_path, Path::new("/base/subdir/file.py"));
        assert_eq!(init_path, Path::new("/base/subdir/file/__init__.py"));
    }

    #[test]
    fn test_expand_path_with_single_segment() {
        let path = Path::new("/base");
        let segments = ["file".to_string()];

        let [base_path, init_path] = expand_path(path, &segments);

        assert_eq!(base_path, Path::new("/base/file.py"));
        assert_eq!(init_path, Path::new("/base/file/__init__.py"));
    }

    #[test]
    #[should_panic]
    fn test_expand_path_with_empty_segments() {
        let path = Path::new("/base");
        let segments = [];

        let _ = expand_path(path, &segments);
    }
}
