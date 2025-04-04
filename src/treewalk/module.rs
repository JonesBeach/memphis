use std::collections::HashMap;

use crate::{core::Container, parser::types::ImportPath, treewalk::types::Module};

pub struct EvaluatedModuleCache {
    /// A cache of the module after evaluation.
    module_cache: HashMap<ImportPath, Container<Module>>,
}

impl EvaluatedModuleCache {
    pub fn new() -> Self {
        Self {
            module_cache: HashMap::default(),
        }
    }

    pub fn fetch_module(&mut self, import_path: &ImportPath) -> Option<Container<Module>> {
        self.module_cache.get(import_path).cloned()
    }

    pub fn store_module(&mut self, import_path: &ImportPath, module: Container<Module>) {
        self.module_cache.insert(import_path.to_owned(), module);
    }
}

pub mod module_loader {
    use std::{
        fs,
        path::{Path, PathBuf},
    };

    use crate::{
        core::{log, LogLevel},
        domain::{Dunder, Source},
        parser::types::ImportPath,
    };

    pub fn load_root_source(filepath: &Path) -> Option<Source> {
        let source = read_source(filepath)?;
        let absolute_path = filepath.canonicalize().ok()?;
        Some(Source::from_named_path(absolute_path, source))
    }

    pub fn load_source(
        import_path: &ImportPath,
        current_path: &Path,
        search_paths: &[PathBuf],
    ) -> Option<Source> {
        log(LogLevel::Debug, || format!("Importing {}", import_path));
        let resolved_path = resolve_path(import_path, current_path, search_paths)?;
        let source = read_source(&resolved_path);

        if let Some(source) = source {
            Some(Source::from_path(
                &import_path.as_str(),
                resolved_path,
                source,
            ))
        } else {
            log(LogLevel::Debug, || {
                format!("Failed to import {} from {:?}", import_path, current_path)
            });
            None
        }
    }

    /// This will look for a Python module from the provided paths list.
    fn resolve_absolute_path(
        path_segments: &[String],
        search_paths: &[PathBuf],
    ) -> Option<PathBuf> {
        search_paths
            .iter()
            .flat_map(|filepath| expand_path(filepath, path_segments))
            .find(|filepath| filepath.exists())
    }

    fn resolve_relative_path(
        level: &usize,
        path_segments: &[String],
        current_path: &Path,
    ) -> Option<PathBuf> {
        // The value in `current_path` contains the filename, so we must add 1 to the level to
        // get back to the directory.
        let base_path = up_n_levels(current_path, level + 1)?;

        expand_path(base_path, path_segments)
            .into_iter()
            .find(|filepath| filepath.exists())
    }

    /// Finds a module but does not read it (returns absolute path).
    fn resolve_path(
        import_path: &ImportPath,
        current_path: &Path,
        search_paths: &[PathBuf],
    ) -> Option<PathBuf> {
        let resolved_path = match import_path {
            ImportPath::Absolute(path_segments) => {
                resolve_absolute_path(path_segments, search_paths)
            }
            ImportPath::Relative(level, path_segments) => {
                resolve_relative_path(level, path_segments, current_path)
            }
        }?;

        Some(
            resolved_path
                .canonicalize()
                .expect("Failed to turn into absolute path"),
        )
    }

    fn read_source(filepath: &Path) -> Option<String> {
        if let Ok(text) = fs::read_to_string(filepath) {
            log(LogLevel::Debug, || {
                format!("Loading: {}", filepath.display())
            });
            Some(text)
        } else {
            None
        }
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
            .join(format!("{}.py", last));

        // Build the `../base/segment_one/segment_two/__init__.py` path
        let init_path = segments
            .iter()
            .fold(path.to_path_buf(), append_segment)
            .join(format!("{}.py", Dunder::Init));

        [base_path, init_path]
    }

    fn up_n_levels(path: &Path, n: usize) -> Option<&Path> {
        (0..n).try_fold(path, |current, _| current.parent())
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_expand_path_with_multiple_segments() {
            let path = Path::new("/base");
            let segments = vec!["subdir".to_string(), "file".to_string()];

            let [base_path, init_path] = expand_path(path, &segments);

            assert_eq!(base_path, Path::new("/base/subdir/file.py"));
            assert_eq!(init_path, Path::new("/base/subdir/file/__init__.py"));
        }

        #[test]
        fn test_expand_path_with_single_segment() {
            let path = Path::new("/base");
            let segments = vec!["file".to_string()];

            let [base_path, init_path] = expand_path(path, &segments);

            assert_eq!(base_path, Path::new("/base/file.py"));
            assert_eq!(init_path, Path::new("/base/file/__init__.py"));
        }

        #[test]
        #[should_panic]
        fn test_expand_path_with_empty_segments() {
            let path = Path::new("/base");
            let segments: Vec<String> = vec![];

            let _ = expand_path(path, &segments);
        }
    }
}
