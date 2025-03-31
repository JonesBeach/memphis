use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    str,
};

use crate::{
    core::Container,
    domain::{DebugStackFrame, Dunder, ToDebugStackFrame},
    parser::types::ImportPath,
    treewalk::types::Module,
};

#[cfg(feature = "stdlib")]
use super::stdlib::Stdlib;

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleSource {
    name: Option<String>,
    path: Option<PathBuf>,
    text: Option<String>,
}

impl Default for ModuleSource {
    /// An empty module occurs when there is no Python code for a module. This can occur for a few
    /// reasons:
    /// 1) Rust-backed module
    /// 2) a module created as a layer in an import such as `import mypackage.mymodule`.
    fn default() -> Self {
        Self {
            name: None,
            path: None,
            text: None,
        }
    }
}

impl ModuleSource {
    const DEFAULT_NAME: Dunder = Dunder::Main;
    const DEFAULT_CONTEXT: &str = "<module>";
    const DEFAULT_PATH: &str = "<stdin>";
    const DEFAULT_TEXT: &str = "<module with no Python code>";

    pub fn new(name: &str, path: PathBuf, text: String) -> Self {
        Self {
            name: Some(name.to_string()),
            path: Some(path),
            text: Some(text),
        }
    }

    pub fn from_root(path: PathBuf, text: String) -> Self {
        Self {
            name: None,
            path: Some(path),
            text: Some(text),
        }
    }

    /// This exists for the unit tests where code is provided directly as a string without reading
    /// from the file system.
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

impl ToDebugStackFrame for ModuleSource {
    fn to_stack_frame(&self) -> DebugStackFrame {
        let name = self.name.as_deref().unwrap_or("<module>");
        DebugStackFrame::new(name, self.display_path().to_path_buf(), 1)
    }
}

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

pub struct ModuleContext {
    /// The list of directories searched during each import. This will be seeded with the location
    /// of the Python stdlib present on the host system.
    search_paths: Vec<PathBuf>,
}

impl ModuleContext {
    pub fn new() -> Self {
        Self {
            #[cfg(feature = "stdlib")]
            search_paths: Stdlib::init().paths().to_vec(),
            #[cfg(not(feature = "stdlib"))]
            search_paths: vec![],
        }
    }

    /// Subsequent absolute imports will use the provided [`PathBuf`] to search for modules.
    pub fn register_root(&mut self, path: &Path) {
        let path = path
            .parent()
            .map_or_else(|| PathBuf::from("./"), |parent| parent.to_path_buf());

        // Insert at the start of the paths so this directory is searched first on subsequent
        // module imports
        // Also, avoid duplicate paths
        if !self.search_paths.contains(&path) {
            self.search_paths.insert(0, path);
        }
    }

    pub fn search_paths(&self) -> &[PathBuf] {
        &self.search_paths
    }
}

pub mod module_loader {
    use std::{
        fs,
        path::{Path, PathBuf},
    };

    use crate::{
        core::{log, LogLevel},
        domain::Dunder,
        parser::types::ImportPath,
    };

    use super::ModuleSource;

    pub fn load_root_module_source(filepath: &Path) -> Option<ModuleSource> {
        let source = read_source(filepath)?;
        let absolute_path = filepath.canonicalize().ok()?;
        Some(ModuleSource::from_root(absolute_path, source))
    }

    pub fn load_module_source(
        import_path: &ImportPath,
        current_path: &Path,
        search_paths: &[PathBuf],
    ) -> Option<ModuleSource> {
        log(LogLevel::Debug, || format!("Importing {}", import_path));
        let resolved_path = resolve_path(import_path, current_path, search_paths)?;
        let source = read_source(&resolved_path);

        if let Some(source) = source {
            Some(ModuleSource::new(
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
