use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    str,
};

use crate::{
    core::{log, Container, LogLevel},
    domain::{DebugStackFrame, Dunder, ToDebugStackFrame},
    parser::types::ImportPath,
};

#[cfg(feature = "stdlib")]
use super::stdlib::Stdlib;
#[cfg(feature = "c_stdlib")]
use super::types::cpython::CPythonModule;
use super::types::Module;

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
    const DEFAULT_PATH: &str = "<stdin>";
    const DEFAULT_TEXT: &str = "<module with no Python code>";

    pub fn new(name: &str, path: PathBuf, text: &str) -> Self {
        Self {
            name: Some(name.to_string()),
            path: Some(path),
            text: Some(text.to_string()),
        }
    }

    /// This exists for the unit tests where code is provided directly as a string without reading
    /// from the file system.
    pub fn new_virtual(text: &str) -> Self {
        Self {
            name: None,
            path: None,
            text: Some(text.to_string()),
        }
    }

    pub fn path(&self) -> &Path {
        self.path
            .as_deref()
            .unwrap_or(Path::new(Self::DEFAULT_PATH))
    }

    pub fn name(&self) -> &str {
        self.name.as_deref().unwrap_or(Self::DEFAULT_NAME.into())
    }

    pub fn text(&self) -> &str {
        self.text.as_deref().unwrap_or(Self::DEFAULT_TEXT)
    }
}

impl ToDebugStackFrame for ModuleSource {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new(self.name(), self.path().to_path_buf(), 1)
    }
}

pub struct ModuleLoader {
    /// The list of directories searched during each import. This will be seeded with the location
    /// of the Python stdlib present on the host system.
    paths: Vec<PathBuf>,

    /// A cache of the module after the file system but before parsing or evaluation. I'm not sure
    /// we need this long term, we may be able to defer to [`Self::cache`].
    fs_cache: HashMap<ImportPath, ModuleSource>,

    /// A list of the modules we failed to import. This is a small optimization to prevent extra
    /// trips to the file system for modules we know are not present. The Python stdlib does this
    /// type of pattern often:
    ///
    /// ```python
    /// try:
    ///     import _abc
    /// except ImportError:
    ///     ...
    ///
    /// try:
    ///     import _abc
    /// except ImportError:
    ///     ...
    ///
    /// ```
    not_found_cache: HashSet<ImportPath>,

    /// A cache of the module after evaluation.
    module_cache: HashMap<ImportPath, Container<Module>>,

    #[cfg(feature = "c_stdlib")]
    builtin_module_cache: HashMap<ImportPath, Container<CPythonModule>>,
}

impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            #[cfg(feature = "stdlib")]
            paths: Stdlib::init().paths().to_vec(),
            #[cfg(not(feature = "stdlib"))]
            paths: vec![],
            fs_cache: HashMap::default(),
            not_found_cache: HashSet::default(),
            module_cache: HashMap::default(),
            #[cfg(feature = "c_stdlib")]
            builtin_module_cache: HashMap::default(),
        }
    }

    /// Subsequent absolute imports will use the provided [`PathBuf`] to search for modules.
    pub fn register_root(&mut self, filepath: PathBuf) {
        let path = filepath
            .parent()
            .map_or_else(|| PathBuf::from("./"), |parent| parent.to_path_buf());

        // Insert at the start of the paths so this directory is searched first on subsequent
        // module imports
        self.paths.insert(0, path);
    }

    pub fn load_module_source(name: &str, filepath: PathBuf) -> Option<ModuleSource> {
        if let Ok(text) = fs::read_to_string(filepath.clone()) {
            log(LogLevel::Debug, || {
                format!("Loading: {}", filepath.display())
            });
            Some(ModuleSource::new(name, filepath, &text))
        } else {
            None
        }
    }

    /// This will look for a Python module in the following directories:
    /// 1) the directory of the root script
    /// 2) the /lib directory
    /// 3) the site_packages directory for the current python target
    fn load_absolute_path(
        name: &ImportPath,
        path_segments: &[String],
        paths: &[PathBuf],
    ) -> Option<ModuleSource> {
        paths
            .iter()
            .flat_map(|filepath| expand_path(filepath, path_segments))
            .find_map(|filepath| Self::load_module_source(&name.as_str(), filepath))
    }

    fn load_relative_path(
        name: &ImportPath,
        level: &usize,
        path_segments: &[String],
        current_path: &Path,
    ) -> Option<ModuleSource> {
        // The value in `current_path` contains the filename, so we must add 1 to the level to
        // get back to the directory.
        let base_path = up_n_levels(current_path, level + 1)?;

        expand_path(base_path, path_segments)
            .into_iter()
            .find_map(|filepath| Self::load_module_source(&name.as_str(), filepath))
    }

    #[cfg(feature = "c_stdlib")]
    pub fn import_builtin_module(&mut self, import_path: &ImportPath) -> Container<CPythonModule> {
        if let Some(module) = self.builtin_module_cache.get(&import_path) {
            module.clone()
        } else {
            let module = Container::new(CPythonModule::new(&import_path.as_str()));
            self.builtin_module_cache
                .insert(import_path.to_owned(), module.clone());
            module
        }
    }

    pub fn fetch_module(&mut self, import_path: &ImportPath) -> Option<Container<Module>> {
        self.module_cache.get(import_path).cloned()
    }

    pub fn store_module(&mut self, import_path: &ImportPath, module: Container<Module>) {
        self.module_cache.insert(import_path.to_owned(), module);
    }

    pub fn load_module(
        &mut self,
        import_path: &ImportPath,
        current_path: &PathBuf,
    ) -> Option<ModuleSource> {
        if let Some(code) = self.fs_cache.get(import_path) {
            return Some(code.clone());
        }
        if self.not_found_cache.contains(import_path) {
            return None;
        }

        log(LogLevel::Debug, || format!("Importing {}", import_path));

        let module = match import_path {
            ImportPath::Absolute(path_segments) => {
                Self::load_absolute_path(import_path, path_segments, &self.paths)
            }
            ImportPath::Relative(level, path_segments) => {
                Self::load_relative_path(import_path, level, path_segments, current_path)
            }
        };

        if let Some(input) = module {
            self.fs_cache.insert(import_path.to_owned(), input.clone());
            Some(input)
        } else {
            log(LogLevel::Debug, || {
                format!("Failed to import {} from {:?}", import_path, current_path)
            });
            self.not_found_cache.insert(import_path.to_owned());
            None
        }
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
