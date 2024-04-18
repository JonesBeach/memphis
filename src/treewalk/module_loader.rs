use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io::{self, ErrorKind};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str;

use crate::core::{log, Container, LogLevel};
use crate::parser::types::ImportPath;

#[cfg(feature = "c_stdlib")]
use super::types::cpython::CPythonModule;
use super::types::{utils::Dunder, Module};

fn lookup_python_site_packages(command: &str) -> Vec<PathBuf> {
    let output = Command::new("python3")
        .args(["-c", command])
        .output()
        .expect("Failed to retrieve Python site-packages path");

    if !output.status.success() {
        panic!("Failed to retrieve Python site-packages path");
    }

    let output_str = str::from_utf8(&output.stdout)
        .map_err(|e| io::Error::new(ErrorKind::InvalidData, e))
        .expect("Failed to retrieve Python site-packages path");

    output_str.lines().map(PathBuf::from).collect()
}

fn init_paths() -> Vec<PathBuf> {
    // The location of any "standard-lib" modules we add ourselves. This refers to the lib
    // directory of this repository.
    let mut paths = vec![PathBuf::from("./lib".to_string())];

    // This is the location of packages installed by pip, i.e. pendulum.
    // TODO can we get rid of this in favor of sys.path below?
    let mut site_packages =
        lookup_python_site_packages("import site; print('\\n'.join(site.getsitepackages()))");
    paths.append(&mut site_packages);

    // This seems to have some overlap with the site-packages above, yet it contains the full set
    // of paths including standard lib items, i.e. argparse.
    let mut sys_path = lookup_python_site_packages("import sys; print('\\n'.join(sys.path))");
    paths.append(&mut sys_path);
    paths
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoadedModule {
    name: Option<String>,
    path: Option<PathBuf>,
    text: Option<String>,
}

impl LoadedModule {
    pub fn new(name: &str, path: PathBuf, text: String) -> Self {
        Self {
            name: Some(name.to_string()),
            path: Some(path),
            text: Some(text),
        }
    }

    /// An empty module occurs when there is no Python code for a module. This can occur for a few
    /// reasons:
    /// 1) Rust-backed module
    /// 2) a module created as a layer in an import such as `import mypackage.mymodule`.
    pub fn empty() -> Self {
        Self {
            name: None,
            path: None,
            text: None,
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

    pub fn empty_path() -> PathBuf {
        "<stdin>".to_string().into()
    }

    pub fn path(&self) -> PathBuf {
        self.path.clone().unwrap_or(Self::empty_path())
    }

    pub fn empty_name() -> String {
        Dunder::Main.into()
    }

    pub fn name(&self) -> String {
        self.name.clone().unwrap_or(Self::empty_name())
    }

    pub fn text(&self) -> Option<String> {
        self.text.clone()
    }
}

pub struct ModuleLoader {
    /// The [`PathBuf`] representing the directory from which memphis was invoked.
    run_dir: PathBuf,

    /// The list of directories searched during each import. This will be seeded with the location
    /// of the Python stdlib present on the host system.
    paths: Vec<PathBuf>,

    /// A cache of the module after the file system but before parsing or evaluation. I'm not sure
    /// we need this long term, we may be able to defer to [`Self::cache`].
    fs_cache: HashMap<ImportPath, LoadedModule>,

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
        let run_dir = env::current_dir().expect("Failed to get current directory");

        Self {
            run_dir,
            paths: init_paths(),
            fs_cache: HashMap::default(),
            not_found_cache: HashSet::default(),
            module_cache: HashMap::default(),
            #[cfg(feature = "c_stdlib")]
            builtin_module_cache: HashMap::default(),
        }
    }

    pub fn load_root(&mut self, filepath: PathBuf) -> Option<LoadedModule> {
        let path = filepath
            .parent()
            .map_or_else(|| PathBuf::from("./"), |parent| parent.to_path_buf());

        // Insert at the start of the paths so this directory is searched first on subsequent
        // module imports
        self.paths.insert(0, path);
        self.load_module_code("<module>", filepath)
    }

    fn load_module_code(&self, name: &str, filepath: PathBuf) -> Option<LoadedModule> {
        if let Ok(text) = fs::read_to_string(filepath.clone()) {
            log(LogLevel::Debug, || {
                format!("Loading: {}", filepath.display())
            });
            Some(LoadedModule::new(name, self.run_dir.join(filepath), text))
        } else {
            None
        }
    }

    /// This will look for a Python module in the following directories:
    /// 1) the directory of the root script
    /// 2) the /lib directory
    /// 3) the site_packages directory for the current python target
    pub fn load_absolute_path(
        &mut self,
        name: &ImportPath,
        path_segments: &Vec<String>,
    ) -> Option<LoadedModule> {
        self.paths
            .iter()
            .flat_map(|path| expand_path(path, path_segments))
            .find_map(|filename| self.load_module_code(&name.as_str(), filename))
    }

    pub fn load_relative_path(
        &mut self,
        name: &ImportPath,
        level: &usize,
        path_segments: &Vec<String>,
        current_path: Option<PathBuf>,
    ) -> Option<LoadedModule> {
        let base_path = match current_path {
            // The value in `current_path` contains the filename, so we must add 1 to the level to
            // get back to the directory. We could change this in the future, but this seemed
            // cleaner for the caller to provide.
            Some(p) => up_n_levels(&p, &(level + 1)),
            None => up_n_levels(&self.run_dir, level),
        };

        expand_path(base_path.as_ref()?, path_segments)
            .into_iter()
            .find_map(|filename| self.load_module_code(&name.as_str(), filename))
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
        current_path: Option<PathBuf>,
    ) -> Option<LoadedModule> {
        if let Some(code) = self.fs_cache.get(import_path) {
            return Some(code.clone());
        }
        if self.not_found_cache.contains(import_path) {
            return None;
        }

        log(LogLevel::Debug, || format!("Importing {}", import_path));

        let module = match import_path {
            ImportPath::Absolute(path_segments) => {
                self.load_absolute_path(import_path, path_segments)
            }
            ImportPath::Relative(level, path_segments) => {
                self.load_relative_path(import_path, level, path_segments, current_path.clone())
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
fn expand_path(path: &Path, path_segments: &Vec<String>) -> Vec<PathBuf> {
    let mut normal_path = path.to_path_buf();
    for (index, value) in path_segments.iter().enumerate() {
        if index == path_segments.len() - 1 {
            normal_path.push(value.to_owned() + ".py");
        } else {
            normal_path.push(value);
        }
    }

    let mut init_path = path.to_path_buf();
    for value in path_segments {
        init_path.push(value);
    }
    init_path.push(format!("{}.py", Dunder::Init));

    vec![normal_path, init_path]
}

fn up_n_levels(original: &Path, n: &usize) -> Option<PathBuf> {
    let mut path = original.to_path_buf();
    for _ in 0..*n {
        match path.parent() {
            Some(parent_path) => {
                path = parent_path.to_path_buf();
            }
            None => return None,
        }
    }
    Some(path)
}
