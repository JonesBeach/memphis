use std::path::{Path, PathBuf};

#[cfg(feature = "stdlib")]
use super::stdlib::Stdlib;

/// A store of directories searched during each import. This will be seeded with the location
/// of the Python stdlib present on the host system.
#[derive(Default)]
pub struct ImportResolver {
    search_paths: Vec<PathBuf>,
}

impl ImportResolver {
    pub fn new() -> Self {
        Self {
            #[cfg(feature = "stdlib")]
            search_paths: Stdlib::load_from_host().paths().to_vec(),
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
