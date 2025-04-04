use std::{
    io::{self, Error, ErrorKind},
    path::PathBuf,
    process::Command,
    str,
};

/// A holder for the paths we can later search for stdlib modules written in Python.
pub struct Stdlib {
    paths: Vec<PathBuf>,
}

impl Stdlib {
    pub fn load_from_host() -> Self {
        // The location of any "standard-lib" modules we add ourselves. This refers to the lib
        // directory of this repository.
        let mut paths = vec![PathBuf::from("./lib".to_string())];

        // This is the location of packages installed by pip, i.e. pendulum.
        // TODO can we get rid of this in favor of sys.path below?
        let mut site_packages =
            run_in_python("import site; print('\\n'.join(site.getsitepackages()))")
                .expect("Failed to get site packages path");
        paths.append(&mut site_packages);

        // This seems to have some overlap with the site-packages above, yet it contains the full set
        // of paths including standard lib items, i.e. argparse.
        let mut sys_path = run_in_python("import sys; print('\\n'.join(sys.path))")
            .expect("Failed to get sys path");
        paths.append(&mut sys_path);

        Self { paths }
    }

    pub fn paths(&self) -> &[PathBuf] {
        &self.paths
    }
}

/// Run a provided command on Python on the host machine.
fn run_in_python(command: &str) -> io::Result<Vec<PathBuf>> {
    let output = Command::new("python3").args(["-c", command]).output()?;

    if !output.status.success() {
        return Err(Error::new(ErrorKind::Other, "Failed to run command"));
    }

    let output_str =
        str::from_utf8(&output.stdout).map_err(|e| Error::new(ErrorKind::InvalidData, e))?;

    Ok(output_str.lines().map(PathBuf::from).collect())
}
