use std::io::{Error, ErrorKind, Result};
use std::process::{Command, Stdio};

fn run(cmd: &mut Command) -> Result<bool> {
    let status = cmd.spawn()?.wait()?;
    if !status.success() {
        return Err(Error::new(ErrorKind::Other, "Command failed"));
    }

    Ok(status.success())
}

fn run_fmt() -> Result<bool> {
    let mut cargo = Command::new("cargo");
    let cmd = cargo.arg("fmt").arg("--check").stdout(Stdio::piped());
    run(cmd)
}

fn run_clippy() -> Result<bool> {
    let mut cargo = Command::new("cargo");
    let cmd = cargo
        .arg("clippy")
        .arg("--all-features")
        .env("RUSTFLAGS", "-D warnings")
        .stdout(Stdio::piped());

    run(cmd)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clippy() {
        let success = run_clippy().expect("clippy run failed");
        assert_eq!(success, true);
    }

    #[test]
    fn fmt() {
        let success = run_fmt().expect("fmt run failed");
        assert_eq!(success, true);
    }
}
