use std::io::{Error, ErrorKind, Result};
use std::process::{Command, Stdio};

fn run_repl_with_pipe() -> Result<bool> {
    let cat_cmd = Command::new("cat")
        .arg("examples/repl.py")
        .stdout(Stdio::piped())
        .spawn()?;

    let mut memphis_cmd = Command::new("target/debug/memphis")
        .stdin(cat_cmd.stdout.unwrap())
        .stdout(Stdio::piped())
        .spawn()?;

    let status = memphis_cmd.wait()?;
    if !status.success() {
        return Err(Error::new(ErrorKind::Other, "Command failed"));
    }

    Ok(status.success())
}

#[cfg(test)]
mod repl_tests {
    use super::*;

    #[test]
    fn run_repl() {
        let success = run_repl_with_pipe().unwrap();
        assert_eq!(success, true);
    }
}
