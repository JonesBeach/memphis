use std::process::Command;

fn test_script(script: &'static str) {
    let output = Command::new("target/debug/memphis")
        .arg(script)
        .output()
        .expect("Failed to run test script");

    if !output.status.success() {
        panic!("Running script {} failed.", script);
    }

    let expected = Command::new("python3")
        .arg(script)
        .output()
        .expect("Failed to run test script");

    if !expected.status.success() {
        panic!("Running script {} failed.", script);
    }

    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&expected.stdout),
        "Running script {} produced unexpected output.",
        script
    );
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    fn ui_tests() -> Vec<&'static str> {
        vec![
            "examples/test.py",
            "examples/async/a.py",
            "examples/async/b.py",
            "examples/async/c.py",
            "examples/async/d.py",
            "examples/exceptions.py",
            "examples/context_manager.py",
            "examples/new_method.py",
            "examples/builtins.py",
            "examples/descriptor_protocol.py",
        ]
    }

    #[test]
    fn run_scripts() {
        for ui_test in ui_tests() {
            test_script(ui_test);
        }
    }
}
