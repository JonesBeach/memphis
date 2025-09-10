use std::process::Command;

fn run_output(binary: &str, script: &'static str, engine: Option<&str>) -> String {
    let mut command = Command::new(binary);

    if let Some(engine_name) = engine {
        command.env("MEMPHIS_ENGINE", engine_name);
    }

    let output = command
        .arg(script)
        .output()
        .expect("Failed to run test script");

    if !output.status.success() {
        panic!("Running script {} failed.", script);
    }

    String::from_utf8_lossy(&output.stdout).to_string()
}

fn test_script(script: &'static str, engine: Option<&str>) {
    let output = run_output("target/debug/memphis", script, engine);
    let expected = run_output("python3", script, None);

    assert_eq!(
        output, expected,
        "Running script {} produced unexpected output.",
        script
    );
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    static TREEWALK_UI_TESTS: [&'static str; 11] = [
        "examples/test.py",
        "examples/async/a.py",
        "examples/async/b.py",
        "examples/async/c.py",
        "examples/async/d.py",
        "examples/async/simple.py",
        "examples/exceptions.py",
        "examples/context_manager.py",
        "examples/new_method.py",
        "examples/builtins.py",
        "examples/descriptor_protocol.py",
    ];

    static BYTECODE_VM_UI_TESTS: [&'static str; 2] =
        ["examples/test_vm.py", "examples/async/simple.py"];

    #[test]
    fn run_treewalk_scripts() {
        for ui_test in TREEWALK_UI_TESTS {
            test_script(ui_test, None);
        }
    }

    #[test]
    fn run_bytecode_vm_scripts() {
        for ui_test in BYTECODE_VM_UI_TESTS {
            test_script(ui_test, Some("bytecode_vm"));
        }
    }
}
