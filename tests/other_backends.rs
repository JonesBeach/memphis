use std::process::Command;

/// Compare the output of memphis running the Engine::BytecodeVm against python3.
fn run_script_and_compare(script: &'static str) {
    let output = Command::new("target/debug/memphis")
        .arg(script)
        .arg("vm")
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
mod bytecode_vm_tests {
    use super::*;

    fn ui_tests() -> Vec<&'static str> {
        vec!["examples/loop_perf.py"]
    }

    #[test]
    fn run_scripts() {
        for ui_test in ui_tests() {
            run_script_and_compare(ui_test);
        }
    }
}

#[cfg(feature = "llvm_backend")]
#[cfg(test)]
mod llvm_backend_tests {
    use super::*;

    /// Run memphis with the Engine::LlvmBackend engine and just confirm it doesn't fail.
    fn run_script(script: &'static str) {
        let output = Command::new("target/debug/memphis")
            .arg(script)
            .arg("llvm")
            .output()
            .expect("Failed to run test script");

        if !output.status.success() {
            panic!("Running script {} failed.", script);
        }
    }

    fn ui_tests() -> Vec<&'static str> {
        vec!["examples/loop_perf.py"]
    }

    #[test]
    fn run_scripts() {
        for ui_test in ui_tests() {
            run_script(ui_test);
        }
    }
}
