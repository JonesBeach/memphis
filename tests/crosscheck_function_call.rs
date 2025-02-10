use memphis::{
    crosscheck_utils::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter},
    ExecutionError, ExecutionErrorKind, MemphisError,
};

// TODO This proc macro is working, but I should really add tests in its own crate before too long.
// #[crosscheck::test]
// fn test_function_call(mut adapter: Adapter) {

fn assert_name_error(e: ExecutionError, expected_name: &str) {
    match e.execution_error_kind {
        ExecutionErrorKind::NameError(name) => {
            assert_eq!(name, expected_name, "Unexpected NameError message");
        }
        _ => panic!("Expected a NameError, but got {:?}", e.execution_error_kind),
    }
}

fn run_test<T: InterpreterTest>(mut adapter: T) {
    let input = r#"
def foo(a, b):
    return a + b

a = foo(2, 9)
"#;
    let _ = adapter.evaluate(input);
    assert_eq!(adapter.read("a"), Some(TestValue::Integer(11)));

    let input = r#"
def foo(a, b):
    c = 9
    return a + b + c

a = foo(2, 9)
"#;
    let _ = adapter.evaluate(input);
    assert_eq!(adapter.read("a"), Some(TestValue::Integer(20)));

    let input = r#"
def middle_call():
    last_call()

def last_call():
    unknown()

middle_call()
"#;
    match adapter.evaluate(input) {
        Err(MemphisError::Execution(e)) => {
            assert_name_error(e, "unknown");
        }
        _ => panic!("Expected an exception!"),
    }
}

#[test]
fn test_treewalk_function_call() {
    run_test(TreewalkAdapter::new());
}

#[test]
fn test_bytecode_vm_function_call() {
    run_test(BytecodeVmAdapter::new());
}
