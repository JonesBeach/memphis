use memphis::{
    crosscheck_utils::{BytecodeVmAdapter, InterpreterTest, TreewalkAdapter},
    domain::{test_utils, MemphisValue},
    MemphisError,
};

// TODO This proc macro is working, but I should really add tests in its own crate before too long.
// #[crosscheck::test]
// fn test_function_call(mut adapter: Adapter) {

fn run_test<T: InterpreterTest>(mut adapter: T) {
    let input = r#"
def foo(a, b):
    return a + b

a = foo(2, 9)
"#;
    let _ = adapter.evaluate(input);
    assert_eq!(adapter.read("a"), Some(MemphisValue::Integer(11)));

    let input = r#"
def foo(a, b):
    c = 9
    return a + b + c

a = foo(2, 9)
"#;
    let _ = adapter.evaluate(input);
    assert_eq!(adapter.read("a"), Some(MemphisValue::Integer(20)));

    let input = r#"
def middle_call():
    last_call()

def last_call():
    unknown()

middle_call()
"#;

    match adapter.evaluate(input) {
        Err(MemphisError::Execution(e)) => {
            test_utils::assert_name_error(&e, "unknown");
            assert_eq!(e.debug_call_stack.len(), 3);
            assert_eq!(e.debug_call_stack.get(0).name(), "<module>");
            assert_eq!(e.debug_call_stack.get(0).file_path_str(), "<stdin>");
            assert_eq!(e.debug_call_stack.get(0).line_number(), 8);
            assert_eq!(e.debug_call_stack.get(1).name(), "middle_call");
            assert_eq!(e.debug_call_stack.get(1).file_path_str(), "<stdin>");
            assert_eq!(e.debug_call_stack.get(1).line_number(), 3);
            assert_eq!(e.debug_call_stack.get(2).name(), "last_call");
            assert_eq!(e.debug_call_stack.get(2).file_path_str(), "<stdin>");
            assert_eq!(e.debug_call_stack.get(2).line_number(), 6);
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
