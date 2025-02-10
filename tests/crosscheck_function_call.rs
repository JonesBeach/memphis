use memphis::crosscheck_utils::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter};

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
        Ok(_) => panic!("Expected error!"),
        Err(_e) => {}
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
