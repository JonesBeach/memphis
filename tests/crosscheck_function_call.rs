use memphis::crosscheck::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter};

fn run_test<T: InterpreterTest>(mut interpreter: T) {
    let input = r#"
def foo(a, b):
    return a + b

a = foo(2, 9)
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("a"), Some(TestValue::Integer(11)));

    let input = r#"
def foo(a, b):
    c = 9
    return a + b + c

a = foo(2, 9)
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("a"), Some(TestValue::Integer(20)));
}

#[test]
fn test_treewalk_function_call() {
    run_test(TreewalkAdapter::new());
}

#[test]
fn test_bytecode_vm_function_call() {
    run_test(BytecodeVmAdapter::new());
}
