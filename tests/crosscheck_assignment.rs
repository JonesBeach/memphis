use memphis::crosscheck_utils::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter};

fn run_test<T: InterpreterTest>(mut interpreter: T) {
    let input = r#"
a = 5 - 3
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("a"), Some(TestValue::Integer(2)));

    let input = r#"
a = "Hello World"
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(
        interpreter.read("a"),
        Some(TestValue::String("Hello World".into()))
    );

    let input = r#"
a = 5 - 3
b = 10
c = None
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("a"), Some(TestValue::Integer(2)));
    assert_eq!(interpreter.read("b"), Some(TestValue::Integer(10)));
    assert_eq!(interpreter.read("c"), Some(TestValue::None));

    let input = r#"
a = 5 - 3
b = 10 + a
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("a"), Some(TestValue::Integer(2)));
    assert_eq!(interpreter.read("b"), Some(TestValue::Integer(12)));
}

#[test]
fn test_treewalk_assignment() {
    run_test(TreewalkAdapter::new());
}

#[test]
fn test_bytecode_vm_assignment() {
    run_test(BytecodeVmAdapter::new());
}
