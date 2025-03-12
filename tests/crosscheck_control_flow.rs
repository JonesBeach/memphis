use memphis::crosscheck_utils::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter};

fn run_test<T: InterpreterTest>(mut interpreter: T) {
    let input = r#"
i = 0
n = 4
while i < n:
    i = i + 1
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("i"), Some(TestValue::Integer(4)));

    let input = r#"
i = 0
if i < 10:
    a = -1
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("a"), Some(TestValue::Integer(-1)));

    let input = r#"
i = 0
if i > 10:
    a = -1
else:
    a = 3
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("a"), Some(TestValue::Integer(3)));
}

#[test]
fn test_treewalk_control_flow() {
    run_test(TreewalkAdapter::new());
}

#[test]
fn test_bytecode_vm_control_flow() {
    run_test(BytecodeVmAdapter::new());
}
