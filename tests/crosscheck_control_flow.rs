use memphis::crosscheck::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter};

fn run_test<T: InterpreterTest>(interpreter: &T) {
    let input = r#"
i = 0
n = 4
while i < n:
    i = i + 1
"#;
    interpreter.assert_var_expected(input, "i", TestValue::Integer(4));

    let input = r#"
i = 0
if i < 10:
    a = -1
"#;
    interpreter.assert_var_expected(input, "a", TestValue::Integer(-1));

    let input = r#"
i = 0
if i > 10:
    a = -1
else:
    a = 3
"#;
    interpreter.assert_var_expected(input, "a", TestValue::Integer(3));
}

#[test]
fn test_treewalk_control_flow() {
    let interpreter = TreewalkAdapter {};
    run_test(&interpreter);
}

#[test]
fn test_bytecode_vm_control_flow() {
    let interpreter = BytecodeVmAdapter {};
    run_test(&interpreter);
}
