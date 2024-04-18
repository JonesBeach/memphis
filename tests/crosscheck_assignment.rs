use std::collections::HashMap;

use memphis::crosscheck::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter};

fn run_test<T: InterpreterTest>(interpreter: &T) {
    let input = r#"
a = 5 - 3
"#;
    interpreter.assert_var_expected(input, "a", TestValue::Integer(2));

    let input = r#"
a = "Hello World"
"#;
    interpreter.assert_var_expected(input, "a", TestValue::String("Hello World".into()));

    let input = r#"
a = 5 - 3
b = 10
c = None
"#;
    interpreter.assert_vars_expected(
        input,
        HashMap::from([
            ("a", TestValue::Integer(2)),
            ("b", TestValue::Integer(10)),
            ("c", TestValue::None),
        ]),
    );

    let input = r#"
a = 5 - 3
b = 10 + a
"#;
    interpreter.assert_vars_expected(
        input,
        HashMap::from([("a", TestValue::Integer(2)), ("b", TestValue::Integer(12))]),
    );
}

#[test]
fn test_treewalk_assignment() {
    let interpreter = TreewalkAdapter {};
    run_test(&interpreter);
}

#[test]
fn test_bytecode_vm_assignment() {
    let interpreter = BytecodeVmAdapter {};
    run_test(&interpreter);
}
