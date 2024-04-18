use memphis::crosscheck::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter};

fn run_test<T: InterpreterTest>(interpreter: &T) {
    let input = r#"
def foo(a, b):
    return a + b

a = foo(2, 9)
"#;
    interpreter.assert_var_expected(input, "a", TestValue::Integer(11));

    let input = r#"
def foo(a, b):
    c = 9
    return a + b + c

a = foo(2, 9)
"#;
    interpreter.assert_var_expected(input, "a", TestValue::Integer(20));
}

#[test]
fn test_treewalk_function_call() {
    let interpreter = TreewalkAdapter {};
    run_test(&interpreter);
}

#[test]
fn test_bytecode_vm_function_call() {
    let interpreter = BytecodeVmAdapter {};
    run_test(&interpreter);
}
