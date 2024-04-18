use memphis::crosscheck::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter};

fn run_test<T: InterpreterTest>(interpreter: &T) {
    let input = r#"
class Foo:
    def bar(self):
        return 4

f = Foo()
b = f.bar()
"#;
    interpreter.assert_var_expected(input, "b", TestValue::Integer(4));

    let input = r#"
class Foo:
    def __init__(self, val):
        self.val = val

    def bar(self):
        return self.val

f = Foo(10)
b = f.bar()
"#;
    interpreter.assert_var_expected(input, "b", TestValue::Integer(10));
}

#[test]
fn test_treewalk_method_call() {
    let interpreter = TreewalkAdapter {};
    run_test(&interpreter);
}

#[test]
fn test_bytecode_vm_method_call() {
    let interpreter = BytecodeVmAdapter {};
    run_test(&interpreter);
}
