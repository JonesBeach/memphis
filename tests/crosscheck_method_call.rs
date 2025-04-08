use memphis::{
    crosscheck_utils::{BytecodeVmAdapter, InterpreterTest, TreewalkAdapter},
    domain::MemphisValue,
};

fn run_test<T: InterpreterTest>(mut interpreter: T) {
    let input = r#"
class Foo:
    def bar(self):
        return 4

f = Foo()
b = f.bar()
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("b"), Some(MemphisValue::Integer(4)));

    let input = r#"
class Foo:
    def __init__(self, val):
        self.val = val

    def bar(self):
        return self.val

f = Foo(10)
b = f.bar()
"#;
    let _ = interpreter.evaluate(input);
    assert_eq!(interpreter.read("b"), Some(MemphisValue::Integer(10)));
}

#[test]
fn test_treewalk_method_call() {
    run_test(TreewalkAdapter::new());
}

#[test]
fn test_bytecode_vm_method_call() {
    run_test(BytecodeVmAdapter::new());
}
