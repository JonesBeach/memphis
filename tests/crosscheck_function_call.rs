use memphis::crosscheck_utils::{
    Adapter, BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter,
};

#[crosscheck::test_with(BytecodeVmAdapter, TreewalkAdapter)]
fn test_function_call(mut adapter: Adapter) {
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
        Err(e) => {
            dbg!(e);
            todo!();
        }
    }
}
