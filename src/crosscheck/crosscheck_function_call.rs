use crate::domain::{test_utils::*, MemphisValue};

use super::macros::*;

#[test]
fn function_call() {
    let mut session = crosscheck_eval!(
        r#"
def foo(a, b):
    return a + b

a = foo(2, 9)
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(11));

    let mut session = crosscheck_eval!(
        r#"
def foo(a, b):
    c = 9
    return a + b + c

a = foo(2, 9)
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(20));

    let e = crosscheck_expect_error!(
        r#"
def middle_call():
    last_call()

def last_call():
    unknown()

middle_call()
"#
    );

    assert_name_error!(e.execution_error, "unknown");
    assert_eq!(e.debug_call_stack.len(), 3);
    assert_eq!(e.debug_call_stack.get(0).name(), "<module>");
    assert_eq!(e.debug_call_stack.get(0).file_path_str(), "<stdin>");
    assert_eq!(e.debug_call_stack.get(0).line_number(), 8);
    assert_eq!(e.debug_call_stack.get(1).name(), "middle_call");
    assert_eq!(e.debug_call_stack.get(1).file_path_str(), "<stdin>");
    assert_eq!(e.debug_call_stack.get(1).line_number(), 3);
    assert_eq!(e.debug_call_stack.get(2).name(), "last_call");
    assert_eq!(e.debug_call_stack.get(2).file_path_str(), "<stdin>");
    assert_eq!(e.debug_call_stack.get(2).line_number(), 6);
}
