use crate::domain::MemphisValue;

use super::macros::*;

#[test]
fn method_call() {
    let mut session = crosscheck_eval!(
        r#"
class Foo:
    def bar(self):
        return 4

f = Foo()
b = f.bar()
"#
    );
    assert_crosscheck_eq!(session, "b", MemphisValue::Integer(4));

    let mut session = crosscheck_eval!(
        r#"
class Foo:
    def __init__(self, val):
        self.val = val

    def bar(self):
        return self.val

f = Foo(10)
b = f.bar()
"#
    );
    assert_crosscheck_eq!(session, "b", MemphisValue::Integer(10));
}
