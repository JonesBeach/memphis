use crate::domain::MemphisValue;

use super::macros::*;

#[test]
fn closure() {
    let mut session = crosscheck_eval!(
        r#"
def make_adder(x):
    def inner_adder(y):
        return x + y
    return inner_adder

adder = make_adder(10)
a = adder(5)
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(15));
}
