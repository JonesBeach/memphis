use crate::domain::{test_utils::*, MemphisValue};

use super::macros::*;

#[test]
fn generator_next_builtin() {
    let mut session = crosscheck_eval!(
        r#"
def gen():
    yield 1
    yield 2

g = gen()
a = next(g)
b = next(g)
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(1));
    assert_crosscheck_eq!(session, "b", MemphisValue::Integer(2));
}

#[test]
fn generator_as_iterator() {
    let mut session = crosscheck_eval!(
        r#"
def gen():
    yield 1
    yield 2

s = 11
for i in gen():
    s = s + i
"#
    );
    assert_crosscheck_eq!(session, "i", MemphisValue::Integer(2));
    assert_crosscheck_eq!(session, "s", MemphisValue::Integer(14));
}

#[test]
fn generator_stop_iteration() {
    let e = crosscheck_expect_error!(
        r#"
def gen():
    yield 1

g = gen()
a = next(g)
b = next(g)
"#
    );

    assert_stop_iteration!(e);
}
