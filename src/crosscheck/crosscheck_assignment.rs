use crate::domain::MemphisValue;

use super::macros::*;

#[test]
fn assignment() {
    let mut session = crosscheck_eval!(
        r#"
a = 5 - 3
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(2));

    let mut session = crosscheck_eval!(
        r#"
a = "Hello World"
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::String("Hello World".into()));

    let mut session = crosscheck_eval!(
        r#"
a = 5 - 3
b = 10
c = None
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(2));
    assert_crosscheck_eq!(session, "b", MemphisValue::Integer(10));
    assert_crosscheck_eq!(session, "c", MemphisValue::None);

    let mut session = crosscheck_eval!(
        r#"
a = 5 - 3
b = 10 + a
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(2));
    assert_crosscheck_eq!(session, "b", MemphisValue::Integer(12));
}
