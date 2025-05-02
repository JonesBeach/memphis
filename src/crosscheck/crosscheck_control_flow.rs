use crate::domain::MemphisValue;

use super::macros::*;

#[test]
fn control_flow() {
    let mut session = crosscheck_eval!(
        r#"
i = 0
n = 4
while i < n:
    i = i + 1
"#
    );
    assert_crosscheck_eq!(session, "i", MemphisValue::Integer(4));

    let mut session = crosscheck_eval!(
        r#"
i = 0
if i < 10:
    a = -1
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(-1));

    let mut session = crosscheck_eval!(
        r#"
i = 0
if i > 10:
    a = -1
else:
    a = 3
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(3));

    let mut session = crosscheck_eval!(
        r#"
i = 0
if i > 10:
    a = -1
elif i > -5:
    a = -2
else:
    a = 3
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::Integer(-2));

    let mut session = crosscheck_eval!(
        r#"
x = 7
result = "none"
if x < 5:
    result = "low"
elif x < 6:
    result = "mid"
"#
    );
    assert_crosscheck_eq!(session, "result", MemphisValue::String("none".to_string()));

    let mut session = crosscheck_eval!(
        r#"
x = 2
if x < 0:
    a = "zero"
elif x < 1:
    a = "one"
elif x < 2:
    a = "two"
elif x < 3:
    a = "three"
else:
    a = "other"
"#
    );
    assert_crosscheck_eq!(session, "a", MemphisValue::String("three".to_string()));
}
