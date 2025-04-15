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
}
