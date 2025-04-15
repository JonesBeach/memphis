use crate::domain::MemphisValue;

use super::macros::*;

#[test]
fn binary_expression() {
    let input = "2 + 2";
    assert_crosscheck_return!(input, MemphisValue::Integer(4));

    let input = "2 / 2";
    assert_crosscheck_return!(input, MemphisValue::Integer(1));

    let input = "4 < 5";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "4 > 5";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "4 * (2 + 3)";
    assert_crosscheck_return!(input, MemphisValue::Integer(20));
}

#[test]
fn unary_expression() {
    let input = "-2";
    assert_crosscheck_return!(input, MemphisValue::Integer(-2));

    let input = "-(-2)";
    assert_crosscheck_return!(input, MemphisValue::Integer(2));

    let input = "+5";
    assert_crosscheck_return!(input, MemphisValue::Integer(5));

    let input = "+(-5)";
    assert_crosscheck_return!(input, MemphisValue::Integer(-5));

    let input = "not True";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "not False";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "~0b1101";
    assert_crosscheck_return!(input, MemphisValue::Integer(-14));

    // TODO test unpacking here
}
