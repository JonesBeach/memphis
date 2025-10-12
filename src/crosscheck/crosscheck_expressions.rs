use crate::domain::MemphisValue;

use super::macros::*;

#[test]
fn binary_expression() {
    let input = "2 + 2";
    assert_crosscheck_return!(input, MemphisValue::Integer(4));

    let input = "2 + 2.1";
    assert_crosscheck_return!(input, MemphisValue::Float(4.1));

    let input = "2 / 2";
    assert_crosscheck_return!(input, MemphisValue::Float(1.0));

    let input = "4 * (2 + 3)";
    assert_crosscheck_return!(input, MemphisValue::Integer(20));
}

#[test]
fn binary_expression_compare_op() {
    let input = "4 < 5";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "4 > 5";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));
}

#[test]
fn operator_chaining() {
    // Equal chains
    let input = "2 == 2 == 2";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "2 == 2 == 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "2 == 2 == 3 == 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "2 == 2 == 2 < 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    // Increasing chain
    let input = "1 < 2 < 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "1 < 2 < 2";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "1 < 3 < 2";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "1 < 2 < 3 < 4";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    // Mixed increasing / equality
    let input = "1 < 2 == 2 < 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "1 < 2 == 3 < 4";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    // Mixed with >= and <=
    let input = "3 >= 2 > 1";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "3 >= 2 > 2";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "2 <= 2 < 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "2 <= 1 < 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    // Mixed equality and less-than
    let input = "2 == 2 < 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "2 == 3 < 4";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "2 < 3 == 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "2 < 3 == 4";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    // Descending chain
    let input = "5 > 4 > 3 > 2";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "5 > 4 > 5";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    // With floats mixed in
    let input = "1 < 2.0 < 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "1.0 < 2 < 1.5";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "2.0 == 2 < 3.0";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    // Not-equals chain
    let input = "1 != 2 != 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "1 != 1 != 2";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    // Mix of == and !=
    let input = "1 == 1 != 2";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "1 == 2 != 3";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    // Chained in — both true
    let input = "2 in [1,2,3] in [[1,2,3],[4,5,6]]";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "2 in [1,2,3] in [[2,3,4],[4,5,6]]";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    // Chained not in
    let input = "4 not in [1,2,3] not in [[1,2,3],[4,5,6]]";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    // Mixed in / not in
    let input = "2 in [1,2,3] not in [[1,2,3],[4,5,6]]";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    // Mixed not in / in
    let input = "4 not in [1,2,3] in [[4,5,6],[7,8,9]]";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));
}

#[test]
fn binary_expression_logical_op() {
    let input = "True and False";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "True or False";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));
}

#[test]
fn unary_expression_negative() {
    let input = "-2";
    assert_crosscheck_return!(input, MemphisValue::Integer(-2));

    let input = "-2.5";
    assert_crosscheck_return!(input, MemphisValue::Float(-2.5));

    let input = "-(-2)";
    assert_crosscheck_return!(input, MemphisValue::Integer(2));

    let input = "-(-2.5)";
    assert_crosscheck_return!(input, MemphisValue::Float(2.5));
}

#[test]
fn unary_expression_positive() {
    let input = "+5";
    assert_crosscheck_return!(input, MemphisValue::Integer(5));

    let input = "+(-5)";
    assert_crosscheck_return!(input, MemphisValue::Integer(-5));
}

#[test]
fn unary_expression_not() {
    let input = "not True";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "not False";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "not None";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "not 1";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "not 0";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "not 1.0";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "not 0.0";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = r#"not "a""#;
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = r#"not """#;
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));

    let input = "not [1]";
    assert_crosscheck_return!(input, MemphisValue::Boolean(false));

    let input = "not []";
    assert_crosscheck_return!(input, MemphisValue::Boolean(true));
}

#[test]
fn unary_expression_invert() {
    let input = "~0b1101";
    assert_crosscheck_return!(input, MemphisValue::Integer(-14));
}
