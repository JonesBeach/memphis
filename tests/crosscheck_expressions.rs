use memphis::crosscheck::{BytecodeVmAdapter, InterpreterTest, TestValue, TreewalkAdapter};

fn run_binary_expression_test<T: InterpreterTest>(interpreter: &T) {
    let input = "2 + 2";
    interpreter.assert_expr_expected(input, TestValue::Integer(4));

    let input = "2 / 2";
    interpreter.assert_expr_expected(input, TestValue::Integer(1));

    let input = "4 < 5";
    interpreter.assert_expr_expected(input, TestValue::Boolean(true));

    let input = "4 > 5";
    interpreter.assert_expr_expected(input, TestValue::Boolean(false));

    let input = "4 * (2 + 3)";
    interpreter.assert_expr_expected(input, TestValue::Integer(20));
}

fn run_unary_expression_test<T: InterpreterTest>(interpreter: &T) {
    let input = "-2";
    interpreter.assert_expr_expected(input, TestValue::Integer(-2));

    let input = "-(-2)";
    interpreter.assert_expr_expected(input, TestValue::Integer(2));

    let input = "+5";
    interpreter.assert_expr_expected(input, TestValue::Integer(5));

    let input = "+(-5)";
    interpreter.assert_expr_expected(input, TestValue::Integer(-5));

    let input = "not True";
    interpreter.assert_expr_expected(input, TestValue::Boolean(false));

    let input = "not False";
    interpreter.assert_expr_expected(input, TestValue::Boolean(true));

    let input = "~0b1101";
    interpreter.assert_expr_expected(input, TestValue::Integer(-14));

    // TODO test unpacking here
}

#[test]
fn test_treewalk_binary_expression() {
    let interpreter = TreewalkAdapter {};
    run_binary_expression_test(&interpreter);
}

#[test]
fn test_bytecode_vm_binary_expression() {
    let interpreter = BytecodeVmAdapter {};
    run_binary_expression_test(&interpreter);
}

#[test]
fn test_treewalk_unary_expression() {
    let interpreter = TreewalkAdapter {};
    run_unary_expression_test(&interpreter);
}

#[test]
fn test_bytecode_vm_unary_expression() {
    let interpreter = BytecodeVmAdapter {};
    run_unary_expression_test(&interpreter);
}
