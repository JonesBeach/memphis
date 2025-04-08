use memphis::{
    crosscheck_utils::{BytecodeVmAdapter, InterpreterTest, TreewalkAdapter},
    domain::MemphisValue,
};

fn run_binary_expression_test<T: InterpreterTest>(mut interpreter: T) {
    let input = "2 + 2";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Integer(4));
        }
    }

    let input = "2 / 2";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Integer(1));
        }
    }

    let input = "4 < 5";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Boolean(true));
        }
    }

    let input = "4 > 5";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Boolean(false));
        }
    }

    let input = "4 * (2 + 3)";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Integer(20));
        }
    }
}

fn run_unary_expression_test<T: InterpreterTest>(mut interpreter: T) {
    let input = "-2";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Integer(-2));
        }
    }

    let input = "-(-2)";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Integer(2));
        }
    }

    let input = "+5";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Integer(5));
        }
    }

    let input = "+(-5)";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Integer(-5));
        }
    }

    let input = "not True";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Boolean(false));
        }
    }

    let input = "not False";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Boolean(true));
        }
    }

    let input = "~0b1101";
    match interpreter.evaluate(input) {
        Err(e) => panic!("Interpreter error: {:?}", e),
        Ok(result) => {
            assert_eq!(result, MemphisValue::Integer(-14));
        }
    }

    // TODO test unpacking here
}

#[test]
fn test_treewalk_binary_expression() {
    run_binary_expression_test(TreewalkAdapter::new());
}

#[test]
fn test_bytecode_vm_binary_expression() {
    run_binary_expression_test(BytecodeVmAdapter::new());
}

#[test]
fn test_treewalk_unary_expression() {
    run_unary_expression_test(TreewalkAdapter::new());
}

#[test]
fn test_bytecode_vm_unary_expression() {
    run_unary_expression_test(BytecodeVmAdapter::new());
}
