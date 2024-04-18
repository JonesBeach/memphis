use crate::bytecode_vm::types::Value;
use crate::treewalk::types::ExprResult;

/// A common implementation to represent the return value of a Python expression for use in
/// crosscheck tests. This frees each engine up to implement their return values as they like,
/// provided the [`From`] trait is implemented.
#[derive(Clone, Debug, PartialEq)]
pub enum TestValue {
    Void,
    None,
    Integer(i64),
    String(String),
    Boolean(bool),
}

impl From<Value> for TestValue {
    fn from(value: Value) -> Self {
        match value {
            Value::Void => TestValue::Void,
            Value::None => TestValue::None,
            Value::Integer(val) => TestValue::Integer(val),
            Value::String(val) => TestValue::String(val),
            Value::Boolean(val) => TestValue::Boolean(val),
            _ => unimplemented!(
                "Conversion to TestValue not implemented for type {:?}",
                value
            ),
        }
    }
}

impl From<ExprResult> for TestValue {
    fn from(value: ExprResult) -> Self {
        match value {
            ExprResult::Void => TestValue::Void,
            ExprResult::None => TestValue::None,
            ExprResult::Integer(_) => {
                TestValue::Integer(value.as_integer_val().expect("failed to get integer"))
            }
            ExprResult::String(_) => {
                TestValue::String(value.as_string().expect("failed to get string"))
            }
            ExprResult::Boolean(val) => TestValue::Boolean(val),
            _ => unimplemented!(
                "Conversion to TestValue not implemented for type '{}'",
                value.get_type()
            ),
        }
    }
}
