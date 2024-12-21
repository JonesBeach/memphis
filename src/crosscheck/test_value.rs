use std::fmt::{Display, Formatter, Result};

use crate::bytecode_vm::types::Value;
use crate::treewalk::types::ExprResult;

/// A common implementation to represent the return value of a Python expression for use in
/// crosscheck tests. This frees each engine up to implement their return values as they like,
/// provided the [`From`] trait is implemented.
#[derive(Clone, Debug, PartialEq)]
pub enum TestValue {
    None,
    Integer(i64),
    String(String),
    Boolean(bool),
    List(Vec<TestValue>),
}

impl Display for TestValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TestValue::None => write!(f, "None"),
            TestValue::Integer(i) => write!(f, "{}", i),
            TestValue::List(i) => {
                let items = i
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{}]", items)
            }
            _ => unimplemented!(),
        }
    }
}

impl From<Value> for TestValue {
    fn from(value: Value) -> Self {
        match value {
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
            ExprResult::None => TestValue::None,
            ExprResult::Integer(_) => {
                TestValue::Integer(value.as_integer().expect("Failed to get integer"))
            }
            ExprResult::String(_) => {
                TestValue::String(value.as_string().expect("failed to get string"))
            }
            ExprResult::Boolean(val) => TestValue::Boolean(val),
            ExprResult::List(i) => {
                let items = i
                    .into_iter()
                    .map(|item| item.into())
                    .collect::<Vec<TestValue>>();
                TestValue::List(items)
            }
            _ => unimplemented!(
                "Conversion to TestValue not implemented for type '{}'",
                value.get_type()
            ),
        }
    }
}
