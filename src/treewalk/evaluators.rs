use crate::{
    core::Container,
    domain::ExecutionErrorKind,
    parser::types::{BinOp, LogicalOp, UnaryOp},
    treewalk::{
        types::{List, Set},
        Interpreter, TreewalkResult, TreewalkValue,
    },
};

pub fn evaluate_logical_op(
    left: bool,
    op: &LogicalOp,
    right: bool,
) -> TreewalkResult<TreewalkValue> {
    match op {
        LogicalOp::And => Ok(TreewalkValue::Boolean(left && right)),
        LogicalOp::Or => Ok(TreewalkValue::Boolean(left || right)),
    }
}

pub fn evaluate_integer_operation(
    left: i64,
    op: &BinOp,
    right: i64,
    interpreter: &Interpreter,
) -> TreewalkResult<TreewalkValue> {
    match op {
        BinOp::Add => Ok(TreewalkValue::Integer(left + right)),
        BinOp::Sub => Ok(TreewalkValue::Integer(left - right)),
        BinOp::Mul => Ok(TreewalkValue::Integer(left * right)),
        BinOp::Div => {
            if right == 0 {
                Err(interpreter.error(ExecutionErrorKind::DivisionByZero(
                    "integer division or modulo by zero".into(),
                )))
            } else {
                Ok(TreewalkValue::Integer(left / right))
            }
        }
        BinOp::IntegerDiv => {
            if right == 0 {
                Err(interpreter.error(ExecutionErrorKind::DivisionByZero(
                    "integer division or modulo by zero".into(),
                )))
            } else {
                Ok(TreewalkValue::Integer(left / right))
            }
        }
        BinOp::Mod => {
            if right == 0 {
                Err(interpreter.error(ExecutionErrorKind::DivisionByZero(
                    "integer division or modulo by zero".into(),
                )))
            } else {
                Ok(TreewalkValue::Integer(left % right))
            }
        }
        BinOp::GreaterThan => Ok(TreewalkValue::Boolean(left > right)),
        BinOp::LessThan => Ok(TreewalkValue::Boolean(left < right)),
        BinOp::GreaterThanOrEqual => Ok(TreewalkValue::Boolean(left >= right)),
        BinOp::LessThanOrEqual => Ok(TreewalkValue::Boolean(left <= right)),
        BinOp::Equals => Ok(TreewalkValue::Boolean(left == right)),
        BinOp::NotEquals => Ok(TreewalkValue::Boolean(left != right)),
        BinOp::BitwiseAnd => Ok(TreewalkValue::Integer(left & right)),
        BinOp::BitwiseOr => Ok(TreewalkValue::Integer(left | right)),
        BinOp::BitwiseXor => Ok(TreewalkValue::Integer(left ^ right)),
        BinOp::LeftShift => {
            if right > 100 {
                // TODO support long ranges. This is found in _collections_abc.py
                // longrange_iterator = type(iter(range(1 << 1000)))
                Ok(TreewalkValue::Integer(left << 10))
            } else {
                Ok(TreewalkValue::Integer(left << right))
            }
        }
        BinOp::RightShift => Ok(TreewalkValue::Integer(left >> right)),
        BinOp::Expo => {
            let right: u32 = right.try_into().map_err(|_| interpreter.runtime_error())?;
            Ok(TreewalkValue::Integer(left.pow(right)))
        }
        BinOp::In | BinOp::NotIn => Err(interpreter.type_error("Expected an iterable")),
        _ => unreachable!(),
    }
}

pub fn evaluate_floating_point_operation(
    left: f64,
    op: &BinOp,
    right: f64,
    interpreter: &Interpreter,
) -> TreewalkResult<TreewalkValue> {
    match op {
        BinOp::Add => Ok(TreewalkValue::FloatingPoint(left + right)),
        BinOp::Sub => Ok(TreewalkValue::FloatingPoint(left - right)),
        BinOp::Mul => Ok(TreewalkValue::FloatingPoint(left * right)),
        BinOp::Div => {
            if right == 0.0 {
                Err(interpreter.error(ExecutionErrorKind::DivisionByZero(
                    "float division by zero".into(),
                )))
            } else {
                Ok(TreewalkValue::FloatingPoint(left / right))
            }
        }
        BinOp::GreaterThan => Ok(TreewalkValue::Boolean(left > right)),
        BinOp::LessThan => Ok(TreewalkValue::Boolean(left < right)),
        BinOp::GreaterThanOrEqual => Ok(TreewalkValue::Boolean(left >= right)),
        BinOp::LessThanOrEqual => Ok(TreewalkValue::Boolean(left <= right)),
        BinOp::Equals => Ok(TreewalkValue::Boolean(left == right)),
        BinOp::NotEquals => Ok(TreewalkValue::Boolean(left != right)),
        _ => unimplemented!(),
    }
}

pub fn evaluate_object_comparison(
    left: TreewalkValue,
    op: &BinOp,
    right: TreewalkValue,
) -> TreewalkResult<TreewalkValue> {
    match op {
        BinOp::Equals => Ok(TreewalkValue::Boolean(left == right)),
        BinOp::NotEquals => Ok(TreewalkValue::Boolean(left != right)),
        BinOp::Is => Ok(TreewalkValue::Boolean(left.is(&right))),
        BinOp::IsNot => Ok(TreewalkValue::Boolean(!left.is(&right))),
        _ => unimplemented!(),
    }
}

pub fn evaluate_unary_operation(
    op: &UnaryOp,
    right: TreewalkValue,
    interpreter: &Interpreter,
) -> TreewalkResult<TreewalkValue> {
    match op {
        UnaryOp::Minus => Ok(right.negated()),
        // this acts as a no-op. can be overridden with __pos__ for custom classes
        UnaryOp::Plus => Ok(right),
        UnaryOp::Not => Ok(right.inverted()),
        UnaryOp::BitwiseNot => {
            let i = right.as_integer().ok_or_else(|| {
                interpreter.type_error(format!(
                    "bad operand type for unary ~: '{}'",
                    right.get_type()
                ))
            })?;
            Ok(TreewalkValue::Integer(!i))
        }
        UnaryOp::Unpack => {
            let list = right
                .as_list()
                // Attempted to unpack a non-iterable
                .ok_or_else(|| {
                    interpreter.type_error(format!(
                        "Value after * must be an iterable, not {}",
                        right.get_type()
                    ))
                })?;
            Ok(TreewalkValue::List(list))
        }
        UnaryOp::DictUnpack => {
            todo!()
        }
    }
}

pub fn evaluate_set_operation(
    left: Container<Set>,
    op: &BinOp,
    right: Container<Set>,
) -> TreewalkResult<TreewalkValue> {
    let l = left.borrow().clone();
    let r = right.borrow().clone();
    match op {
        BinOp::Equals => Ok(TreewalkValue::Boolean(l == r)),
        BinOp::NotEquals => Ok(TreewalkValue::Boolean(l != r)),
        BinOp::LessThanOrEqual => Ok(TreewalkValue::Boolean(l.subset(r))),
        _ => unimplemented!(),
    }
}

pub fn evaluate_list_operation(
    left: Container<List>,
    op: &BinOp,
    right: Container<List>,
) -> TreewalkResult<TreewalkValue> {
    let l = left.borrow().clone();
    let r = right.borrow().clone();
    match op {
        BinOp::Add => Ok(TreewalkValue::List(Container::new(l + r))),
        BinOp::Equals => Ok(TreewalkValue::Boolean(l == r)),
        BinOp::NotEquals => Ok(TreewalkValue::Boolean(l != r)),
        _ => unimplemented!(),
    }
}
