use crate::domain::ExecutionErrorKind;
use crate::treewalk::interpreter::TreewalkResult;
use crate::{
    core::Container,
    parser::types::{BinOp, LogicalOp, UnaryOp},
    treewalk::{
        types::{ExprResult, List, Set},
        Interpreter,
    },
};

pub fn evaluate_logical_op(left: bool, op: &LogicalOp, right: bool) -> TreewalkResult<ExprResult> {
    match op {
        LogicalOp::And => Ok(ExprResult::Boolean(left && right)),
        LogicalOp::Or => Ok(ExprResult::Boolean(left || right)),
    }
}

pub fn evaluate_integer_operation(
    left: i64,
    op: &BinOp,
    right: i64,
    interpreter: &Interpreter,
) -> TreewalkResult<ExprResult> {
    match op {
        BinOp::Add => Ok(ExprResult::Integer(left + right)),
        BinOp::Sub => Ok(ExprResult::Integer(left - right)),
        BinOp::Mul => Ok(ExprResult::Integer(left * right)),
        BinOp::Div => {
            if right == 0 {
                Err(interpreter.error(ExecutionErrorKind::DivisionByZero(
                    "integer division or modulo by zero".into(),
                )))
            } else {
                Ok(ExprResult::Integer(left / right))
            }
        }
        BinOp::IntegerDiv => {
            if right == 0 {
                Err(interpreter.error(ExecutionErrorKind::DivisionByZero(
                    "integer division or modulo by zero".into(),
                )))
            } else {
                Ok(ExprResult::Integer(left / right))
            }
        }
        BinOp::Mod => {
            if right == 0 {
                Err(interpreter.error(ExecutionErrorKind::DivisionByZero(
                    "integer division or modulo by zero".into(),
                )))
            } else {
                Ok(ExprResult::Integer(left % right))
            }
        }
        BinOp::GreaterThan => Ok(ExprResult::Boolean(left > right)),
        BinOp::LessThan => Ok(ExprResult::Boolean(left < right)),
        BinOp::GreaterThanOrEqual => Ok(ExprResult::Boolean(left >= right)),
        BinOp::LessThanOrEqual => Ok(ExprResult::Boolean(left <= right)),
        BinOp::Equals => Ok(ExprResult::Boolean(left == right)),
        BinOp::NotEquals => Ok(ExprResult::Boolean(left != right)),
        BinOp::BitwiseAnd => Ok(ExprResult::Integer(left & right)),
        BinOp::BitwiseOr => Ok(ExprResult::Integer(left | right)),
        BinOp::BitwiseXor => Ok(ExprResult::Integer(left ^ right)),
        BinOp::LeftShift => {
            if right > 100 {
                // TODO support long ranges. This is found in _collections_abc.py
                // longrange_iterator = type(iter(range(1 << 1000)))
                Ok(ExprResult::Integer(left << 10))
            } else {
                Ok(ExprResult::Integer(left << right))
            }
        }
        BinOp::RightShift => Ok(ExprResult::Integer(left >> right)),
        BinOp::Expo => {
            let right: u32 = right.try_into().map_err(|_| interpreter.runtime_error())?;
            Ok(ExprResult::Integer(left.pow(right)))
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
) -> TreewalkResult<ExprResult> {
    match op {
        BinOp::Add => Ok(ExprResult::FloatingPoint(left + right)),
        BinOp::Sub => Ok(ExprResult::FloatingPoint(left - right)),
        BinOp::Mul => Ok(ExprResult::FloatingPoint(left * right)),
        BinOp::Div => {
            if right == 0.0 {
                Err(interpreter.error(ExecutionErrorKind::DivisionByZero(
                    "float division by zero".into(),
                )))
            } else {
                Ok(ExprResult::FloatingPoint(left / right))
            }
        }
        BinOp::GreaterThan => Ok(ExprResult::Boolean(left > right)),
        BinOp::LessThan => Ok(ExprResult::Boolean(left < right)),
        BinOp::GreaterThanOrEqual => Ok(ExprResult::Boolean(left >= right)),
        BinOp::LessThanOrEqual => Ok(ExprResult::Boolean(left <= right)),
        BinOp::Equals => Ok(ExprResult::Boolean(left == right)),
        BinOp::NotEquals => Ok(ExprResult::Boolean(left != right)),
        _ => unimplemented!(),
    }
}

pub fn evaluate_object_comparison(
    left: ExprResult,
    op: &BinOp,
    right: ExprResult,
) -> TreewalkResult<ExprResult> {
    match op {
        BinOp::Equals => Ok(ExprResult::Boolean(left == right)),
        BinOp::NotEquals => Ok(ExprResult::Boolean(left != right)),
        BinOp::Is => Ok(ExprResult::Boolean(left.is(&right))),
        BinOp::IsNot => Ok(ExprResult::Boolean(!left.is(&right))),
        _ => unimplemented!(),
    }
}

pub fn evaluate_unary_operation(
    op: &UnaryOp,
    right: ExprResult,
    interpreter: &Interpreter,
) -> TreewalkResult<ExprResult> {
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
            Ok(ExprResult::Integer(!i))
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
            Ok(ExprResult::List(list))
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
) -> TreewalkResult<ExprResult> {
    let l = left.borrow().clone();
    let r = right.borrow().clone();
    match op {
        BinOp::Equals => Ok(ExprResult::Boolean(l == r)),
        BinOp::NotEquals => Ok(ExprResult::Boolean(l != r)),
        BinOp::LessThanOrEqual => Ok(ExprResult::Boolean(l.subset(r))),
        _ => unimplemented!(),
    }
}

pub fn evaluate_list_operation(
    left: Container<List>,
    op: &BinOp,
    right: Container<List>,
) -> TreewalkResult<ExprResult> {
    let l = left.borrow().clone();
    let r = right.borrow().clone();
    match op {
        BinOp::Add => Ok(ExprResult::List(Container::new(l + r))),
        BinOp::Equals => Ok(ExprResult::Boolean(l == r)),
        BinOp::NotEquals => Ok(ExprResult::Boolean(l != r)),
        _ => unimplemented!(),
    }
}
