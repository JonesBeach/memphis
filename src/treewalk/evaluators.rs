use crate::{
    core::{Container, Storable},
    parser::types::{BinOp, LogicalOp, UnaryOp},
    treewalk::types::{ExprResult, List},
    types::errors::InterpreterError,
};

use super::{types::Set, CallStack};

pub(crate) fn evaluate_logical_op(
    left: bool,
    op: &LogicalOp,
    right: bool,
) -> Result<ExprResult, InterpreterError> {
    match op {
        LogicalOp::And => Ok(ExprResult::Boolean(left && right)),
        LogicalOp::Or => Ok(ExprResult::Boolean(left || right)),
    }
}

pub(crate) fn evaluate_integer_operation(
    left: i64,
    op: &BinOp,
    right: i64,
    call_stack: CallStack,
) -> Result<ExprResult, InterpreterError> {
    match op {
        BinOp::Add => Ok(ExprResult::Integer(Container::new(left + right))),
        BinOp::Sub => Ok(ExprResult::Integer(Container::new(left - right))),
        BinOp::Mul => Ok(ExprResult::Integer(Container::new(left * right))),
        BinOp::Div => {
            if right == 0 {
                Err(InterpreterError::DivisionByZero(
                    "division by zero".into(),
                    call_stack,
                ))
            } else {
                Ok(ExprResult::Integer(Container::new(left / right)))
            }
        }
        BinOp::IntegerDiv => {
            if right == 0 {
                Err(InterpreterError::DivisionByZero(
                    "integer division or modulo by zero".into(),
                    call_stack,
                ))
            } else {
                Ok(ExprResult::Integer(Container::new(left / right)))
            }
        }
        BinOp::Mod => {
            if right == 0 {
                Err(InterpreterError::DivisionByZero(
                    "integer division or modulo by zero".into(),
                    call_stack,
                ))
            } else {
                Ok(ExprResult::Integer(Container::new(left % right)))
            }
        }
        BinOp::GreaterThan => Ok(ExprResult::Boolean(left > right)),
        BinOp::LessThan => Ok(ExprResult::Boolean(left < right)),
        BinOp::GreaterThanOrEqual => Ok(ExprResult::Boolean(left >= right)),
        BinOp::LessThanOrEqual => Ok(ExprResult::Boolean(left <= right)),
        BinOp::Equals => Ok(ExprResult::Boolean(left == right)),
        BinOp::NotEquals => Ok(ExprResult::Boolean(left != right)),
        BinOp::BitwiseAnd => Ok(ExprResult::Integer(Container::new(left & right))),
        BinOp::BitwiseOr => Ok(ExprResult::Integer(Container::new(left | right))),
        BinOp::BitwiseXor => Ok(ExprResult::Integer(Container::new(left ^ right))),
        BinOp::LeftShift => {
            if right > 100 {
                // TODO support long ranges. This is found in _collections_abc.py
                // longrange_iterator = type(iter(range(1 << 1000)))
                Ok(ExprResult::Integer(Container::new(left << 10)))
            } else {
                Ok(ExprResult::Integer(Container::new(left << right)))
            }
        }
        BinOp::RightShift => Ok(ExprResult::Integer(Container::new(left >> right))),
        BinOp::In => Err(InterpreterError::ExpectedIterable(call_stack)),
        BinOp::NotIn => Err(InterpreterError::ExpectedIterable(call_stack)),
        BinOp::Expo => {
            let right: u32 = right
                .try_into()
                .map_err(|_| InterpreterError::RuntimeError)?;
            Ok(ExprResult::Integer(Container::new(left.pow(right))))
        }
        _ => unreachable!(),
    }
}

pub(crate) fn evaluate_floating_point_operation(
    left: f64,
    op: &BinOp,
    right: f64,
    call_stack: CallStack,
) -> Result<ExprResult, InterpreterError> {
    match op {
        BinOp::Add => Ok(ExprResult::FloatingPoint(left + right)),
        BinOp::Sub => Ok(ExprResult::FloatingPoint(left - right)),
        BinOp::Mul => Ok(ExprResult::FloatingPoint(left * right)),
        BinOp::Div => {
            if right == 0.0 {
                Err(InterpreterError::DivisionByZero(
                    "float division by zero".into(),
                    call_stack,
                ))
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

pub(crate) fn evaluate_object_comparison(
    left: ExprResult,
    op: &BinOp,
    right: ExprResult,
) -> Result<ExprResult, InterpreterError> {
    match op {
        BinOp::Equals => Ok(ExprResult::Boolean(left == right)),
        BinOp::NotEquals => Ok(ExprResult::Boolean(left != right)),
        BinOp::Is => Ok(ExprResult::Boolean(left.is(&right))),
        BinOp::IsNot => Ok(ExprResult::Boolean(!left.is(&right))),
        _ => unimplemented!(),
    }
}

pub(crate) fn evaluate_unary_operation(
    op: &UnaryOp,
    right: ExprResult,
    call_stack: CallStack,
) -> Result<ExprResult, InterpreterError> {
    match op {
        UnaryOp::Minus => Ok(right.negated()),
        // this acts as a no-op. can be overridden with __pos__ for custom classes
        UnaryOp::Plus => Ok(right),
        UnaryOp::Not => Ok(right.inverted()),
        UnaryOp::BitwiseNot => {
            let i = right.as_integer().ok_or(InterpreterError::TypeError(
                Some(format!(
                    "bad operand type for unary ~: '{}'",
                    right.get_type()
                )),
                call_stack,
            ))?;

            let o = !*i.borrow();

            Ok(ExprResult::Integer(o.store()))
        }
        UnaryOp::Unpack => {
            let list = right
                .as_list()
                // Attempted to unpack a non-iterable
                .ok_or(InterpreterError::TypeError(
                    Some(format!(
                        "Value after * must be an iterable, not {}",
                        right.get_type()
                    )),
                    call_stack,
                ))?;
            Ok(ExprResult::List(list))
        }
    }
}

pub(crate) fn evaluate_set_operation(
    left: Container<Set>,
    op: &BinOp,
    right: Container<Set>,
) -> Result<ExprResult, InterpreterError> {
    let l = left.borrow().clone();
    let r = right.borrow().clone();
    match op {
        BinOp::Equals => Ok(ExprResult::Boolean(l == r)),
        BinOp::NotEquals => Ok(ExprResult::Boolean(l != r)),
        BinOp::LessThanOrEqual => Ok(ExprResult::Boolean(l.subset(r))),
        _ => unimplemented!(),
    }
}

pub(crate) fn evaluate_list_operation(
    left: Container<List>,
    op: &BinOp,
    right: Container<List>,
) -> Result<ExprResult, InterpreterError> {
    let l = left.borrow().clone();
    let r = right.borrow().clone();
    match op {
        BinOp::Add => Ok(ExprResult::List(Container::new(l + r))),
        BinOp::Equals => Ok(ExprResult::Boolean(l == r)),
        BinOp::NotEquals => Ok(ExprResult::Boolean(l != r)),
        _ => unimplemented!(),
    }
}
