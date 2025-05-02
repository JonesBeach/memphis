use crate::{
    core::Container,
    parser::types::{BinOp, LogicalOp, UnaryOp},
    treewalk::{
        types::{List, Set},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn evaluate_numeric_op<F>(
        &self,
        left: TreewalkValue,
        right: TreewalkValue,
        op: F,
        force_float: bool,
    ) -> TreewalkResult<TreewalkValue>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let result = match (left, right) {
            (TreewalkValue::Integer(x), TreewalkValue::Integer(y)) => {
                let res = op(x as f64, y as f64);
                if force_float {
                    TreewalkValue::Float(res)
                } else {
                    TreewalkValue::Integer(res as i64)
                }
            }
            (TreewalkValue::Float(x), TreewalkValue::Float(y)) => TreewalkValue::Float(op(x, y)),
            (TreewalkValue::Integer(x), TreewalkValue::Float(y)) => {
                TreewalkValue::Float(op(x as f64, y))
            }
            (TreewalkValue::Float(x), TreewalkValue::Integer(y)) => {
                TreewalkValue::Float(op(x, y as f64))
            }
            _ => return Err(self.type_error("Unsupported operand types for numeric operation")),
        };

        Ok(result)
    }

    pub fn evaluate_logical_op(
        &self,
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
        &self,
        left: i64,
        op: &BinOp,
        right: i64,
    ) -> TreewalkResult<TreewalkValue> {
        match op {
            BinOp::Add => Ok(TreewalkValue::Integer(left + right)),
            BinOp::Sub => Ok(TreewalkValue::Integer(left - right)),
            BinOp::Mul => Ok(TreewalkValue::Integer(left * right)),
            BinOp::Div => {
                if right == 0 {
                    Err(self.div_by_zero_error("integer division or modulo by zero"))
                } else {
                    Ok(TreewalkValue::Float(left as f64 / right as f64))
                }
            }
            BinOp::IntegerDiv => {
                if right == 0 {
                    Err(self.div_by_zero_error("integer division or modulo by zero"))
                } else {
                    Ok(TreewalkValue::Integer(left / right))
                }
            }
            BinOp::Mod => {
                if right == 0 {
                    Err(self.div_by_zero_error("integer division or modulo by zero"))
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
                let right: u32 = right.try_into().map_err(|_| self.runtime_error())?;
                Ok(TreewalkValue::Integer(left.pow(right)))
            }
            BinOp::In | BinOp::NotIn => Err(self.type_error("Expected an iterable")),
            _ => unreachable!(),
        }
    }

    pub fn evaluate_floating_point_operation(
        &self,
        left: f64,
        op: &BinOp,
        right: f64,
    ) -> TreewalkResult<TreewalkValue> {
        match op {
            BinOp::Add => Ok(TreewalkValue::Float(left + right)),
            BinOp::Sub => Ok(TreewalkValue::Float(left - right)),
            BinOp::Mul => Ok(TreewalkValue::Float(left * right)),
            BinOp::Div => {
                if right == 0.0 {
                    Err(self.div_by_zero_error("float division by zero"))
                } else {
                    Ok(TreewalkValue::Float(left / right))
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
        &self,
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
        &self,
        op: &UnaryOp,
        right: TreewalkValue,
    ) -> TreewalkResult<TreewalkValue> {
        match op {
            UnaryOp::Minus => Ok(right.negated()),
            // this acts as a no-op. can be overridden with __pos__ for custom classes
            UnaryOp::Plus => Ok(right),
            UnaryOp::Not => Ok(right.inverted()),
            UnaryOp::BitwiseNot => {
                let i = right.as_integer().ok_or_else(|| {
                    self.type_error(format!(
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
                        self.type_error(format!(
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
        &self,
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
        &self,
        left: Container<List>,
        op: &BinOp,
        right: Container<List>,
    ) -> TreewalkResult<TreewalkValue> {
        let l = left.borrow().clone();
        let r = right.borrow().clone();
        match op {
            BinOp::Add => unreachable!("This should be handled in evaluate_binary_op now"),
            BinOp::Equals => Ok(TreewalkValue::Boolean(l == r)),
            BinOp::NotEquals => Ok(TreewalkValue::Boolean(l != r)),
            _ => unimplemented!(),
        }
    }
}
