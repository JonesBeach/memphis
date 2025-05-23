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
            (TreewalkValue::Int(x), TreewalkValue::Int(y)) => {
                let res = op(x as f64, y as f64);
                if force_float {
                    TreewalkValue::Float(res)
                } else {
                    TreewalkValue::Int(res as i64)
                }
            }
            (TreewalkValue::Float(x), TreewalkValue::Float(y)) => TreewalkValue::Float(op(x, y)),
            (TreewalkValue::Int(x), TreewalkValue::Float(y)) => {
                TreewalkValue::Float(op(x as f64, y))
            }
            (TreewalkValue::Float(x), TreewalkValue::Int(y)) => {
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
            LogicalOp::And => Ok(TreewalkValue::Bool(left && right)),
            LogicalOp::Or => Ok(TreewalkValue::Bool(left || right)),
        }
    }

    pub fn evaluate_integer_operation(
        &self,
        left: i64,
        op: &BinOp,
        right: i64,
    ) -> TreewalkResult<TreewalkValue> {
        match op {
            BinOp::Add => Ok(TreewalkValue::Int(left + right)),
            BinOp::Sub => Ok(TreewalkValue::Int(left - right)),
            BinOp::Mul => Ok(TreewalkValue::Int(left * right)),
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
                    Ok(TreewalkValue::Int(left / right))
                }
            }
            BinOp::Mod => {
                if right == 0 {
                    Err(self.div_by_zero_error("integer division or modulo by zero"))
                } else {
                    Ok(TreewalkValue::Int(left % right))
                }
            }
            BinOp::GreaterThan => Ok(TreewalkValue::Bool(left > right)),
            BinOp::LessThan => Ok(TreewalkValue::Bool(left < right)),
            BinOp::GreaterThanOrEqual => Ok(TreewalkValue::Bool(left >= right)),
            BinOp::LessThanOrEqual => Ok(TreewalkValue::Bool(left <= right)),
            BinOp::Equals => Ok(TreewalkValue::Bool(left == right)),
            BinOp::NotEquals => Ok(TreewalkValue::Bool(left != right)),
            BinOp::BitwiseAnd => Ok(TreewalkValue::Int(left & right)),
            BinOp::BitwiseOr => Ok(TreewalkValue::Int(left | right)),
            BinOp::BitwiseXor => Ok(TreewalkValue::Int(left ^ right)),
            BinOp::LeftShift => {
                if right > 100 {
                    // TODO support long ranges. This is found in _collections_abc.py
                    // longrange_iterator = type(iter(range(1 << 1000)))
                    Ok(TreewalkValue::Int(left << 10))
                } else {
                    Ok(TreewalkValue::Int(left << right))
                }
            }
            BinOp::RightShift => Ok(TreewalkValue::Int(left >> right)),
            BinOp::Expo => {
                let right: u32 = right.try_into().map_err(|_| self.runtime_error())?;
                Ok(TreewalkValue::Int(left.pow(right)))
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
            BinOp::GreaterThan => Ok(TreewalkValue::Bool(left > right)),
            BinOp::LessThan => Ok(TreewalkValue::Bool(left < right)),
            BinOp::GreaterThanOrEqual => Ok(TreewalkValue::Bool(left >= right)),
            BinOp::LessThanOrEqual => Ok(TreewalkValue::Bool(left <= right)),
            BinOp::Equals => Ok(TreewalkValue::Bool(left == right)),
            BinOp::NotEquals => Ok(TreewalkValue::Bool(left != right)),
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
            BinOp::Equals => Ok(TreewalkValue::Bool(left == right)),
            BinOp::NotEquals => Ok(TreewalkValue::Bool(left != right)),
            BinOp::Is => Ok(TreewalkValue::Bool(left.is(&right))),
            BinOp::IsNot => Ok(TreewalkValue::Bool(!left.is(&right))),
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
                Ok(TreewalkValue::Int(!i))
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
            BinOp::Equals => Ok(TreewalkValue::Bool(l == r)),
            BinOp::NotEquals => Ok(TreewalkValue::Bool(l != r)),
            BinOp::LessThanOrEqual => Ok(TreewalkValue::Bool(l.subset(r))),
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
            BinOp::Equals => Ok(TreewalkValue::Bool(l == r)),
            BinOp::NotEquals => Ok(TreewalkValue::Bool(l != r)),
            _ => unimplemented!(),
        }
    }
}
