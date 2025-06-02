use crate::{
    core::Container,
    domain::Dunder,
    parser::types::{BinOp, LogicalOp, UnaryOp},
    treewalk::{utils::args, TreewalkInterpreter, TreewalkResult, TreewalkValue},
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

    pub fn evaluate_compare_op<F>(
        &self,
        left: TreewalkValue,
        right: TreewalkValue,
        op: F,
    ) -> TreewalkResult<TreewalkValue>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        let result = match (left, right) {
            (TreewalkValue::Int(x), TreewalkValue::Int(y)) => op(x as f64, y as f64),
            (TreewalkValue::Float(x), TreewalkValue::Float(y)) => op(x, y),
            (TreewalkValue::Int(x), TreewalkValue::Float(y)) => op(x as f64, y),
            (TreewalkValue::Float(x), TreewalkValue::Int(y)) => op(x, y as f64),
            _ => return Err(self.type_error("Unsupported operand types for numeric operation")),
        };

        Ok(TreewalkValue::Bool(result))
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

    pub fn evaluate_bin_op(
        &self,
        left: TreewalkValue,
        op: &BinOp,
        right: TreewalkValue,
    ) -> TreewalkResult<TreewalkValue> {
        use BinOp::*;

        match op {
            In => {
                let mut iterable = right.expect_iterator(self)?;
                return Ok(TreewalkValue::Bool(iterable.any(|i| i == left)));
            }
            NotIn => {
                let mut iterable = right.expect_iterator(self)?;
                return Ok(TreewalkValue::Bool(!iterable.any(|i| i == left)));
            }
            Equals => {
                if left.as_object().is_some() && right.as_object().is_some() {
                    return self.invoke_method(&left, &Dunder::Eq, args![right]);
                }
                return Ok(TreewalkValue::Bool(left == right));
            }
            NotEquals => {
                if left.as_object().is_some() && right.as_object().is_some() {
                    return self.invoke_method(&left, &Dunder::Ne, args![right]);
                }
                return Ok(TreewalkValue::Bool(left != right));
            }
            Add => {
                // List concatenation takes priority
                if left.as_list().is_some() && right.as_list().is_some() {
                    let left_list = left.expect_list(self)?;
                    let right_list = right.expect_list(self)?;
                    let l = left_list.borrow().clone();
                    let r = right_list.borrow().clone();
                    return Ok(TreewalkValue::List(Container::new(l + r)));
                }

                // Fall back to numeric addition
                return self
                    .evaluate_numeric_op(left, right, |a, b| a + b, false)
                    .map_err(|_| self.type_error("Unsupported operand types for +"));
            }
            Sub => {
                return self
                    .evaluate_numeric_op(left, right, |a, b| a - b, false)
                    .map_err(|_| self.type_error("Unsupported operand types for -"));
            }
            Mul => {
                // TODO check string multiplication first
                // Fallback to numeric multiplication
                return self
                    .evaluate_numeric_op(left, right, |a, b| a * b, false)
                    .map_err(|_| self.type_error("Unsupported operand types for *"));
            }
            LessThan => {
                return self
                    .evaluate_compare_op(left, right, |a, b| a < b)
                    .map_err(|_| self.type_error("Unsupported operand types for <"));
            }
            GreaterThan => {
                return self
                    .evaluate_compare_op(left, right, |a, b| a > b)
                    .map_err(|_| self.type_error("Unsupported operand types for >"));
            }
            LessThanOrEqual => {
                if left.as_set().is_some() && right.as_set().is_some() {
                    let left_set = left.expect_set(self)?;
                    let right_set = right.expect_set(self)?;
                    let l = left_set.borrow().clone();
                    let r = right_set.borrow().clone();
                    return Ok(TreewalkValue::Bool(l.subset(r)));
                }

                return self
                    .evaluate_compare_op(left, right, |a, b| a <= b)
                    .map_err(|_| self.type_error("Unsupported operand types for <="));
            }
            GreaterThanOrEqual => {
                return self
                    .evaluate_compare_op(left, right, |a, b| a >= b)
                    .map_err(|_| self.type_error("Unsupported operand types for >="));
            }
            Is => return Ok(TreewalkValue::Bool(left.is(&right))),
            IsNot => return Ok(TreewalkValue::Bool(!left.is(&right))),
            _ => {}
        };

        // These clauses are left over from before we did dynamic typing properly. These should
        // eventually all be incorporated in the `match op` above.
        if left.is_integer() && right.is_integer() {
            let left = left.expect_integer(self)?;
            let right = right.expect_integer(self)?;
            return self.evaluate_integer_operation(left, op, right);
        } else if left.is_fp() && right.is_fp() {
            let left = left.expect_fp(self)?;
            let right = right.expect_fp(self)?;
            return self.evaluate_floating_point_operation(left, op, right);
        }

        unreachable!()
    }

    // this function is deprecated in favor of evaluate_bin_op.
    pub fn evaluate_integer_operation(
        &self,
        left: i64,
        op: &BinOp,
        right: i64,
    ) -> TreewalkResult<TreewalkValue> {
        match op {
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
            _ => unreachable!(),
        }
    }

    // this function is deprecated in favor of evaluate_bin_op.
    pub fn evaluate_floating_point_operation(
        &self,
        left: f64,
        op: &BinOp,
        right: f64,
    ) -> TreewalkResult<TreewalkValue> {
        match op {
            BinOp::Div => {
                if right == 0.0 {
                    Err(self.div_by_zero_error("float division by zero"))
                } else {
                    Ok(TreewalkValue::Float(left / right))
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn evaluate_unary_operation(
        &self,
        op: &UnaryOp,
        right: TreewalkValue,
    ) -> TreewalkResult<TreewalkValue> {
        use UnaryOp::*;

        match op {
            Minus => right
                .negated()
                .ok_or_else(|| self.type_error("Unsupported operand type for unary '-'")),
            // this acts as a no-op. can be overridden with __pos__ for custom classes
            Plus => Ok(right),
            Not => Ok(right.not()),
            BitwiseNot => {
                let i = right.as_integer().ok_or_else(|| {
                    self.type_error(format!(
                        "bad operand type for unary ~: '{}'",
                        right.get_type()
                    ))
                })?;
                Ok(TreewalkValue::Int(!i))
            }
            Unpack => {
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
            DictUnpack => {
                todo!()
            }
        }
    }
}
