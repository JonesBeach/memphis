use crate::{
    domain::Dunder,
    parser::types::{BinOp, CompareOp, LogicalOp, UnaryOp},
    treewalk::{utils::args, TreewalkInterpreter, TreewalkResult, TreewalkValue},
};

impl TreewalkInterpreter {
    pub fn evaluate_logical_op(
        &self,
        left: TreewalkValue,
        op: &LogicalOp,
        right: TreewalkValue,
    ) -> TreewalkResult<TreewalkValue> {
        let left_truthy = left.coerce_to_boolean();

        match op {
            LogicalOp::And => {
                if left_truthy {
                    Ok(right)
                } else {
                    Ok(left)
                }
            }
            LogicalOp::Or => {
                if left_truthy {
                    Ok(left)
                } else {
                    Ok(right)
                }
            }
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
            Add => self.invoke_method(&left, &Dunder::Add, args![right]),
            Sub => self.invoke_method(&left, &Dunder::Sub, args![right]),
            Mul => self.invoke_method(&left, &Dunder::Mul, args![right]),
            Div => self.invoke_method(&left, &Dunder::Truediv, args![right]),
            IntegerDiv => self.invoke_method(&left, &Dunder::Floordiv, args![right]),
            Mod => self.invoke_method(&left, &Dunder::Mod, args![right]),
            BitwiseAnd => self.invoke_method(&left, &Dunder::And, args![right]),
            BitwiseOr => self.invoke_method(&left, &Dunder::Or, args![right]),
            BitwiseXor => self.invoke_method(&left, &Dunder::Xor, args![right]),
            LeftShift => self.invoke_method(&left, &Dunder::Lshift, args![right]),
            RightShift => self.invoke_method(&left, &Dunder::Rshift, args![right]),
            Expo => self.invoke_method(&left, &Dunder::Pow, args![right]),
            MatMul => todo!(),
        }
    }

    pub fn evaluate_compare_op(
        &self,
        left: TreewalkValue,
        op: &CompareOp,
        right: TreewalkValue,
    ) -> TreewalkResult<TreewalkValue> {
        use CompareOp::*;

        match op {
            // For In and NotIn, Python semantics are reversed.
            // a in [b] calls [b].__contains__(a)
            In => self.invoke_method(&right, &Dunder::Contains, args![left]),
            NotIn => {
                let contains = self.invoke_method(&right, &Dunder::Contains, args![left])?;
                Ok(contains.not())
            }
            Equals => self.invoke_method(&left, &Dunder::Eq, args![right]),
            NotEquals => self.invoke_method(&left, &Dunder::Ne, args![right]),
            LessThan => self.invoke_method(&left, &Dunder::Lt, args![right]),
            GreaterThan => self.invoke_method(&left, &Dunder::Gt, args![right]),
            LessThanOrEqual => self.invoke_method(&left, &Dunder::Le, args![right]),
            GreaterThanOrEqual => self.invoke_method(&left, &Dunder::Ge, args![right]),
            Is => Ok(TreewalkValue::Bool(left.is(&right))),
            IsNot => Ok(TreewalkValue::Bool(!left.is(&right))),
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
                let i = right.as_int().map_err(|_| {
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
                    .map_err(|_| {
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
