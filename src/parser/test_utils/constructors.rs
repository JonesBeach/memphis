macro_rules! var {
    ($name:expr) => {
        $crate::parser::types::Expr::Variable($name.to_string())
    };
}

macro_rules! str {
    ($name:expr) => {
        $crate::parser::types::Expr::StringLiteral($name.to_string())
    };
}

macro_rules! int {
    ($val:expr) => {
        $crate::parser::types::Expr::Integer($val)
    };
}

macro_rules! float {
    ($val:expr) => {
        $crate::parser::types::Expr::Float($val)
    };
}

macro_rules! bool {
    ($val:expr) => {
        $crate::parser::types::Expr::Boolean($val)
    };
}

macro_rules! list {
    ($($expr:expr),* $(,)?) => {
        $crate::parser::types::Expr::List(vec![
            $($expr),*
        ])
    };
}

macro_rules! tuple {
    ($($expr:expr),* $(,)?) => {
        $crate::parser::types::Expr::Tuple(vec![
            $($expr),*
        ])
    };
}

macro_rules! set {
    ($($expr:expr),* $(,)?) => {
        $crate::parser::types::Expr::Set(HashSet::from([
            $($expr),*
        ]))
    };
}

macro_rules! stmt {
    ($variant:expr) => {
        $crate::parser::types::Statement::new(1, $variant)
    };
}

macro_rules! stmt_assign {
    ($left:expr, $right:expr) => {
        $crate::parser::test_utils::stmt!($crate::parser::types::StatementKind::Assignment {
            left: $left,
            right: $right,
        })
    };
}

macro_rules! stmt_expr {
    ($left:expr) => {
        $crate::parser::test_utils::stmt!($crate::parser::types::StatementKind::Expression($left))
    };
}

macro_rules! stmt_return {
    ($($expr:expr),* $(,)?) => {
        $crate::parser::test_utils::stmt!($crate::parser::types::StatementKind::Return(vec![
            $($expr),*
        ]))
    };
}

macro_rules! bin_op {
    ($left:expr, $op:ident, $right:expr) => {
        $crate::parser::types::Expr::BinaryOperation {
            left: Box::new($left),
            op: $crate::parser::types::BinOp::$op,
            right: Box::new($right),
        }
    };
}

macro_rules! logic_op {
    ($left:expr, $op:ident, $right:expr) => {
        $crate::parser::types::Expr::LogicalOperation {
            left: Box::new($left),
            op: $crate::parser::types::LogicalOp::$op,
            right: Box::new($right),
        }
    };
}

macro_rules! unary_op {
    ($op:ident, $right:expr) => {
        $crate::parser::types::Expr::UnaryOperation {
            op: $crate::parser::types::UnaryOp::$op,
            right: Box::new($right),
        }
    };
}

macro_rules! yield_expr {
    () => {
        $crate::parser::types::Expr::Yield(None)
    };

    ($right:expr) => {
        $crate::parser::types::Expr::Yield(Some(Box::new($right)))
    };
}

macro_rules! yield_from {
    ($right:expr) => {
        $crate::parser::types::Expr::YieldFrom(Box::new($right))
    };
}

macro_rules! param {
    ($name:expr) => {
        $crate::parser::types::Param {
            arg: $name.to_string(),
            default: None,
        }
    };

    ($name:expr, $default:expr) => {
        $crate::parser::types::Param {
            arg: $name.to_string(),
            default: Some($default),
        }
    };
}

macro_rules! params {
    ($($expr:expr),* $(,)?) => {
        $crate::parser::types::Params {
            args: vec![$($expr),*],
            args_var: None,
            kwargs_var: None,
        }
    };
}

macro_rules! call_args {
    ($($positional:expr),* $(,)?) => {
        $crate::parser::types::CallArgs {
            args: vec![$($positional),*],
            kwargs: vec![],
            args_var: None,
        }
    };
}

macro_rules! member_access {
    ($object:expr, $field:expr) => {
        $crate::parser::types::Expr::MemberAccess {
            object: Box::new($object),
            field: $field.to_string(),
        }
    };
}

macro_rules! lambda {
    ($args:expr, $expr:expr) => {
        $crate::parser::types::Expr::Lambda {
            args: $args,
            expr: Box::new($expr),
        }
    };
}

macro_rules! func_call {
    ($name:expr) => {
        $crate::parser::types::Expr::FunctionCall {
            callee: $crate::parser::types::Callee::Symbol($name.to_string()),
            args: call_args![],
        }
    };

    ($name:expr, $args:expr) => {
        $crate::parser::types::Expr::FunctionCall {
            callee: $crate::parser::types::Callee::Symbol($name.to_string()),
            args: $args,
        }
    };
}

macro_rules! func_call_callee {
    ($callee:expr) => {
        $crate::parser::types::Expr::FunctionCall {
            callee: $crate::parser::types::Callee::Expr(Box::new($callee)),
            args: call_args![],
        }
    };

    ($callee:expr, $args:expr) => {
        $crate::parser::types::Expr::FunctionCall {
            callee: $crate::parser::types::Callee::Expr(Box::new($callee)),
            args: $args,
        }
    };
}

pub(crate) use bin_op;
pub(crate) use bool;
pub(crate) use call_args;
pub(crate) use float;
pub(crate) use func_call;
pub(crate) use func_call_callee;
pub(crate) use int;
pub(crate) use lambda;
pub(crate) use list;
pub(crate) use logic_op;
pub(crate) use member_access;
pub(crate) use param;
pub(crate) use params;
pub(crate) use set;
pub(crate) use stmt;
pub(crate) use stmt_assign;
pub(crate) use stmt_expr;
pub(crate) use stmt_return;
pub(crate) use str;
pub(crate) use tuple;
pub(crate) use unary_op;
pub(crate) use var;
pub(crate) use yield_expr;
pub(crate) use yield_from;
