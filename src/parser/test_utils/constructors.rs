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

macro_rules! dict_pair {
    ($key:expr, $val:expr) => {
        $crate::parser::types::DictOperation::Pair($key, $val)
    };
}

macro_rules! dict_unpack {
    ($val:expr) => {
        $crate::parser::types::DictOperation::Unpack($val)
    };
}

macro_rules! dict {
    ($($expr:expr),* $(,)?) => {
        $crate::parser::types::Expr::Dict(vec![
            $($expr),*
        ])
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

macro_rules! cmp_op {
    ($left:expr, $op:ident, $right:expr) => {
        $crate::parser::types::Expr::ComparisonChain {
            left: Box::new($left),
            ops: vec![($crate::parser::types::CompareOp::$op, $right)],
        }
    };
}

macro_rules! cmp_chain {
    ($left:expr, [ $( ($op:ident, $right:expr) ),+ $(,)? ]) => {
        Expr::ComparisonChain {
            left: Box::new($left),
            ops: vec![ $( ($crate::parser::types::CompareOp::$op, $right) ),+ ],
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

macro_rules! slice {
    ($start:expr, $stop:expr, $step:expr) => {
        $crate::parser::types::SliceParams {
            start: $start,
            stop: $stop,
            step: $step,
        }
    };
}

macro_rules! slice_op {
    ($object:expr, $params:expr) => {
        $crate::parser::types::Expr::SliceOperation {
            object: Box::new($object),
            params: Box::new($params),
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

macro_rules! await_expr {
    ($right:expr) => {
        $crate::parser::types::Expr::Await(Box::new($right))
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

/// Convenience macro for `obj.method()` and `obj.method(args)`.
/// Expands to `func_call_callee!(member_access!(obj, method), args)`.
macro_rules! method_call {
    ($obj:expr, $method:expr) => {
        func_call_callee!(member_access!($obj, $method))
    };
    ($obj:expr, $method:expr, $args:expr) => {
        func_call_callee!(member_access!($obj, $method), $args)
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

macro_rules! import {
    ($module:expr) => {
        $crate::parser::types::RegularImport {
            import_path: ImportPath::from($module),
            alias: None,
        }
    };
    ($module:expr, $alias:expr) => {
        $crate::parser::types::RegularImport {
            import_path: ImportPath::from($module),
            alias: Some($alias.into()),
        }
    };
}

pub(crate) use await_expr;
pub(crate) use bin_op;
pub(crate) use bool;
pub(crate) use call_args;
pub(crate) use cmp_chain;
pub(crate) use cmp_op;
pub(crate) use dict;
pub(crate) use dict_pair;
pub(crate) use dict_unpack;
pub(crate) use float;
pub(crate) use func_call;
pub(crate) use func_call_callee;
pub(crate) use import;
pub(crate) use int;
pub(crate) use lambda;
pub(crate) use list;
pub(crate) use logic_op;
pub(crate) use member_access;
pub(crate) use method_call;
pub(crate) use param;
pub(crate) use params;
pub(crate) use set;
pub(crate) use slice;
pub(crate) use slice_op;
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
