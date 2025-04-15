macro_rules! var {
    ($name:expr) => {
        Expr::Variable($name.to_string())
    };
}

macro_rules! str {
    ($name:expr) => {
        Expr::StringLiteral($name.to_string())
    };
}

macro_rules! int {
    ($val:expr) => {
        Expr::Integer($val)
    };
}

macro_rules! bool {
    ($val:expr) => {
        Expr::Boolean($val)
    };
}

macro_rules! list {
    ($($expr:expr),* $(,)?) => {
        Expr::List(vec![
            $($expr),*
        ])
    };
}

macro_rules! tuple {
    ($($expr:expr),* $(,)?) => {
        Expr::Tuple(vec![
            $($expr),*
        ])
    };
}

macro_rules! set {
    ($($expr:expr),* $(,)?) => {
        Expr::Set(HashSet::from([
            $($expr),*
        ]))
    };
}

macro_rules! stmt {
    ($variant:expr) => {
        crate::parser::types::Statement::new(1, $variant)
    };
}

macro_rules! stmt_assign {
    ($left:expr, $right:expr) => {
        stmt!(StatementKind::Assignment {
            left: $left,
            right: $right,
        })
    };
}

macro_rules! stmt_return {
    ($($expr:expr),* $(,)?) => {
        stmt!(StatementKind::Return(vec![
            $($expr),*
        ]))
    };
}

macro_rules! bin_op {
    ($left:expr, $op:ident, $right:expr) => {
        Expr::BinaryOperation {
            left: Box::new($left),
            op: BinOp::$op,
            right: Box::new($right),
        }
    };
}

macro_rules! logic_op {
    ($left:expr, $op:ident, $right:expr) => {
        Expr::LogicalOperation {
            left: Box::new($left),
            op: LogicalOp::$op,
            right: Box::new($right),
        }
    };
}

macro_rules! unary_op {
    ($op:ident, $right:expr) => {
        Expr::UnaryOperation {
            op: UnaryOp::$op,
            right: Box::new($right),
        }
    };
}

macro_rules! param {
    ($name:expr) => {
        Param {
            arg: $name.to_string(),
            default: None,
        }
    };

    ($name:expr, $default:expr) => {
        Param {
            arg: $name.to_string(),
            default: Some($default),
        }
    };
}

macro_rules! params {
    ($($expr:expr),* $(,)?) => {
        Params {
            args: vec![$($expr),*],
            args_var: None,
            kwargs_var: None,
        }
    };
}

macro_rules! call_args {
    ($($positional:expr),* $(,)?) => {
        CallArgs {
            args: vec![$($positional),*],
            kwargs: vec![],
            args_var: None,
        }
    };
}

macro_rules! member_access {
    ($object:expr, $field:expr) => {
        Expr::MemberAccess {
            object: Box::new($object),
            field: $field.to_string(),
        }
    };
}

macro_rules! lambda {
    ($args:expr, $expr:expr) => {
        Expr::Lambda {
            args: $args,
            expr: Box::new($expr),
        }
    };
}

macro_rules! func_call {
    ($name:expr) => {
        Expr::FunctionCall {
            name: $name.to_string(),
            args: call_args![],
            callee: None,
        }
    };

    ($name:expr, $args:expr) => {
        Expr::FunctionCall {
            name: $name.to_string(),
            args: $args,
            callee: None,
        }
    };

    ($name:expr, $args:expr, $callee:expr) => {
        Expr::FunctionCall {
            name: $name.to_string(),
            args: $args,
            callee: Some(Box::new($callee)),
        }
    };
}

macro_rules! method_call {
    ($object:expr, $name:expr) => {
        Expr::MethodCall {
            object: Box::new($object),
            name: $name.to_string(),
            args: call_args![],
        }
    };

    ($object:expr, $name:expr, $args:expr) => {
        Expr::MethodCall {
            object: Box::new($object),
            name: $name.to_string(),
            args: $args,
        }
    };
}

pub(crate) use bin_op;
pub(crate) use bool;
pub(crate) use call_args;
pub(crate) use func_call;
pub(crate) use int;
pub(crate) use lambda;
pub(crate) use list;
pub(crate) use logic_op;
pub(crate) use member_access;
pub(crate) use method_call;
pub(crate) use param;
pub(crate) use params;
pub(crate) use set;
pub(crate) use stmt;
pub(crate) use stmt_assign;
pub(crate) use stmt_return;
pub(crate) use str;
pub(crate) use tuple;
pub(crate) use unary_op;
pub(crate) use var;
