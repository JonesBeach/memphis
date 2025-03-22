use super::types::{Statement, StatementKind};

#[allow(unused)]
pub fn stmt(kind: StatementKind) -> Statement {
    Statement::new(1, kind)
}

#[macro_export]
macro_rules! var {
    ($name:expr) => {
        Expr::Variable($name.to_string())
    };
}

#[macro_export]
macro_rules! str {
    ($name:expr) => {
        Expr::StringLiteral($name.to_string())
    };
}

#[macro_export]
macro_rules! int {
    ($val:expr) => {
        Expr::Integer($val)
    };
}

#[macro_export]
macro_rules! list {
        ($($expr:expr),* $(,)?) => {
            Expr::List(vec![
                $($expr),*
            ])
        };
    }

#[macro_export]
macro_rules! tuple {
        ($($expr:expr),* $(,)?) => {
            Expr::Tuple(vec![
                $($expr),*
            ])
        };
    }

#[macro_export]
macro_rules! set {
        ($($expr:expr),* $(,)?) => {
            Expr::Set(HashSet::from([
                $($expr),*
            ]))
        };
    }

#[macro_export]
macro_rules! stmt_assign {
    ($left:expr, $right:expr) => {
        stmt(StatementKind::Assignment {
            left: $left,
            right: $right,
        })
    };
}

#[macro_export]
macro_rules! bin_op {
    ($left:expr, $op:ident, $right:expr) => {
        Expr::BinaryOperation {
            left: Box::new($left),
            op: BinOp::$op,
            right: Box::new($right),
        }
    };
}

#[macro_export]
macro_rules! logic_op {
    ($left:expr, $op:ident, $right:expr) => {
        Expr::LogicalOperation {
            left: Box::new($left),
            op: LogicalOp::$op,
            right: Box::new($right),
        }
    };
}

#[macro_export]
macro_rules! unary_op {
    ($op:ident, $right:expr) => {
        Expr::UnaryOperation {
            op: UnaryOp::$op,
            right: Box::new($right),
        }
    };
}

#[macro_export]
macro_rules! parsed_args {
        ($($positional:expr),* $(,)?) => {
            ParsedArguments {
                args: vec![$($positional),*],
                kwargs: vec![],
                args_var: None,
            }
        };
    }

#[macro_export]
macro_rules! member_access {
    ($object:expr, $field:expr) => {
        Expr::MemberAccess {
            object: Box::new($object),
            field: $field.to_string(),
        }
    };
}

#[macro_export]
macro_rules! func_call {
    ($name:expr, $args:expr) => {
        Expr::FunctionCall {
            name: $name.to_string(),
            args: $args,
            callee: None,
        }
    };
}
