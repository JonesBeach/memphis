use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        result::Raise,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

pub struct Float;

impl_typed!(Float, Type::Float);
impl_method_provider!(
    Float,
    [
        NewBuiltin,
        AddBuiltin,
        SubBuiltin,
        MulBuiltin,
        TruedivBuiltin,
        LtBuiltin,
        LeBuiltin,
        GtBuiltin,
        GeBuiltin,
    ]
);

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct AddBuiltin;
#[derive(Clone)]
struct SubBuiltin;
#[derive(Clone)]
struct MulBuiltin;
#[derive(Clone)]
struct TruedivBuiltin;
#[derive(Clone)]
struct LtBuiltin;
#[derive(Clone)]
struct LeBuiltin;
#[derive(Clone)]
struct GtBuiltin;
#[derive(Clone)]
struct GeBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let a = match args.len() {
            1 => 0.0,
            2 => args.get_arg(1).coerce_to_float().raise(interpreter)?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Float(a))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for AddBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_float()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Float(a + (b as f64)))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Float(a + b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for +"))
        }
    }

    fn name(&self) -> String {
        Dunder::Add.into()
    }
}

impl Callable for SubBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_float()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Float(a - (b as f64)))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Float(a - b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for -"))
        }
    }

    fn name(&self) -> String {
        Dunder::Sub.into()
    }
}

impl Callable for MulBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_float()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Float(a * (b as f64)))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Float(a * b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for *"))
        }
    }

    fn name(&self) -> String {
        Dunder::Sub.into()
    }
}

impl Callable for TruedivBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_float()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            if b == 0 {
                return Err(interpreter.div_by_zero_error("float division by zero"));
            }
            Ok(TreewalkValue::Float(a / (b as f64)))
        } else if let TreewalkValue::Float(b) = b {
            if b == 0.0 {
                return Err(interpreter.div_by_zero_error("float division by zero"));
            }
            Ok(TreewalkValue::Float(a / b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for /"))
        }
    }

    fn name(&self) -> String {
        Dunder::Truediv.into()
    }
}

impl Callable for LtBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_float()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a < b as f64))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool(a < b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for <"))
        }
    }

    fn name(&self) -> String {
        Dunder::Lt.into()
    }
}

impl Callable for LeBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_float()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a <= b as f64))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool(a <= b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for <="))
        }
    }

    fn name(&self) -> String {
        Dunder::Le.into()
    }
}

impl Callable for GtBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_float()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a > b as f64))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool(a > b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for >"))
        }
    }

    fn name(&self) -> String {
        Dunder::Gt.into()
    }
}

impl Callable for GeBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_float()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a >= b as f64))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool(a >= b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for >="))
        }
    }

    fn name(&self) -> String {
        Dunder::Ge.into()
    }
}
