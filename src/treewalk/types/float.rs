use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
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
        TruedivBuiltin
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

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let a = match args.len() {
            1 => 0.0,
            2 => args.get_arg(1).expect_float(interpreter)?,
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

        let a = args.expect_self(interpreter)?.expect_float(interpreter)?;
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

        let a = args.expect_self(interpreter)?.expect_float(interpreter)?;
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

        let a = args.expect_self(interpreter)?.expect_float(interpreter)?;
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

        let a = args.expect_self(interpreter)?.expect_float(interpreter)?;
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
