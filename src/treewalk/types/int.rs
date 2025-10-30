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

pub struct Int;

impl_typed!(Int, Type::Int);
impl_method_provider!(
    Int,
    [
        NewBuiltin,
        AddBuiltin,
        SubBuiltin,
        MulBuiltin,
        TruedivBuiltin,
        FloordivBuiltin,
        ModBuiltin,
        AndBuiltin,
        OrBuiltin,
        XorBuiltin,
        LshiftBuiltin,
        RshiftBuiltin,
        PowBuiltin,
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
struct FloordivBuiltin;
#[derive(Clone)]
struct ModBuiltin;
#[derive(Clone)]
struct AndBuiltin;
#[derive(Clone)]
struct OrBuiltin;
#[derive(Clone)]
struct XorBuiltin;
#[derive(Clone)]
struct LshiftBuiltin;
#[derive(Clone)]
struct RshiftBuiltin;
#[derive(Clone)]
struct PowBuiltin;
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

        let int = match args.len() {
            1 => 0,
            2 => args.get_arg(1).coerce_to_int().raise(interpreter)?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Int(int))
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
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a + b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Float((a as f64) + b))
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
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a - b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Float((a as f64) - b))
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
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a * b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Float((a as f64) * b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for *"))
        }
    }

    fn name(&self) -> String {
        Dunder::Mul.into()
    }
}

impl Callable for TruedivBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            if b == 0 {
                return Err(interpreter.div_by_zero_error("integer division or modulo by zero"));
            }
            Ok(TreewalkValue::Float((a as f64) / (b as f64)))
        } else if let TreewalkValue::Float(b) = b {
            if b == 0.0 {
                return Err(interpreter.div_by_zero_error("integer division or modulo by zero"));
            }
            Ok(TreewalkValue::Float((a as f64) / b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for /"))
        }
    }

    fn name(&self) -> String {
        Dunder::Truediv.into()
    }
}

impl Callable for FloordivBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            if b == 0 {
                return Err(interpreter.div_by_zero_error("integer division or modulo by zero"));
            }
            Ok(TreewalkValue::Int(a / b))
        } else if let TreewalkValue::Float(b) = b {
            if b == 0.0 {
                return Err(interpreter.div_by_zero_error("integer division or modulo by zero"));
            }
            Ok(TreewalkValue::Float((a as f64 / b).floor()))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for //"))
        }
    }

    fn name(&self) -> String {
        Dunder::Floordiv.into()
    }
}

impl Callable for ModBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            if b == 0 {
                return Err(interpreter.div_by_zero_error("integer division or modulo by zero"));
            }
            Ok(TreewalkValue::Int(a % b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for %"))
        }
    }

    fn name(&self) -> String {
        Dunder::Mod.into()
    }
}

impl Callable for AndBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a & b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for &"))
        }
    }

    fn name(&self) -> String {
        Dunder::And.into()
    }
}

impl Callable for OrBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a | b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for |"))
        }
    }

    fn name(&self) -> String {
        Dunder::Or.into()
    }
}

impl Callable for XorBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a ^ b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for ^"))
        }
    }

    fn name(&self) -> String {
        Dunder::Xor.into()
    }
}

impl Callable for LshiftBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            if b > 100 {
                // TODO support long ranges. This is found in _collections_abc.py
                // longrange_iterator = type(iter(range(1 << 1000)))
                Ok(TreewalkValue::Int(a << 10))
            } else {
                Ok(TreewalkValue::Int(a << b))
            }
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for <<"))
        }
    }

    fn name(&self) -> String {
        Dunder::Lshift.into()
    }
}

impl Callable for RshiftBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a >> b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for >>"))
        }
    }

    fn name(&self) -> String {
        Dunder::Rshift.into()
    }
}

impl Callable for PowBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            let b: u32 = b.try_into().map_err(|_| interpreter.runtime_error())?;
            Ok(TreewalkValue::Int(a.pow(b)))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for **"))
        }
    }

    fn name(&self) -> String {
        Dunder::Pow.into()
    }
}

impl Callable for LtBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a < b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool((a as f64) < b))
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
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a <= b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool((a as f64) <= b))
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
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a > b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool((a as f64) > b))
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
            .as_int()
            .raise(interpreter)?;
        let b = args.get_arg(0);

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a >= b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool((a as f64) >= b))
        } else {
            Err(interpreter.type_error("unsupported operand type(s) for >="))
        }
    }

    fn name(&self) -> String {
        Dunder::Ge.into()
    }
}
