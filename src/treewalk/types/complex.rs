use crate::treewalk::interpreter::TreewalkResult;
use std::fmt::{Display, Error, Formatter};

use crate::{domain::Dunder, treewalk::Interpreter};

use super::{
    domain::{
        builtins::utils::validate_args,
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    utils::ResolvedArguments,
    ExprResult,
};

const DEFAULT_RE: f64 = 0.0;
const DEFAULT_IM: f64 = 0.0;

#[derive(Clone, PartialEq)]
pub struct Complex {
    re: f64,
    im: f64,
}

impl Typed for Complex {
    fn get_type() -> Type {
        Type::Complex
    }
}

impl MethodProvider for Complex {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Complex {
    pub fn new(re: f64, im: f64) -> Self {
        Self { re, im }
    }

    fn from_str(input: &str) -> Option<Self> {
        // Remove the trailing 'j' character
        if !input.ends_with('j') {
            return None;
        }
        let input = &input[..input.len() - 1];

        // Find the position of the '+' or '-' sign for the imaginary part
        let mut split_pos = None;
        for (i, c) in input.char_indices().rev() {
            if c == '+' || c == '-' {
                split_pos = Some(i);
                break;
            }
        }

        let (real_str, imag_str) = input.split_at(split_pos?);

        let real_part = real_str.parse::<f64>().ok()?;
        let imag_part = imag_str.parse::<f64>().ok()?;

        Some(Self::new(real_part, imag_part))
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.re == DEFAULT_RE {
            write!(f, "{}j", self.im)
        } else {
            write!(f, "({}+{}j)", self.re, self.im)
        }
    }
}

/// The __new__ method directly creates a complex number with the given parameters. For an
/// immutable built-in type like complex, the __init__ method typically does nothing so we do not
/// need to add it here. This is because the complex object is already fully initialized by the
/// time __init__ is called, and since it is immutable, its state cannot be changed after creation.
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        validate_args(&args, |len| [1, 2, 3].contains(&len), interpreter)?;

        let complex = match args.len() {
            1 => Complex::new(DEFAULT_RE, DEFAULT_IM),
            2 => match args.get_arg(1).as_fp() {
                Some(re) => Complex::new(re, DEFAULT_IM),
                None => {
                    let input = &args.get_arg(1).as_string().ok_or_else(|| {
                        interpreter.type_error(format!(
                            "complex() first argument must be a string or a number, not '{}'",
                            args.get_arg(1).get_type()
                        ))
                    })?;
                    Complex::from_str(input)
                        .ok_or_else(|| interpreter.type_error("Expected a complex number"))?
                }
            },
            3 => {
                let re = args.get_arg(1).expect_fp(interpreter)?;
                let im = args.get_arg(2).expect_fp(interpreter)?;
                Complex::new(re, im)
            }
            _ => unreachable!(),
        };

        Ok(ExprResult::Complex(complex))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
