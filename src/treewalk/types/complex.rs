use crate::{
    treewalk::{types::builtins::utils::validate_args, Interpreter},
    types::errors::InterpreterError,
};
use std::fmt::{Display, Error, Formatter};

use super::{
    traits::Callable,
    utils::{Dunder, ResolvedArguments},
    ExprResult,
};

const DEFAULT_RE: f64 = 0.0;
const DEFAULT_IM: f64 = 0.0;

#[derive(Clone, PartialEq)]
pub struct Complex {
    re: f64,
    im: f64,
}

impl Complex {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }

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
    ) -> Result<ExprResult, InterpreterError> {
        let complex = match args.len() {
            1 => Complex::new(DEFAULT_RE, DEFAULT_IM),
            2 => match args.get_arg(1).as_fp() {
                Some(re) => Complex::new(re, DEFAULT_IM),
                None => {
                    let input = &args
                        .get_arg(1)
                        .as_string()
                        .ok_or(InterpreterError::TypeError(
                            Some(format!(
                                "complex() first argument must be a string or a number, not '{}'",
                                args.get_arg(1).get_type()
                            )),
                            interpreter.state.call_stack(),
                        ))?;
                    Complex::from_str(input).ok_or(InterpreterError::TypeError(
                        None,
                        interpreter.state.call_stack(),
                    ))?
                }
            },
            3 => {
                let re = args
                    .get_arg(1)
                    .as_fp()
                    .ok_or(InterpreterError::ExpectedFloatingPoint(
                        interpreter.state.call_stack(),
                    ))?;
                let im = args
                    .get_arg(2)
                    .as_fp()
                    .ok_or(InterpreterError::ExpectedFloatingPoint(
                        interpreter.state.call_stack(),
                    ))?;
                Complex::new(re, im)
            }
            _ => {
                validate_args(&args, 2, interpreter.state.call_stack())?;
                unreachable!()
            }
        };

        Ok(ExprResult::Complex(complex))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
