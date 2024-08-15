use std::{
    fmt::{Debug, Display, Error, Formatter},
    hash::{Hash, Hasher},
    ops::Deref,
};

use crate::{
    core::InterpreterEntrypoint,
    resolved_args,
    treewalk::{types::ExprResult, Interpreter},
    types::errors::{InterpreterError, MemphisError},
};

use super::{Dunder, ResolvedArguments};

/// A wrapper which includes a reference to an `Interpreter`.
///
/// This struct is used in cases where the expression result needs to evaluate
/// methods using the interpreter over its lifetime, such as when implementing traits like
/// `PartialEq` or `Hash`.
///
/// # Notes
/// - This struct should be used sparingly, only in contexts where interpreter access is
///   necessary. In other cases, prefer using simpler types to avoid unnecessary coupling.
#[derive(Clone)]
pub struct Contextual<T> {
    value: T,

    /// Store a pointer to the interpreter so that we can evaluate the `Hash`/`Eq` traits.
    interpreter: Interpreter,
}

impl<T> Contextual<T> {
    pub fn new(value: T, interpreter: Interpreter) -> Self {
        Self { value, interpreter }
    }

    pub fn interpreter(&self) -> &Interpreter {
        &self.interpreter
    }
}

impl<T> Deref for Contextual<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> Display for Contextual<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.value)
    }
}

impl<T> Debug for Contextual<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Reuse the Display implementation for Debug
        Display::fmt(self, f)
    }
}

impl Contextual<ExprResult> {
    /// Use the interpreter to evaluate equality
    fn equals(&self, other: &Self) -> bool {
        let result = self.interpreter.evaluate_method(
            self.value.clone(),
            &Dunder::Eq,
            &resolved_args!(other.value.clone()),
        );

        match result {
            Ok(ExprResult::Boolean(true)) => true,
            Ok(_) => false,
            Err(e) => self
                .interpreter
                .handle_runtime_error(MemphisError::Interpreter(e)),
        }
    }

    /// Use the interpreter to evaluate the hash
    fn hash(&self) -> u64 {
        let result =
            self.interpreter
                .evaluate_method(self.value.clone(), &Dunder::Hash, &resolved_args!());

        match result {
            Ok(ExprResult::Integer(hash_val)) => *hash_val.borrow() as u64,
            Ok(_) => self
                .interpreter
                .handle_runtime_error(MemphisError::Interpreter(
                    InterpreterError::ExpectedInteger(self.interpreter.state.call_stack()),
                )),
            Err(e) => self
                .interpreter
                .handle_runtime_error(MemphisError::Interpreter(e)),
        }
    }
}

impl PartialEq for Contextual<ExprResult> {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}

impl Eq for Contextual<ExprResult> {}

impl Hash for Contextual<ExprResult> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash());
    }
}
