use std::{
    fmt::{Debug, Display, Error, Formatter},
    hash::{Hash, Hasher},
    ops::Deref,
};

use crate::{
    core::memphis_utils,
    domain::{Dunder, ExecutionError, ExecutionErrorKind},
    errors::MemphisError,
    treewalk::{
        utils::{args, Args},
        TreewalkDisruption, TreewalkInterpreter, TreewalkValue,
    },
};

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
    interpreter: TreewalkInterpreter,
}

impl<T> Contextual<T> {
    pub fn new(value: T, interpreter: TreewalkInterpreter) -> Self {
        Self { value, interpreter }
    }

    pub fn interpreter(&self) -> &TreewalkInterpreter {
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

impl Contextual<TreewalkValue> {
    /// Use the interpreter to evaluate equality
    fn equals(&self, other: &Self) -> bool {
        // This isn't technically correct, but the idea here is that we should fallback to the
        // default behavior (comparing identity) if we are unable to call Dunder::Eq. We are unable
        // to call this because for class objects, this is unbound: complex.__eq__(complex). The
        // way Python actually detects this is not whether the method call is unbound or not, but
        // whether that method has been overridden (i.e. exists before 'object' in the MRO).
        //
        // The correct pseudocode is:
        //
        // if obj1 has an overridden __eq__ method:
        //    return obj1.__eq__(obj2)
        // else:
        //    return id(obj1) == id(obj2)
        //
        // Python handles an extra optimization here by comparing identity for objects without an
        // overridden __eq__ method, but we just handle that as the fallback implementation of
        // object.__eq__ which is discovered via the MRO.
        //
        // "obj1 has an overridden __eq__ method" will always evaluate to false when obj1 is a
        // class, which is what we detect by checking for the unbound case (receiver().is_none()).
        let eq = match self.interpreter.resolve_method(&self.value, Dunder::Eq) {
            Err(TreewalkDisruption::Signal(_)) => todo!(),
            Err(TreewalkDisruption::Error(e)) => memphis_utils::exit(MemphisError::Execution(e)),
            Ok(eq) => eq,
        };
        if eq.receiver().is_none() {
            return self.value == other.value;
        }

        let result =
            self.interpreter
                .invoke_method(&self.value, Dunder::Eq, args![other.value.clone()]);

        match result {
            Ok(TreewalkValue::Boolean(true)) => true,
            Ok(_) => false,
            Err(TreewalkDisruption::Signal(_)) => todo!(),
            Err(TreewalkDisruption::Error(e)) => memphis_utils::exit(MemphisError::Execution(e)),
        }
    }

    /// Use the interpreter to evaluate the hash
    fn hash(&self) -> u64 {
        let result = self
            .interpreter
            .call_function("hash", args![self.value.clone()]);

        match result {
            Ok(TreewalkValue::Integer(hash_val)) => hash_val as u64,
            Ok(_) => memphis_utils::exit(MemphisError::Execution(ExecutionError::new(
                self.interpreter.state.debug_call_stack(),
                ExecutionErrorKind::TypeError(None),
            ))),
            Err(TreewalkDisruption::Signal(_)) => todo!(),
            Err(TreewalkDisruption::Error(e)) => memphis_utils::exit(MemphisError::Execution(e)),
        }
    }
}

impl PartialEq for Contextual<TreewalkValue> {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}

impl Eq for Contextual<TreewalkValue> {}

impl Hash for Contextual<TreewalkValue> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash());
    }
}
