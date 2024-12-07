use std::collections::HashMap;

use crate::{
    parser::types::{KwargsOperation, ParsedArguments},
    treewalk::{
        types::{ExprResult, Str},
        Interpreter,
    },
    types::errors::InterpreterError,
};

/// Represents the fully resolved parameter state for all positional and keyword arguments.
///
/// For the unresolved state, see [`ParsedArguments`].
#[derive(Default, Debug, Clone)]
pub struct ResolvedArguments {
    bound_val: Option<ExprResult>,
    args: Vec<ExprResult>,
    kwargs: HashMap<ExprResult, ExprResult>,
}

impl ResolvedArguments {
    pub fn from(
        interpreter: &Interpreter,
        arguments: &ParsedArguments,
    ) -> Result<Self, InterpreterError> {
        let mut arg_values = arguments
            .args
            .iter()
            .map(|arg| interpreter.evaluate_expr(arg))
            .collect::<Result<Vec<_>, _>>()?;

        #[allow(clippy::mutable_key_type)]
        let mut kwargs = HashMap::default();
        for kwarg in arguments.kwargs.iter() {
            match kwarg {
                KwargsOperation::Pair(key, value) => {
                    kwargs.insert(
                        ExprResult::String(Str::new(key.to_string())),
                        interpreter.evaluate_expr(value)?,
                    );
                }
                KwargsOperation::Unpacking(expr) => {
                    let unpacked = interpreter.evaluate_expr(expr)?;
                    for key in unpacked.clone() {
                        if kwargs.contains_key(&key) {
                            return Err(InterpreterError::KeyError(
                                key.to_string(),
                                interpreter.state.call_stack(),
                            ));
                        }
                        let value = interpreter.read_index(&unpacked, &key)?;
                        kwargs.insert(key, value);
                    }
                }
            }
        }

        let mut second_arg_values = if let Some(ref args_var) = arguments.args_var {
            let args_var_value = interpreter.evaluate_expr(args_var)?;
            let args = args_var_value
                .as_tuple()
                .ok_or(InterpreterError::ExpectedTuple(
                    interpreter.state.call_stack(),
                ))?;
            args.clone().borrow().raw()
        } else {
            vec![]
        };
        arg_values.append(&mut second_arg_values);

        Ok(Self::new(arg_values, kwargs))
    }

    #[allow(clippy::mutable_key_type)]
    pub fn new(args: Vec<ExprResult>, kwargs: HashMap<ExprResult, ExprResult>) -> Self {
        Self {
            bound_val: None,
            args,
            kwargs,
        }
    }

    pub fn add_arg(&mut self, arg: ExprResult) -> Self {
        self.args.push(arg);
        self.clone()
    }

    pub fn bind(&mut self, val: ExprResult) {
        self.bound_val = Some(val);
    }

    /// The `Dunder::New` method expects the class to be passed in as the first argument but in
    /// an unbound way.
    pub fn bind_new(&mut self, val: ExprResult) {
        self.args.insert(0, val);
    }

    pub fn get_self(&self) -> Option<ExprResult> {
        self.bound_val.clone()
    }

    /// Access a positional argument by index. Bound arguments are not included in this, use
    /// `get_self` for those.
    pub fn get_arg(&self, index: usize) -> ExprResult {
        self.args[index].clone()
    }

    pub fn get_arg_optional(&self, index: usize) -> Option<ExprResult> {
        self.args.get(index).cloned()
    }

    /// Return a `Dict` of all the keyword arguments.
    #[allow(clippy::mutable_key_type)]
    pub fn get_kwargs(&self) -> HashMap<ExprResult, ExprResult> {
        self.kwargs.clone()
    }

    /// Access a keyword argument by key.
    pub fn get_kwarg(&self, key: &ExprResult) -> Option<ExprResult> {
        self.kwargs.get(key).cloned()
    }

    pub fn len(&self) -> usize {
        self.args.len()
    }

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    pub fn iter_args(&self) -> std::slice::Iter<'_, ExprResult> {
        self.args.iter()
    }

    pub fn bound_len(&self) -> usize {
        self.bound_args().len()
    }

    /// When we are loading a symbol table for a new scope, we must join the bound object with
    /// the positional arguments since the function signature will expect the bound object.
    pub fn bound_args(&self) -> Vec<ExprResult> {
        let mut base = if let Some(bound) = self.bound_val.clone() {
            vec![bound]
        } else {
            vec![]
        };
        base.append(&mut self.args.clone());
        base
    }
}

/// This macro is useful to create `ResolvedArguments` when you only need positional arguments.
/// When kwargs are needed, you can use `ResolvedArguments::new`.
#[macro_export]
macro_rules! resolved_args {
    // Double curly braces ensure that the entire macro expands into a single expression, which is
    // necessary since we are returning a value from this macro.
    ( $( $arg:expr ),* ) => {{
        // When no args are pass, this gives an unused mut warning
        #[allow(unused_mut)]
        let mut args = ResolvedArguments::default();
        $(
            args = args.add_arg($arg);
        )*
        args
    }};
}
