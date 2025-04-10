use std::collections::HashMap;

use crate::{
    parser::types::{CallArgs, KwargsOperation},
    treewalk::{types::Str, Interpreter, TreewalkResult, TreewalkValue},
};

/// Represents the fully resolved parameter state for all positional and keyword arguments.
///
/// For the unresolved state, see [`CallArgs`].
#[derive(Default, Debug, Clone)]
pub struct Arguments {
    bound_val: Option<TreewalkValue>,
    args: Vec<TreewalkValue>,
    kwargs: HashMap<TreewalkValue, TreewalkValue>,
}

impl Arguments {
    pub fn from(interpreter: &Interpreter, arguments: &CallArgs) -> TreewalkResult<Self> {
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
                        TreewalkValue::String(Str::new(key.to_string())),
                        interpreter.evaluate_expr(value)?,
                    );
                }
                KwargsOperation::Unpacking(expr) => {
                    let unpacked = interpreter.evaluate_expr(expr)?;
                    for key in unpacked.clone() {
                        if kwargs.contains_key(&key) {
                            return Err(interpreter.key_error(key.to_string()));
                        }
                        let value = interpreter.read_index(&unpacked, &key)?;
                        kwargs.insert(key, value);
                    }
                }
            }
        }

        let mut second_arg_values = if let Some(ref args_var) = arguments.args_var {
            let args_var_value = interpreter.evaluate_expr(args_var)?;
            let args = args_var_value.expect_tuple(interpreter)?;
            args.raw()
        } else {
            vec![]
        };
        arg_values.append(&mut second_arg_values);

        Ok(Self::new(arg_values, kwargs))
    }

    #[allow(clippy::mutable_key_type)]
    pub fn new(args: Vec<TreewalkValue>, kwargs: HashMap<TreewalkValue, TreewalkValue>) -> Self {
        Self {
            bound_val: None,
            args,
            kwargs,
        }
    }

    pub fn add_arg(&mut self, arg: TreewalkValue) -> Self {
        self.args.push(arg);
        self.clone()
    }

    pub fn bind(&mut self, val: TreewalkValue) {
        self.bound_val = Some(val);
    }

    /// The `Dunder::New` method expects the class to be passed in as the first argument but in
    /// an unbound way.
    pub fn bind_new(&mut self, val: TreewalkValue) {
        self.args.insert(0, val);
    }

    pub fn get_self(&self) -> Option<TreewalkValue> {
        self.bound_val.clone()
    }

    pub fn expect_self(&self, interpreter: &Interpreter) -> TreewalkResult<TreewalkValue> {
        self.get_self()
            .ok_or_else(|| interpreter.type_error("Unbound call!"))
    }

    /// Access a positional argument by index. Bound arguments are not included in this, use
    /// `get_self` for those.
    pub fn get_arg(&self, index: usize) -> TreewalkValue {
        self.args[index].clone()
    }

    pub fn get_arg_optional(&self, index: usize) -> Option<TreewalkValue> {
        self.args.get(index).cloned()
    }

    /// Return a `Dict` of all the keyword arguments.
    #[allow(clippy::mutable_key_type)]
    pub fn get_kwargs(&self) -> HashMap<TreewalkValue, TreewalkValue> {
        self.kwargs.clone()
    }

    /// Access a keyword argument by key.
    pub fn get_kwarg(&self, key: &TreewalkValue) -> Option<TreewalkValue> {
        self.kwargs.get(key).cloned()
    }

    pub fn len(&self) -> usize {
        self.args.len()
    }

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    pub fn iter_args(&self) -> std::slice::Iter<'_, TreewalkValue> {
        self.args.iter()
    }

    pub fn bound_len(&self) -> usize {
        self.bound_args().len()
    }

    /// When we are loading a symbol table for a new scope, we must join the bound object with
    /// the positional arguments since the function signature will expect the bound object.
    pub fn bound_args(&self) -> Vec<TreewalkValue> {
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
macro_rules! args {
    // Double curly braces ensure that the entire macro expands into a single expression, which is
    // necessary since we are returning a value from this macro.
    ( $( $arg:expr ),* ) => {{
        // When no args are pass, this gives an unused mut warning
        #[allow(unused_mut)]
        let mut args = Arguments::default();
        $(
            args = args.add_arg($arg);
        )*
        args
    }};
}

pub fn check_args<F>(
    args: &Arguments,
    condition: F,
    interpreter: &Interpreter,
) -> TreewalkResult<()>
where
    F: Fn(usize) -> bool,
{
    if !condition(args.len()) {
        Err(interpreter.type_error(format!("Found {} args", args.len())))
    } else {
        Ok(())
    }
}
