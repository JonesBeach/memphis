use std::{collections::HashMap, slice::Iter};

use crate::{
    parser::types::{CallArgs, KwargsOperation},
    treewalk::{
        protocols::TryEvalFrom,
        types::{Dict, Str, Tuple},
        SymbolTable, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// Represents the fully resolved parameter state for all positional and keyword arguments.
///
/// For the unresolved state, see [`CallArgs`].
#[derive(Default, Debug, Clone)]
pub struct Args {
    bound_val: Option<TreewalkValue>,
    args: Vec<TreewalkValue>,
    kwargs: SymbolTable,
}

impl Args {
    pub fn from(interpreter: &TreewalkInterpreter, call_args: &CallArgs) -> TreewalkResult<Self> {
        let mut positional = call_args
            .args
            .iter()
            .map(|arg| interpreter.evaluate_expr(arg))
            .collect::<Result<Vec<_>, _>>()?;

        if let Some(ref args_var) = call_args.args_var {
            let value = interpreter.evaluate_expr(args_var)?;
            let args = Tuple::try_eval_from(value, interpreter)?;
            // Clone each item in place without an intermediate Vec
            positional.extend_from_slice(args.items());
        };

        #[allow(clippy::mutable_key_type)]
        let mut kwargs = HashMap::default();
        for kwarg in call_args.kwargs.iter() {
            match kwarg {
                KwargsOperation::Pair(key, value) => {
                    kwargs.insert(key.to_string(), interpreter.evaluate_expr(value)?);
                }
                KwargsOperation::Unpacking(expr) => {
                    let unpacked = interpreter.evaluate_expr(expr)?;
                    for key in unpacked.expect_iterable(interpreter)? {
                        let key_str = key.expect_string(interpreter)?;
                        if kwargs.contains_key(&key_str) {
                            return Err(interpreter.key_error(key_str.to_string()));
                        }
                        let value = interpreter.read_index(&unpacked, &key)?;
                        kwargs.insert(key_str, value);
                    }
                }
            }
        }

        Ok(Self::new(positional, kwargs))
    }

    #[allow(clippy::mutable_key_type)]
    pub fn new(args: Vec<TreewalkValue>, kwargs: HashMap<String, TreewalkValue>) -> Self {
        Self {
            bound_val: None,
            args,
            kwargs,
        }
    }

    pub fn add_arg(&mut self, arg: TreewalkValue) {
        self.args.push(arg);
    }

    pub fn with_bound(mut self, val: TreewalkValue) -> Self {
        self.bind(val);
        self
    }

    pub fn with_bound_new(mut self, val: TreewalkValue) -> Self {
        self.bind_new(val);
        self
    }

    fn bind(&mut self, val: TreewalkValue) {
        self.bound_val = Some(val);
    }

    /// The `Dunder::New` method expects the class to be passed in as the first argument but in
    /// an unbound way.
    fn bind_new(&mut self, val: TreewalkValue) {
        self.args.insert(0, val);
    }

    pub fn get_self(&self) -> Option<TreewalkValue> {
        self.bound_val.clone()
    }

    pub fn expect_self(&self, interpreter: &TreewalkInterpreter) -> TreewalkResult<TreewalkValue> {
        self.get_self()
            .ok_or_else(|| interpreter.type_error("Unbound call!"))
    }

    /// Access a positional argument by index. Bound arguments are not included in this, use
    /// `get_self` for those.
    pub fn get_arg(&self, index: usize) -> TreewalkValue {
        self.args[index].clone()
    }

    pub fn get_arg_mut(&mut self, index: usize) -> &mut TreewalkValue {
        &mut self.args[index]
    }

    pub fn get_arg_optional(&self, index: usize) -> Option<TreewalkValue> {
        self.args.get(index).cloned()
    }

    pub fn has_kwargs(&self) -> bool {
        !self.kwargs.is_empty()
    }

    pub fn get_kwargs_dict(&self, interpreter: &TreewalkInterpreter) -> Dict {
        #[allow(clippy::mutable_key_type)]
        let mut uptyped_kwargs = HashMap::new();
        for (key, value) in &self.kwargs {
            uptyped_kwargs.insert(TreewalkValue::Str(Str::new(key)), value.clone());
        }
        Dict::new(interpreter, uptyped_kwargs)
    }

    pub fn get_kwargs(&self) -> &SymbolTable {
        &self.kwargs
    }

    /// Access a keyword argument by key.
    pub fn get_kwarg(&self, key: &str) -> Option<TreewalkValue> {
        self.kwargs.get(key).cloned()
    }

    pub fn has_kwarg(&self, key: &str) -> bool {
        self.kwargs.contains_key(key)
    }

    pub fn len(&self) -> usize {
        self.args.len()
    }

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    pub fn iter_args(&self) -> Iter<'_, TreewalkValue> {
        self.args.iter()
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
macro_rules! args {
    () => {{
        Args::default()
    }};
    // Double curly braces ensure that the entire macro expands into a single expression, which is
    // necessary since we are returning a value from this macro.
    ( $( $arg:expr ),* ) => {{
        let mut args = $crate::treewalk::utils::Args::default();
        $(
            args.add_arg($arg);
        )*
        args
    }};
}

pub(crate) use args;

pub fn check_args<F>(
    args: &Args,
    condition: F,
    interpreter: &TreewalkInterpreter,
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
