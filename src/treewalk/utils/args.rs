use std::{collections::HashMap, slice::Iter};

use crate::{
    domain::{DomainResult, ExecutionError},
    treewalk::{
        types::{Dict, Str},
        SymbolTable, TreewalkInterpreter, TreewalkValue,
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

    pub fn with_bound_receiver(mut self, val: Option<TreewalkValue>) -> Self {
        if let Some(receiver) = val {
            self.bind(receiver);
            self
        } else {
            self
        }
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

    pub fn get_self(&self) -> DomainResult<TreewalkValue> {
        match &self.bound_val {
            Some(b) => Ok(b.clone()),
            None => Err(ExecutionError::type_error(
                "Unbound method needs an argument",
            )),
        }
    }

    /// Access a positional argument by index. Bound arguments are not included in this, use
    /// `get_self` for those.
    pub fn get_arg(&self, index: usize) -> TreewalkValue {
        self.args[index].clone()
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

pub fn check_args<F>(args: &Args, condition: F) -> DomainResult<()>
where
    F: Fn(usize) -> bool,
{
    if !condition(args.len()) {
        Err(ExecutionError::type_error(format!(
            "Found {} args",
            args.len()
        )))
    } else {
        Ok(())
    }
}
