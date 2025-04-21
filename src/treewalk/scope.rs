use std::collections::{hash_map::Iter, HashMap, HashSet};

use crate::{
    core::Container,
    treewalk::{
        types::{Dict, DictItems, Function, Str, Tuple},
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// This represents a symbol table for a given scope.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Scope {
    symbol_table: HashMap<String, TreewalkValue>,

    /// Used to hold directives such as `global x` which will expire with this scope.
    global_vars: HashSet<String>,

    /// Used to hold directives such as `nonlocal x` which will expire with this scope.
    nonlocal_vars: HashSet<String>,
}

impl Scope {
    pub fn new(
        interpreter: &TreewalkInterpreter,
        function: &Container<Function>,
        args: &Args,
    ) -> TreewalkResult<Container<Self>> {
        let mut scope = Self::default();

        let expected_args = &function.borrow().args;

        // Function expects fewer positional args than it was invoked with and there is not an
        // `args_var` in which to store the rest.
        check_args(
            args,
            |_| !(expected_args.args.len() < args.bound_len() && expected_args.args_var.is_none()),
            interpreter,
        )?;

        let bound_args = args.bound_args();
        let mut missing_args = vec![];

        for (index, arg_definition) in expected_args.args.iter().enumerate() {
            // Check if the argument is provided, otherwise use default
            let value = if index < bound_args.len() {
                bound_args[index].clone()
            } else {
                match &arg_definition.default {
                    Some(default_value) => interpreter.evaluate_expr(default_value)?,
                    None => {
                        missing_args.push(arg_definition.arg.clone());
                        // We use None here only because if we hit this case, we will return an
                        // error shortly after this loop. We can't do it here because we need to
                        // find all the missing args first.
                        TreewalkValue::None
                    }
                }
            };

            scope.insert(&arg_definition.arg, value);
        }

        // Function expects more positional args than it was invoked with.
        if !missing_args.is_empty() {
            let num_missing = missing_args.len();
            let noun = if num_missing == 1 {
                "argument"
            } else {
                "arguments"
            };
            let arg_names = missing_args
                .into_iter()
                .map(|a| format!("'{}'", a))
                .collect::<Vec<_>>()
                .join(" and ");
            return Err(interpreter.type_error(format!(
                "{}() missing {} required positional {}: {}",
                function.borrow().name(),
                num_missing,
                noun,
                arg_names
            )));
        }

        if let Some(ref args_var) = expected_args.args_var {
            let extra = args.len() - expected_args.args.len();
            let left_over = bound_args.iter().rev().take(extra).rev().cloned().collect();
            let args_value = TreewalkValue::Tuple(Tuple::new(left_over));
            scope.insert(args_var.as_str(), args_value);
        }

        if let Some(ref kwargs_var) = expected_args.kwargs_var {
            let kwargs_value =
                TreewalkValue::Dict(Container::new(Dict::new(interpreter, args.get_kwargs())));
            scope.insert(kwargs_var.as_str(), kwargs_value);
        }

        Ok(Container::new(scope.to_owned()))
    }

    pub fn get(&self, name: &str) -> Option<TreewalkValue> {
        self.symbol_table.get(name).cloned()
    }

    /// Return a list of all the symbols available in this `Scope`.
    pub fn symbols(&self) -> Vec<String> {
        self.symbol_table.keys().cloned().collect()
    }

    pub fn delete(&mut self, name: &str) -> Option<TreewalkValue> {
        self.symbol_table.remove(name)
    }

    /// Insert an `TreewalkValue` to this `Scope`. The `Scope` is returned to allow calls to be
    /// chained.
    pub fn insert(&mut self, name: &str, value: TreewalkValue) -> &mut Self {
        self.symbol_table.insert(name.to_string(), value);
        self
    }

    /// Given a variable `var`, indicate that `var` should refer to the variable in the
    /// global/module scope (which does not live in this struct) for the duration of _this_
    /// local scope.
    pub fn mark_global(&mut self, name: &str) {
        self.global_vars.insert(name.to_string());
    }

    /// Given a variable `var`, indicate that `var` should refer to the variable in the
    /// enclosing scope (which does not live in this struct) for the duration of _this_
    /// local scope.
    pub fn mark_nonlocal(&mut self, name: &str) {
        self.nonlocal_vars.insert(name.to_string());
    }

    pub fn has_global(&self, name: &str) -> bool {
        self.global_vars.contains(name)
    }

    pub fn has_nonlocal(&self, name: &str) -> bool {
        self.nonlocal_vars.contains(name)
    }

    pub fn as_dict(&self, interpreter: &TreewalkInterpreter) -> Container<Dict> {
        #[allow(clippy::mutable_key_type)]
        let mut items = HashMap::new();
        for (key, value) in self.symbol_table.iter() {
            items.insert(TreewalkValue::Str(Str::new(key.clone())), value.clone());
        }

        Container::new(Dict::new(interpreter, items))
    }
}

/// Implement IntoIterator for &Scope to allow iteration by reference
impl<'a> IntoIterator for &'a Scope {
    type Item = (&'a String, &'a TreewalkValue);
    type IntoIter = Iter<'a, String, TreewalkValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.symbol_table.iter()
    }
}

impl From<HashMap<String, TreewalkValue>> for Scope {
    fn from(symbol_table: HashMap<String, TreewalkValue>) -> Self {
        Self {
            symbol_table,
            global_vars: HashSet::new(),
            nonlocal_vars: HashSet::new(),
        }
    }
}

pub struct ScopeParsingError;

impl TryFrom<Dict> for Scope {
    type Error = ScopeParsingError;

    fn try_from(value: Dict) -> Result<Self, Self::Error> {
        let mut symbol_table = HashMap::new();
        let dict_items = DictItems::try_from(value).map_err(|_| ScopeParsingError)?;
        for pair in dict_items {
            let tuple = pair.as_tuple().ok_or(ScopeParsingError)?;
            let key = tuple.first().as_string().ok_or(ScopeParsingError)?;
            let value = tuple.second();
            symbol_table.insert(key, value);
        }

        Ok(Self::from(symbol_table))
    }
}
