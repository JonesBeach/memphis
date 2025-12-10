use std::collections::HashMap;

use crate::{
    domain::{DomainResult, ExecutionError},
    parser::types::{CallArgs, KwargsOperation, Params},
    treewalk::{
        protocols::TryEvalFrom,
        result::Raise,
        types::{
            function::{RuntimeParam, RuntimeParams},
            Tuple,
        },
        utils::Args,
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    /// Evaluate the arguments a function is called with.
    pub fn evaluate_args(&self, call_args: &CallArgs) -> TreewalkResult<Args> {
        let mut positional = call_args
            .args
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<TreewalkResult<Vec<_>>>()?;

        if let Some(ref args_var) = call_args.args_var {
            let value = self.evaluate_expr(args_var)?;
            let args = Tuple::try_eval_from(value, self)?;
            // Clone each item in place without an intermediate Vec
            positional.extend_from_slice(args.items());
        };

        #[allow(clippy::mutable_key_type)]
        let mut kwargs = HashMap::default();
        for kwarg in call_args.kwargs.iter() {
            match kwarg {
                KwargsOperation::Pair(key, value) => {
                    let value = self.evaluate_expr(value)?;
                    insert_kwarg(&mut kwargs, key, value).raise(self)?;
                }
                KwargsOperation::Unpacking(expr) => {
                    let unpacked = self.evaluate_expr(expr)?;
                    for key_val in unpacked.clone().as_iterable().raise(self)? {
                        let key = key_val.as_str().raise(self)?;
                        let value = self.load_index(&unpacked, &key_val)?;
                        insert_kwarg(&mut kwargs, &key, value).raise(self)?;
                    }
                }
            }
        }

        Ok(Args::new(positional, kwargs))
    }

    /// Evaluate the parameters a function is defined with, specifically any default values.
    pub fn evaluate_params(&self, call_params: &Params) -> TreewalkResult<RuntimeParams> {
        let runtime_params = call_params
            .args
            .iter()
            .map(|param| {
                let default = match &param.default {
                    Some(expr) => Some(self.evaluate_expr(expr)?),
                    None => None,
                };
                Ok(RuntimeParam {
                    arg: param.arg.clone(),
                    default,
                })
            })
            .collect::<TreewalkResult<Vec<_>>>()?;

        Ok(RuntimeParams {
            args: runtime_params,
            args_var: call_params.args_var.clone(),
            kwargs_var: call_params.kwargs_var.clone(),
        })
    }
}

fn insert_kwarg(
    kwargs: &mut HashMap<String, TreewalkValue>,
    key: &str,
    value: TreewalkValue,
) -> DomainResult<()> {
    if kwargs.contains_key(key) {
        Err(ExecutionError::key_error(key))
    } else {
        kwargs.insert(key.to_string(), value);
        Ok(())
    }
}
