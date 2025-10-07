use std::collections::HashMap;

use crate::{
    core::Container,
    parser::types::Params,
    treewalk::{
        types::Tuple,
        utils::{check_args, Args},
        SymbolTable, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

pub fn bind_args(
    callee_name: &str,
    args: &Args,
    expected_args: &Params,
    interpreter: &TreewalkInterpreter,
) -> TreewalkResult<SymbolTable> {
    let mut table = HashMap::new();

    // Function expects fewer positional args than it was invoked with and there is not an
    // `args_var` in which to store the rest.
    check_args(
        args,
        |_| !(expected_args.args.len() < args.bound_len() && expected_args.args_var.is_none()),
        interpreter,
    )?;

    let bound_args = args.bound_args();
    let mut missing_args = vec![];

    for (index, arg_def) in expected_args.args.iter().enumerate() {
        // Check if the argument is provided, otherwise use default
        let value = if index < bound_args.len() {
            bound_args[index].clone()
        } else {
            match &arg_def.default {
                Some(default_value) => interpreter.evaluate_expr(default_value)?,
                None => {
                    missing_args.push(arg_def.arg.clone());
                    // We use None here only because if we hit this case, we will return an
                    // error shortly after this loop. We can't do it here because we need to
                    // find all the missing args first.
                    TreewalkValue::None
                }
            }
        };

        table.insert(arg_def.arg.clone(), value);
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
            .map(|a| format!("'{a}'"))
            .collect::<Vec<_>>()
            .join(" and ");
        return Err(interpreter.type_error(format!(
            "{callee_name}() missing {num_missing} required positional {noun}: {arg_names}"
        )));
    }

    if let Some(ref args_var) = expected_args.args_var {
        let extra = args.len() - expected_args.args.len();
        let left_over = bound_args.iter().rev().take(extra).rev().cloned().collect();
        let args_value = TreewalkValue::Tuple(Tuple::new(left_over));
        table.insert(args_var.to_string(), args_value);
    }

    if let Some(ref kwargs_var) = expected_args.kwargs_var {
        let kwargs_value = TreewalkValue::Dict(Container::new(args.get_kwargs(interpreter)));
        table.insert(kwargs_var.to_string(), kwargs_value);
    }

    Ok(table)
}
