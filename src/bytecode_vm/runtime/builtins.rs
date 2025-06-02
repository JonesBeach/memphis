use crate::bytecode_vm::{VmResult, VmValue};

use super::{
    types::BuiltinFunc, BuiltinFunction, Class, FunctionObject, List, Module, Range, Reference,
    VirtualMachine,
};

static BUILTINS: [(&str, BuiltinFunc); 3] = [
    ("list", builtin_list),
    ("range", builtin_range),
    ("print", builtin_print),
];

pub fn register_builtins(vm: &mut VirtualMachine, module: &mut Module) {
    for (name, func) in BUILTINS {
        let builtin_obj = BuiltinFunction::new(name, func);
        let reference = vm.heapify(VmValue::BuiltinFunction(builtin_obj));
        module.global_store.insert(name.to_string(), reference);
    }
}

/// This is intended to be functionally equivalent to `__build_class__` in CPython.
pub fn build_class(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let code_value = vm.deref(args[0])?;
    let code = code_value.expect_code(vm)?;
    let name = code.name().to_string();

    let function = FunctionObject::new(code.clone());
    let frame = vm.convert_function_to_frame(function, vec![])?;

    let frame = vm.run_new_frame(frame)?;
    Ok(vm.heapify(VmValue::Class(Class::new(name, frame.namespace()))))
}

fn builtin_list(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let items = match args.len() {
        0 => vec![],
        1 => match vm.deref(args[0])? {
            VmValue::List(list) => list.items.clone(),
            _ => return Err(vm.type_error("list() expects an iterable")),
        },
        _ => return Err(vm.type_error("list() takes at most one argument")),
    };

    Ok(vm.heapify(VmValue::List(List::new(items))))
}

fn builtin_range(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let range = match args.len() {
        1 => {
            let stop = vm.deref(args[0])?.as_integer().ok_or_else(|| {
                vm.type_error("'<TODO>' object cannot be interpreted as an integer")
            })?;
            Range::with_stop(stop)
        }
        2 => {
            let start = vm.deref(args[0])?.as_integer().ok_or_else(|| {
                vm.type_error("'<TODO>' object cannot be interpreted as an integer")
            })?;
            let stop = vm.deref(args[1])?.as_integer().ok_or_else(|| {
                vm.type_error("'<TODO>' object cannot be interpreted as an integer")
            })?;
            Range::with_start_stop(start, stop)
        }
        3 => {
            let start = vm.deref(args[0])?.as_integer().ok_or_else(|| {
                vm.type_error("'<TODO>' object cannot be interpreted as an integer")
            })?;
            let stop = vm.deref(args[1])?.as_integer().ok_or_else(|| {
                vm.type_error("'<TODO>' object cannot be interpreted as an integer")
            })?;
            let step = vm.deref(args[2])?.as_integer().ok_or_else(|| {
                vm.type_error("'<TODO>' object cannot be interpreted as an integer")
            })?;
            Range::new(start, stop, step)
        }
        0 => {
            return Err(vm.type_error(&format!(
                "range expected at least 1 argument, got {}",
                args.len()
            )))
        }
        _ => {
            return Err(vm.type_error(&format!(
                "range expected at most 3 arguments, got {}",
                args.len()
            )))
        }
    };

    Ok(vm.heapify(VmValue::Range(range)))
}

fn builtin_print(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    for arg in args.iter() {
        print!("{}", vm.deref(*arg)?);
    }
    println!();

    // TODO replace this with something like vm.none(), which could return a Reference to a
    // heapified VmValue::None you allocate once during VM setup.
    Ok(vm.heapify(VmValue::None))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_builtins_inserts_list() {
        let mut vm = VirtualMachine::default();
        let mut module = Module::new("test_module");
        register_builtins(&mut vm, &mut module);
        assert!(module.global_store.contains_key("list"));
        assert!(!module.global_store.contains_key("dict"));
    }
}
