use crate::{
    bytecode_vm::{VmResult, VmValue},
    core::Container,
};

use super::{types::BuiltinFunc, BuiltinFunction, Module, Reference, Runtime, VirtualMachine};

fn asyncio_run(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let co_binding = vm.deref(args[0])?;
    let coroutine = co_binding.expect_coroutine(vm)?;
    let mut executor = std::mem::take(&mut vm.executor);
    let result = executor.run(vm, coroutine.clone())?;
    vm.executor = executor;
    Ok(result)
}

fn asyncio_create_task(_vm: &mut VirtualMachine, _args: Vec<Reference>) -> VmResult<Reference> {
    todo!()
}

fn asyncio_sleep(_vm: &mut VirtualMachine, _args: Vec<Reference>) -> VmResult<Reference> {
    todo!()
}

pub fn init_module(runtime: &mut Runtime) {
    let mut asyncio_mod = Module::new("asyncio");
    register_builtins(runtime, &mut asyncio_mod);
    runtime.store_module(Container::new(asyncio_mod));
}

static BUILTINS: [(&str, BuiltinFunc); 3] = [
    ("run", asyncio_run),
    ("create_task", asyncio_create_task),
    ("sleep", asyncio_sleep),
];

pub fn register_builtins(runtime: &mut Runtime, module: &mut Module) {
    for (name, func) in BUILTINS {
        let func_ref = runtime
            .heap
            .allocate(VmValue::BuiltinFunction(BuiltinFunction::new(name, func)));
        module.write(name, func_ref);
    }
}
