use crate::{
    bytecode_vm::{VmResult, VmValue},
    core::Container,
};

use super::{BuiltinFunction, Module, Reference, Runtime, VirtualMachine};

fn asyncio_run(_vm: &mut VirtualMachine, _args: Vec<Reference>) -> VmResult<Reference> {
    todo!();
}

pub fn init_module(runtime: &mut Runtime) {
    let func_ref = runtime
        .heap
        .allocate(VmValue::BuiltinFunction(BuiltinFunction::new(
            "run",
            asyncio_run,
        )));

    let mut asyncio_mod = Module::new("asyncio");
    asyncio_mod.write("run", func_ref);
    runtime.store_module(Container::new(asyncio_mod));
}
