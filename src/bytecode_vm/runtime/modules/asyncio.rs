use std::time::Duration;

use crate::{
    bytecode_vm::{
        result::Raise,
        runtime::{runtime::register_builtin_funcs, types::Module, BuiltinFn, Reference},
        Runtime, VirtualMachine, VmResult, VmValue,
    },
    core::Container,
    domain::ModuleName,
};

fn asyncio_run(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let co_binding = vm.deref(args[0]).raise(vm)?;
    let coroutine = co_binding.expect_coroutine().raise(vm)?;

    // take raw pointer before borrowing anything mutably
    let vm_ptr = vm as *mut VirtualMachine;
    vm.executor.run_until_complete(vm_ptr, coroutine.clone())
}

fn asyncio_create_task(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let co_binding = vm.deref(args[0]).raise(vm)?;
    let coroutine = co_binding.expect_coroutine().raise(vm)?;

    // enqueue it on the executor, which will start it running
    vm.executor.spawn(coroutine.clone());

    // return an awaitable handle you can await later (the same coroutine we passed in here)
    Ok(args[0])
}

fn asyncio_sleep(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let duration_in_s = vm.deref(args[0]).raise(vm)?.expect_float().raise(vm)?;

    let micros = duration_in_s * 1_000_000.0;
    let duration = Duration::from_micros(micros as u64);

    let sleep_future = VmValue::SleepFuture(duration);
    Ok(vm.heapify(sleep_future))
}

pub fn init_module(runtime: &mut Runtime) {
    let mut asyncio_mod = Module::new(ModuleName::from_segments(&["asyncio"]));
    register_builtin_funcs(runtime, &mut asyncio_mod, &BUILTINS);
    runtime.store_module(Container::new(asyncio_mod));
}

static BUILTINS: [(&str, BuiltinFn); 3] = [
    ("run", asyncio_run),
    ("create_task", asyncio_create_task),
    ("sleep", asyncio_sleep),
];
