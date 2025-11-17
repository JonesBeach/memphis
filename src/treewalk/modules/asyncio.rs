use crate::{
    core::Container,
    domain::{ImportPath, Source},
    treewalk::{
        protocols::Callable,
        result::Raise,
        type_system::CloneableCallable,
        types::Module,
        utils::{check_args, Args},
        ModuleStore, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct AsyncioRunBuiltin;
#[derive(Clone)]
pub struct AsyncioSleepBuiltin;
#[derive(Clone)]
pub struct AsyncioCreateTaskBuiltin;

impl Callable for AsyncioRunBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let coroutine = args.get_arg(0).as_coroutine().raise(interpreter)?;
        interpreter.with_executor(|exec| exec.run(interpreter, coroutine))
    }

    fn name(&self) -> String {
        "run".into()
    }
}

impl Callable for AsyncioSleepBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;
        let duration = args.get_arg(0).as_float().raise(interpreter)?;
        interpreter.with_executor(|exec| exec.sleep(duration))
    }

    fn name(&self) -> String {
        "sleep".into()
    }
}

impl Callable for AsyncioCreateTaskBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let coroutine = args.get_arg(0).as_coroutine().raise(interpreter)?;
        interpreter.with_executor(|exec| exec.spawn(coroutine))
    }

    fn name(&self) -> String {
        "create_task".into()
    }
}

fn builtins() -> Vec<Box<dyn CloneableCallable>> {
    vec![
        Box::new(AsyncioRunBuiltin),
        Box::new(AsyncioSleepBuiltin),
        Box::new(AsyncioCreateTaskBuiltin),
    ]
}

fn init() -> Module {
    let mut mod_ = Module::new(Source::default());
    for builtin in builtins() {
        mod_.insert(&builtin.name(), TreewalkValue::BuiltinFunction(builtin));
    }
    mod_
}

pub fn import(module_store: &mut ModuleStore) {
    let asyncio_mod = init();
    module_store.store_module(&ImportPath::from("asyncio"), Container::new(asyncio_mod));
}
