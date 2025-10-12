use crate::{
    core::Container,
    domain::{ImportPath, Source},
    treewalk::{
        protocols::Callable,
        type_system::CloneableCallable,
        types::Module,
        utils::{check_args, Args},
        ModuleStore, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct NetListenBuiltin;

impl Callable for NetListenBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        todo!();
    }

    fn name(&self) -> String {
        "listen".into()
    }
}

fn builtins() -> Vec<Box<dyn CloneableCallable>> {
    vec![Box::new(NetListenBuiltin)]
}

pub fn import(module_store: &mut ModuleStore) {
    let mut net_mod = Module::new(Source::default());
    for builtin in builtins() {
        net_mod.insert(&builtin.name(), TreewalkValue::BuiltinFunction(builtin));
    }
    module_store.store_module(&ImportPath::from("memphis.net"), Container::new(net_mod));
}
