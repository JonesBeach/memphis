use crate::{
    core::{
        net::{Connection, Socket},
        Container,
    },
    domain::{ModuleName, Type},
    treewalk::{
        protocols::Callable,
        result::Raise,
        type_system::{CloneableCallable, MethodProvider},
        types::{Class, Exception, Module, Object},
        utils::{check_args, Args},
        ModuleStore, TreewalkInterpreter, TreewalkResult, TreewalkValue, TypeRegistry,
    },
};

mod conn;
mod socket;

#[derive(Clone)]
pub struct NetListenBuiltin;

impl Callable for NetListenBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let host_port = args.get_arg(0).as_tuple().raise(interpreter)?;
        let host = host_port.first().as_str().raise(interpreter)?;
        let port = host_port.second().as_int().raise(interpreter)?;

        let socket = Socket::new(host, port as usize)
            .map_err(|e| Exception::runtime_error_with(format!("Failed to bind Socket: {}", e)))
            .raise(interpreter)?;

        let socket_class = interpreter
            .state
            .read_class(&ModuleName::from_segments(&["memphis", "net"]), "Socket")
            .ok_or_else(|| Exception::runtime_error_with("Socket class not found"))
            .raise(interpreter)?;

        let obj = Object::with_payload(socket_class.clone(), socket);

        Ok(TreewalkValue::Object(Container::new(obj)))
    }

    fn name(&self) -> String {
        "listen".into()
    }
}

#[cfg(test)]
mod tests {
    use std::net::TcpListener;

    use crate::{
        domain::test_utils::*,
        treewalk::{test_utils::*, utils::args},
    };

    use super::*;

    #[test]
    fn test_listen_runtime_error_on_conflict() {
        // Bind the port manually first
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();

        // Call the builtin directly
        let builtin = NetListenBuiltin;
        let args = args![tuple![str!("127.0.0.1"), int!(port)]];
        let interpreter = TreewalkInterpreter::default();

        let result = builtin.call(&interpreter, args);

        assert!(result.is_err());
        let err_binding = result.unwrap_err();
        let err = err_binding.as_err();
        assert_runtime_error_contains!(err.exception, "Failed to bind Socket");
    }
}

fn builtins() -> Vec<Box<dyn CloneableCallable>> {
    vec![Box::new(NetListenBuiltin)]
}

fn register_native_class<T: MethodProvider>(
    mod_: &mut Module,
    name: &str,
    type_registry: &TypeRegistry,
) {
    let object_class = type_registry.get_type_class(&Type::Object);
    let type_class = type_registry.get_type_class(&Type::Type);

    let mut class = Class::new_direct(name, Some(type_class.clone()), vec![object_class.clone()]);

    for builtin in T::get_methods() {
        class.set_on_class(&builtin.name(), TreewalkValue::BuiltinMethod(builtin));
    }

    mod_.insert(name, TreewalkValue::Class(Container::new(class)));
}

fn init(type_registry: &TypeRegistry) -> Module {
    let mut net_mod = Module::new_builtin(ModuleName::from_segments(&["memphis", "net"]));
    for builtin in builtins() {
        net_mod.insert(&builtin.name(), TreewalkValue::BuiltinFunction(builtin));
    }

    register_native_class::<Socket>(&mut net_mod, "Socket", type_registry);
    register_native_class::<Connection>(&mut net_mod, "Connection", type_registry);

    net_mod
}

pub fn import(module_store: &mut ModuleStore, type_registry: &TypeRegistry) {
    let net_mod = init(type_registry);

    let memphis_mod = module_store.get_or_create_module(&ModuleName::from_segments(&["memphis"]));
    memphis_mod.borrow_mut().insert(
        "net",
        TreewalkValue::Module(Container::new(net_mod.clone())),
    );

    module_store.store_module(Container::new(net_mod));
}
