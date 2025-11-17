use crate::{
    core::{net::Socket, Container},
    domain::{ExecutionError, ImportPath},
    treewalk::{
        macros::impl_method_provider,
        modules::net::Connection,
        protocols::Callable,
        result::Raise,
        types::{Object, Str, Tuple},
        utils::Args,
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl_method_provider!(Socket, [AcceptBuiltin]);

#[derive(Clone)]
struct AcceptBuiltin;

impl Callable for AcceptBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        let self_val = args.get_self().raise(interpreter)?;
        let socket = self_val.as_native_object::<Socket>().raise(interpreter)?;

        let (stream, addr) = socket
            .accept()
            .map_err(|e| {
                ExecutionError::runtime_error_with(format!("Socket.accept() failed: {}", e))
            })
            .raise(interpreter)?;

        let conn = Connection::new(stream);

        let conn_class = interpreter
            .state
            .read_class(&ImportPath::from("memphis.net.Connection"))
            .ok_or_else(|| ExecutionError::runtime_error_with("Connection class not found"))
            .raise(interpreter)?;

        let conn_obj = Object::with_payload(conn_class.clone(), conn);

        Ok(TreewalkValue::Tuple(Tuple::new(vec![
            TreewalkValue::Object(Container::new(conn_obj)),
            TreewalkValue::Str(Str::new(&addr.to_string())),
        ])))
    }

    fn name(&self) -> String {
        "accept".into()
    }
}
