use crate::{
    core::net::Connection,
    treewalk::{
        macros::impl_method_provider,
        protocols::Callable,
        result::Raise,
        types::Exception,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl_method_provider!(Connection, [ConnRecv, ConnSend, ConnClose,]);

#[derive(Clone)]
struct ConnRecv;
#[derive(Clone)]
struct ConnSend;
#[derive(Clone)]
struct ConnClose;

impl Callable for ConnRecv {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        let n = args.get_arg(0).as_int().raise(interpreter)?;

        let self_val = args.get_self().raise(interpreter)?;
        let mut conn = self_val
            .as_native_object_mut::<Connection>()
            .raise(interpreter)?;
        let bytes = conn
            .recv(n as usize)
            .map_err(|e| Exception::runtime_error_with(format!("Connection.recv() failed: {}", e)))
            .raise(interpreter)?;

        Ok(TreewalkValue::Bytes(bytes))
    }

    fn name(&self) -> String {
        "recv".into()
    }
}

impl Callable for ConnSend {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let data = args.get_arg(0).as_bytes().raise(interpreter)?;

        let self_val = args.get_self().raise(interpreter)?;
        let mut conn = self_val
            .as_native_object_mut::<Connection>()
            .raise(interpreter)?;
        conn.send(&data)
            .map_err(|e| Exception::runtime_error_with(format!("Connection.send() failed: {}", e)))
            .raise(interpreter)?;

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "send".into()
    }
}

impl Callable for ConnClose {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;

        let self_val = args.get_self().raise(interpreter)?;
        let mut conn = self_val
            .as_native_object_mut::<Connection>()
            .raise(interpreter)?;
        conn.close()
            .map_err(|e| Exception::runtime_error_with(format!("Connection.close() failed: {}", e)))
            .raise(interpreter)?;

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "close".into()
    }
}
