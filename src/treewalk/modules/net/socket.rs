use std::{
    io,
    net::{SocketAddr, TcpListener, TcpStream},
};

use crate::{
    core::Container,
    domain::ImportPath,
    treewalk::{
        macros::impl_method_provider,
        modules::net::Connection,
        protocols::Callable,
        types::{Object, Str, Tuple},
        utils::Args,
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

pub struct Socket {
    listener: TcpListener,
}

impl_method_provider!(Socket, [AcceptBuiltin]);

impl Socket {
    pub fn new(host: String, port: usize) -> Self {
        let addr = format!("{}:{}", host, port);
        Self {
            listener: TcpListener::bind(addr).expect("Failed to bind TcpListener"),
        }
    }

    fn accept(&self) -> io::Result<(TcpStream, SocketAddr)> {
        self.listener.accept()
    }
}

#[derive(Clone)]
struct AcceptBuiltin;

impl Callable for AcceptBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        let listener = args.expect_self(interpreter)?.expect_object(interpreter)?;
        let binding = listener.borrow();
        let socket = binding
            .downcast_ref::<Socket>()
            .ok_or_else(|| interpreter.type_error("Expected Socket object"))?;

        let (stream, addr) = socket.accept().map_err(|e| {
            interpreter.runtime_error_with(format!("Socket.accept() failed: {}", e))
        })?;

        let conn = Connection::new(stream);

        let conn_class = interpreter
            .state
            .read_class(&ImportPath::from("memphis.net.Connection"))
            .ok_or_else(|| interpreter.runtime_error_with("Connection class not found"))?;

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
