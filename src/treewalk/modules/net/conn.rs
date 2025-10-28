use std::{
    io::{self, Read, Write},
    net::{Shutdown, TcpStream},
};

use crate::treewalk::{
    macros::impl_method_provider,
    protocols::Callable,
    utils::{check_args, Args},
    TreewalkInterpreter, TreewalkResult, TreewalkValue,
};

pub struct Connection {
    stream: TcpStream,
}

impl Connection {
    pub fn new(stream: TcpStream) -> Self {
        Self { stream }
    }

    fn recv(&mut self, bufsize: usize) -> io::Result<Vec<u8>> {
        let mut buffer = vec![0; bufsize];
        let n = self.stream.read(&mut buffer)?;
        Ok(buffer[..n].to_vec())
    }

    fn send(&mut self, data: &[u8]) -> io::Result<()> {
        self.stream.write_all(data)
    }

    fn close(&mut self) -> io::Result<()> {
        self.stream.shutdown(Shutdown::Both)
    }
}

impl_method_provider!(Connection, [ConnRecv, ConnSend, ConnClose,]);

#[derive(Clone)]
struct ConnRecv;
#[derive(Clone)]
struct ConnSend;
#[derive(Clone)]
struct ConnClose;

impl Callable for ConnRecv {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        let n = args.get_arg(0).expect_integer(interpreter)? as usize;

        let conn_obj = args.expect_self(interpreter)?.expect_object(interpreter)?;
        let mut binding = conn_obj.borrow_mut();
        let conn = binding
            .downcast_mut::<Connection>()
            .ok_or_else(|| interpreter.type_error("Expected Connection object"))?;
        let bytes = conn.recv(n).map_err(|e| {
            interpreter.runtime_error_with(format!("Connection.recv() failed: {}", e))
        })?;

        Ok(TreewalkValue::Bytes(bytes))
    }

    fn name(&self) -> String {
        "recv".into()
    }
}

impl Callable for ConnSend {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let data = args.get_arg(0).expect_bytes(interpreter)?;

        let conn_obj = args.expect_self(interpreter)?.expect_object(interpreter)?;
        let mut binding = conn_obj.borrow_mut();
        let conn = binding
            .downcast_mut::<Connection>()
            .ok_or_else(|| interpreter.type_error("Expected Connection object"))?;
        conn.send(&data).map_err(|e| {
            interpreter.runtime_error_with(format!("Connection.send() failed: {}", e))
        })?;

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "send".into()
    }
}

impl Callable for ConnClose {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0, interpreter)?;

        let conn_obj = args.expect_self(interpreter)?.expect_object(interpreter)?;
        let mut binding = conn_obj.borrow_mut();
        let conn = binding
            .downcast_mut::<Connection>()
            .ok_or_else(|| interpreter.type_error("Expected Connection object"))?;
        conn.close().map_err(|e| {
            interpreter.runtime_error_with(format!("Connection.close() failed: {}", e))
        })?;

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "close".into()
    }
}
