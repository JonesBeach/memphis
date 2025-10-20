use std::{
    io::{Read, Write},
    net::{TcpListener, TcpStream},
};

use crate::{
    core::Container,
    domain::{ImportPath, Source},
    treewalk::{
        protocols::{Callable, MemberRead},
        type_system::CloneableCallable,
        types::{Module, Str, Tuple},
        utils::{check_args, Args},
        ModuleStore, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug)]
struct Socket {
    listener: TcpListener,
}

impl Socket {
    fn new(host: String, port: usize) -> Self {
        let addr = format!("{}:{}", host, port);
        Self {
            listener: TcpListener::bind(addr).unwrap(),
        }
    }
}

impl Clone for Socket {
    fn clone(&self) -> Self {
        Self {
            listener: self.listener.try_clone().unwrap(),
        }
    }
}

impl MemberRead for Socket {
    fn get_member(
        &self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        let result = match name {
            "accept" => Some(TreewalkValue::BuiltinFunction(Box::new(AcceptBuiltin {
                listener: self.listener.try_clone().unwrap(),
            }))),
            _ => None,
        };

        Ok(result)
    }
}

#[derive(Debug)]
struct Connection {
    stream: TcpStream,
}

impl Connection {
    #[allow(dead_code)]
    fn recv(&mut self, bufsize: usize) -> String {
        let mut buffer = vec![0; bufsize];
        let n = self.stream.read(&mut buffer).unwrap_or(0);
        String::from_utf8_lossy(&buffer[..n]).to_string()
    }

    #[allow(dead_code)]
    fn send(&mut self, data: &str) {
        self.stream.write_all(data.as_bytes()).unwrap();
    }

    #[allow(dead_code)]
    fn close(&mut self) {
        self.stream.shutdown(std::net::Shutdown::Both).ok();
    }
}

impl Clone for Connection {
    fn clone(&self) -> Self {
        Self {
            stream: self.stream.try_clone().unwrap(),
        }
    }
}

impl MemberRead for Connection {
    fn get_member(
        &self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        let result = match name {
            "recv" => Some(TreewalkValue::BuiltinFunction(Box::new(ConnRecv {
                stream: self.stream.try_clone().unwrap(),
            }))),
            "send" => Some(TreewalkValue::BuiltinFunction(Box::new(ConnSend {
                stream: self.stream.try_clone().unwrap(),
            }))),
            "close" => Some(TreewalkValue::BuiltinFunction(Box::new(ConnClose {
                stream: self.stream.try_clone().unwrap(),
            }))),
            _ => None,
        };
        Ok(result)
    }
}

pub struct ConnRecv {
    pub stream: TcpStream,
}
impl Clone for ConnRecv {
    fn clone(&self) -> Self {
        Self {
            stream: self.stream.try_clone().unwrap(),
        }
    }
}
pub struct ConnSend {
    pub stream: TcpStream,
}
impl Clone for ConnSend {
    fn clone(&self) -> Self {
        Self {
            stream: self.stream.try_clone().unwrap(),
        }
    }
}
pub struct ConnClose {
    pub stream: TcpStream,
}

impl Clone for ConnClose {
    fn clone(&self) -> Self {
        Self {
            stream: self.stream.try_clone().unwrap(),
        }
    }
}

impl Callable for ConnRecv {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        let n = args.get_arg(0).expect_integer(interpreter)? as usize;
        let mut buffer = vec![0; n];
        let num_read = self.stream.try_clone().unwrap().read(&mut buffer).unwrap();
        Ok(TreewalkValue::Bytes(buffer[..num_read].to_vec()))
    }

    fn name(&self) -> String {
        "recv".into()
    }
}

impl Callable for ConnSend {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        // Validate args
        check_args(&args, |len| len == 1, interpreter)?;

        // Get string to send
        let data = args.get_arg(0).expect_string(interpreter)?;
        // We clone the stream because each builtin owns its handle
        let mut stream = self.stream.try_clone().unwrap();
        stream.write_all(data.as_bytes()).unwrap();

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "send".into()
    }
}

impl Callable for ConnClose {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0, interpreter)?;

        let stream = self.stream.try_clone().unwrap();
        stream.shutdown(std::net::Shutdown::Both).ok();

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "close".into()
    }
}

pub struct AcceptBuiltin {
    listener: TcpListener,
}

impl Clone for AcceptBuiltin {
    fn clone(&self) -> Self {
        Self {
            listener: self.listener.try_clone().unwrap(),
        }
    }
}

impl Callable for AcceptBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        let (stream, addr) = self.listener.accept().unwrap();
        let conn = Connection { stream };
        let conn_value = TreewalkValue::NativeObject(Box::new(conn));
        let addr_value = TreewalkValue::Str(Str::new(&addr.to_string()));

        Ok(TreewalkValue::Tuple(Tuple::new(vec![
            conn_value, addr_value,
        ])))
    }

    fn name(&self) -> String {
        "accept".into()
    }
}

#[derive(Clone)]
pub struct NetListenBuiltin;

impl Callable for NetListenBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let host_port = args.get_arg(0).expect_tuple(interpreter)?;
        let host = host_port.first().expect_string(interpreter)?;
        let port = host_port.second().expect_integer(interpreter)?;
        let socket = Socket::new(host, port as usize);

        Ok(TreewalkValue::NativeObject(Box::new(socket)))
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
