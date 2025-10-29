use std::{
    io,
    net::{SocketAddr, TcpListener, TcpStream},
};

pub struct Socket {
    listener: TcpListener,
}

impl Socket {
    pub fn new(host: String, port: usize) -> io::Result<Self> {
        Ok(Self {
            listener: TcpListener::bind(format!("{}:{}", host, port))?,
        })
    }

    pub fn accept(&self) -> io::Result<(TcpStream, SocketAddr)> {
        self.listener.accept()
    }
}

#[test]
fn test_socket_new_binds_successfully() {
    // Port 0 is the magic port, the OS will pick an available port for us.
    let socket = Socket::new("127.0.0.1".to_string(), 0);
    assert!(socket.is_ok());
}

#[test]
fn test_socket_new_fails_on_conflict() {
    let addr = "127.0.0.1:0";
    let listener = TcpListener::bind(addr).unwrap();
    let port = listener.local_addr().unwrap().port();

    // Try to bind again to the same port
    let socket = Socket::new("127.0.0.1".to_string(), port.into());
    assert!(socket.is_err());
}
