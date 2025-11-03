use std::{
    io::{self, Read, Write},
    net::{Shutdown, TcpStream},
};

pub struct Connection {
    stream: TcpStream,
}

impl Connection {
    pub fn new(stream: TcpStream) -> Self {
        Self { stream }
    }

    pub fn recv(&mut self, bufsize: usize) -> io::Result<Vec<u8>> {
        let mut buffer = vec![0; bufsize];
        let n = self.stream.read(&mut buffer)?;
        Ok(buffer[..n].to_vec())
    }

    pub fn send(&mut self, data: &[u8]) -> io::Result<()> {
        self.stream.write_all(data)
    }

    pub fn close(&mut self) -> io::Result<()> {
        self.stream.shutdown(Shutdown::Both)
    }
}
