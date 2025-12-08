use std::fmt::{Display, Error, Formatter};

#[derive(Clone, PartialEq, Debug)]
pub enum CompilerError {
    Unsupported(String),
    SyntaxError(String),
    Internal(String),
    ImportError(String),
}

impl CompilerError {
    pub fn import_error(msg: impl Into<String>) -> Self {
        Self::ImportError(msg.into())
    }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Self::Unsupported(msg) => write!(f, "Unsupported feature: {msg}"),
            Self::SyntaxError(msg) => write!(f, "Syntax error: {msg}"),
            Self::Internal(msg) => write!(f, "Internal error: {msg}"),
            Self::ImportError(msg) => write!(f, "ImportError: {msg}"),
        }
    }
}
