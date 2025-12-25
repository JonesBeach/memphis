use std::fmt::Display;

use crate::treewalk::{types::Exception, DomainResult};

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub enum Encoding {
    #[default]
    Utf8,
}

impl TryFrom<&str> for Encoding {
    type Error = Exception;

    fn try_from(value: &str) -> DomainResult<Self> {
        match value {
            "utf-8" => Ok(Self::Utf8),
            _ => Err(Exception::unknown_encoding(value)),
        }
    }
}

impl Display for Encoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Utf8 => write!(f, "utf-8"),
        }
    }
}
