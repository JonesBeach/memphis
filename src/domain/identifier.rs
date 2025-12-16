use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier(String);

impl Identifier {
    pub fn new(s: impl Into<String>) -> Result<Self, IdentifierError> {
        let s = s.into();

        if is_valid_identifier(&s) {
            Ok(Self(s))
        } else {
            Err(IdentifierError::Invalid(s))
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug)]
pub enum IdentifierError {
    Invalid(String),
}

fn is_valid_identifier(s: &str) -> bool {
    // Python-like rules or Memphis rules:
    // - starts with letter or _
    // - contains only alphanumeric + _
    // - non-empty
    let mut chars = s.chars();
    match chars.next() {
        None => return false,
        Some(c) if !(c.is_alphabetic() || c == '_') => return false,
        _ => {}
    }

    chars.all(|c| c.is_alphanumeric() || c == '_')
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
