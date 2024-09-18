use std::fmt::{Display, Error, Formatter};

use crate::core::Voidable;

use super::compiler::types::{CodeObject, Constant};
use super::vm::types::{Class, FunctionObject, Method, Object, Reference};

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    None,
    Integer(i64),
    String(String),
    Boolean(bool),
    Class(Class),
    Object(Object),
    Code(CodeObject),
    Function(FunctionObject),
    Method(Method),
    BuiltinFunction,
}

impl Default for Value {
    fn default() -> Self {
        Self::None
    }
}

impl Voidable for Value {
    fn is_none(&self) -> bool {
        matches!(self, Value::None)
    }
}

impl From<Reference> for Value {
    fn from(value: Reference) -> Self {
        match value {
            Reference::Int(i) => Value::Integer(i),
            Reference::Bool(i) => Value::Boolean(i),
            // These require a lookup using VM state and must be converted before this function.
            Reference::ObjectRef(_) | Reference::ConstantRef(_) => unreachable!(),
        }
    }
}

impl From<&Constant> for Value {
    fn from(value: &Constant) -> Self {
        match value {
            Constant::None => Value::None,
            Constant::Boolean(i) => Value::Boolean(*i),
            Constant::String(i) => Value::String(i.to_string()),
            Constant::Code(i) => Value::Code(i.clone()),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::None => write!(f, "None"),
            Value::Integer(i) => write!(f, "{}", i),
            Value::String(i) => write!(f, "{}", i),
            Value::Boolean(i) => write!(f, "{}", i),
            Value::Code(i) => write!(f, "{}", i),
            _ => unimplemented!("Type {:?} unimplemented in the bytecode VM.", self),
        }
    }
}

impl Value {
    pub fn as_integer(&self) -> i64 {
        match self {
            Value::Integer(i) => *i,
            _ => panic!("expected integer"),
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            Value::Boolean(i) => *i,
            _ => panic!("expected boolean"),
        }
    }

    pub fn as_string(&self) -> &str {
        match self {
            Value::String(i) => i,
            _ => panic!("expected string"),
        }
    }

    pub fn as_code(&self) -> &CodeObject {
        match self {
            Value::Code(i) => i,
            _ => panic!("expected code"),
        }
    }

    pub fn as_function(&self) -> &FunctionObject {
        match self {
            Value::Function(i) => i,
            _ => panic!("expected method"),
        }
    }

    pub fn as_method(&self) -> &Method {
        match self {
            Value::Method(i) => i,
            _ => panic!("expected method"),
        }
    }

    pub fn as_object(&self) -> &Object {
        match self {
            Value::Object(i) => i,
            _ => panic!("expected object"),
        }
    }

    pub fn as_class(&self) -> &Class {
        match self {
            Value::Class(i) => i,
            _ => panic!("expected object"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum VmError {
    StackUnderflow,
    StackOverflow,
    VariableNotFound,
    RuntimeError,
}

#[allow(clippy::enum_variant_names)]
#[derive(Clone, PartialEq, Debug)]
pub enum CompilerError {
    NameError(String),
    SyntaxError(String),
    RuntimeError,
}
