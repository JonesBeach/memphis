use std::fmt::{Display, Error, Formatter};

use crate::{core::Voidable, domain::MemphisValue};

use super::{
    compiler::types::{CodeObject, Constant},
    vm::types::{Class, FunctionObject, Method, Object, Reference},
};

#[derive(Clone, PartialEq, Debug)]
pub enum VmValue {
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

impl Default for VmValue {
    fn default() -> Self {
        Self::None
    }
}

impl Voidable for VmValue {
    fn is_none(&self) -> bool {
        matches!(self, VmValue::None)
    }
}

impl From<Reference> for VmValue {
    fn from(value: Reference) -> Self {
        match value {
            Reference::Int(i) => VmValue::Integer(i),
            Reference::Bool(i) => VmValue::Boolean(i),
            // These require a lookup using VM state and must be converted before this function.
            Reference::ObjectRef(_) | Reference::ConstantRef(_) => unreachable!(),
        }
    }
}

impl From<&Constant> for VmValue {
    fn from(value: &Constant) -> Self {
        match value {
            Constant::None => VmValue::None,
            Constant::Boolean(i) => VmValue::Boolean(*i),
            Constant::String(i) => VmValue::String(i.to_string()),
            Constant::Code(i) => VmValue::Code(i.clone()),
        }
    }
}

impl Display for VmValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            VmValue::None => write!(f, "None"),
            VmValue::Integer(i) => write!(f, "{}", i),
            VmValue::String(i) => write!(f, "{}", i),
            VmValue::Boolean(i) => write!(f, "{}", i),
            VmValue::Code(i) => write!(f, "{}", i),
            _ => unimplemented!("Type {:?} unimplemented in the bytecode VM.", self),
        }
    }
}

impl VmValue {
    pub fn as_integer(&self) -> i64 {
        match self {
            VmValue::Integer(i) => *i,
            _ => panic!("expected integer"),
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            VmValue::Boolean(i) => *i,
            _ => panic!("expected boolean"),
        }
    }

    pub fn as_string(&self) -> &str {
        match self {
            VmValue::String(i) => i,
            _ => panic!("expected string"),
        }
    }

    pub fn as_code(&self) -> &CodeObject {
        match self {
            VmValue::Code(i) => i,
            _ => panic!("expected code"),
        }
    }

    pub fn as_function(&self) -> &FunctionObject {
        match self {
            VmValue::Function(i) => i,
            _ => panic!("expected method"),
        }
    }

    pub fn as_method(&self) -> &Method {
        match self {
            VmValue::Method(i) => i,
            _ => panic!("expected method"),
        }
    }

    pub fn as_object(&self) -> &Object {
        match self {
            VmValue::Object(i) => i,
            _ => panic!("expected object"),
        }
    }

    pub fn as_class(&self) -> &Class {
        match self {
            VmValue::Class(i) => i,
            _ => panic!("expected object"),
        }
    }
}

impl From<VmValue> for MemphisValue {
    fn from(value: VmValue) -> Self {
        match value {
            VmValue::None => MemphisValue::None,
            VmValue::Integer(val) => MemphisValue::Integer(val),
            VmValue::String(val) => MemphisValue::String(val),
            VmValue::Boolean(val) => MemphisValue::Boolean(val),
            _ => unimplemented!(
                "Conversion to TestValue not implemented for type {:?}",
                value
            ),
        }
    }
}
