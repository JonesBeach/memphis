use std::fmt::{Display, Error, Formatter};

use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Constant},
        runtime::{Class, FunctionObject, List, Method, Module, Object, Reference},
    },
    core::{Container, Voidable},
    domain::MemphisValue,
};

#[derive(Clone, Debug)]
pub enum VmValue {
    None,
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Class(Class),
    Object(Object),
    Code(CodeObject),
    Function(FunctionObject),
    Method(Method),
    Module(Container<Module>),
    BuiltinFunction,
    List(List),
}

impl VmValue {
    pub fn into_ref(self) -> Reference {
        match self {
            VmValue::Int(i) => Reference::Int(i),
            VmValue::Float(i) => Reference::Float(i),
            VmValue::Bool(i) => Reference::Bool(i),
            _ => unimplemented!("Conversion to reference not supported for {:?}", self),
        }
    }
}

impl PartialEq for VmValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VmValue::None, VmValue::None) => true,
            (VmValue::Int(a), VmValue::Int(b)) => a == b,
            (VmValue::Float(a), VmValue::Float(b)) => (a - b).abs() < 1e-9,
            (VmValue::String(a), VmValue::String(b)) => a == b,
            (VmValue::Bool(a), VmValue::Bool(b)) => a == b,
            (VmValue::List(a), VmValue::List(b)) => a == b,
            // Add Class/Object/Code/Function/etc handling later if needed
            _ => false,
        }
    }
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
            Reference::Int(i) => VmValue::Int(i),
            Reference::Float(i) => VmValue::Float(i),
            Reference::Bool(i) => VmValue::Bool(i),
            // These require a lookup using VM state and must be converted before this function.
            Reference::ObjectRef(_) | Reference::ConstantRef(_) => unreachable!(),
        }
    }
}

impl From<&Constant> for VmValue {
    fn from(value: &Constant) -> Self {
        match value {
            Constant::None => VmValue::None,
            Constant::Boolean(i) => VmValue::Bool(*i),
            Constant::String(i) => VmValue::String(i.to_string()),
            Constant::Code(i) => VmValue::Code(i.clone()),
        }
    }
}

impl Display for VmValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            VmValue::None => write!(f, "None"),
            VmValue::Int(i) => write!(f, "{}", i),
            VmValue::String(i) => write!(f, "{}", i),
            VmValue::Bool(i) => write!(f, "{}", i),
            VmValue::Code(i) => write!(f, "{}", i),
            _ => unimplemented!("Type {:?} unimplemented in the bytecode VM.", self),
        }
    }
}

impl VmValue {
    pub fn as_integer(&self) -> i64 {
        match self {
            VmValue::Int(i) => *i,
            _ => panic!("expected integer"),
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            VmValue::Bool(i) => *i,
            _ => panic!("expected boolean"),
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

    pub fn as_list(&self) -> Option<&List> {
        match self {
            VmValue::List(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_object(&self) -> Option<&Object> {
        match self {
            VmValue::Object(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_module(&self) -> Option<&Container<Module>> {
        match self {
            VmValue::Module(i) => Some(i),
            _ => None,
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
            VmValue::Int(val) => MemphisValue::Integer(val),
            VmValue::Float(val) => MemphisValue::Float(val),
            VmValue::String(val) => MemphisValue::String(val),
            VmValue::Bool(val) => MemphisValue::Boolean(val),
            _ => unimplemented!("Conversion not implemented for type {:?}", value),
        }
    }
}
