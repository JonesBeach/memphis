use std::{
    fmt::{Display, Error, Formatter},
    time::Duration,
};

use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Constant},
        runtime::{
            types::{
                Class, Coroutine, Dict, FunctionObject, Generator, List, ListIter, Method, Module,
                Object, Range, RangeIter, Tuple, TupleIter,
            },
            BuiltinFunction, Reference,
        },
    },
    core::{floats_equal, Container, Voidable},
    domain::{DomainResult, ExecutionError, MemphisValue, Type},
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
    Generator(Container<Generator>),
    Coroutine(Container<Coroutine>),
    SleepFuture(Duration),
    Method(Method),
    Module(Container<Module>),
    BuiltinFunction(BuiltinFunction),
    List(List),
    Tuple(Tuple),
    Dict(Dict),
    Range(Range),
    ListIter(Container<ListIter>),
    TupleIter(Container<TupleIter>),
    RangeIter(Container<RangeIter>),
}

impl VmValue {
    pub fn into_ref(self) -> Reference {
        match self {
            VmValue::Int(i) => Reference::Int(i),
            VmValue::Float(i) => Reference::Float(i),
            _ => unimplemented!("Conversion to reference not supported for {:?}", self),
        }
    }
}

impl PartialEq for VmValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VmValue::None, VmValue::None) => true,
            (VmValue::Int(a), VmValue::Int(b)) => a == b,
            (VmValue::Float(a), VmValue::Float(b)) => floats_equal(*a, *b),
            (VmValue::Int(a), VmValue::Float(b)) => floats_equal(*a as f64, *b),
            (VmValue::Float(a), VmValue::Int(b)) => floats_equal(*a, *b as f64),
            (VmValue::String(a), VmValue::String(b)) => a == b,
            (VmValue::Bool(a), VmValue::Bool(b)) => a == b,
            (VmValue::List(a), VmValue::List(b)) => a == b,
            (VmValue::Tuple(a), VmValue::Tuple(b)) => a == b,
            (VmValue::Dict(a), VmValue::Dict(b)) => a == b,
            (VmValue::Range(a), VmValue::Range(b)) => a == b,
            // Add Class/Object/Code/Function/etc handling later if needed
            _ => false,
        }
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
            // This requires a lookup using VM state and must be converted before this function.
            Reference::ObjectRef(_) => unreachable!(),
        }
    }
}

impl From<&Constant> for VmValue {
    fn from(value: &Constant) -> Self {
        match value {
            Constant::None => VmValue::None,
            Constant::Boolean(i) => VmValue::Bool(*i),
            Constant::Int(i) => VmValue::Int(*i),
            Constant::Float(i) => VmValue::Float(*i),
            Constant::String(i) => VmValue::String(i.to_string()),
            Constant::Code(i) => VmValue::Code(i.clone()),
        }
    }
}

impl Display for VmValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            VmValue::None => write!(f, "None"),
            VmValue::Int(i) => write!(f, "{i}"),
            VmValue::String(i) => write!(f, "{i}"),
            VmValue::Bool(b) => match b {
                true => write!(f, "True"),
                false => write!(f, "False"),
            },
            VmValue::Range(i) => write!(f, "{i}"),
            VmValue::RangeIter(_) => write!(f, "<rangeiter>"),
            VmValue::Code(i) => write!(f, "{i}"),
            VmValue::BuiltinFunction(i) => write!(f, "{i}"),
            VmValue::Function(i) => write!(f, "{i}"),
            VmValue::Method(i) => write!(f, "{i}"),
            VmValue::Module(i) => write!(f, "{}", i.borrow()),
            VmValue::Coroutine(i) => write!(f, "{}", i.borrow()),
            VmValue::SleepFuture(_) => write!(f, "<sleepfuture>"),
            _ => unimplemented!(
                "Trait Display for type {:?} unimplemented in the bytecode VM.",
                self
            ),
        }
    }
}

impl VmValue {
    /// This is incomplete. On the treewalk side, we handle this by calling resolve_descriptor
    /// inside some MemberRead implementations and not in others.
    pub fn should_bind(&self) -> bool {
        !matches!(self, VmValue::Module(_))
    }

    pub fn get_type(&self) -> Type {
        match self {
            VmValue::None => Type::None,
            VmValue::Int(_) => Type::Int,
            VmValue::Float(_) => Type::Float,
            VmValue::String(_) => Type::Str,
            VmValue::Bool(_) => Type::Bool,
            VmValue::List(_) => Type::List,
            VmValue::Tuple(_) => Type::Tuple,
            VmValue::Range(_) => Type::Range,
            VmValue::ListIter(_) => Type::ListIter,
            VmValue::TupleIter(_) => Type::TupleIter,
            VmValue::RangeIter(_) => Type::RangeIter,
            _ => unimplemented!(
                "get_type for type {:?} unimplemented in the bytecode VM.",
                self
            ),
        }
    }

    pub fn coerce_to_int(&self) -> DomainResult<i64> {
        match self {
            VmValue::Int(i) => Ok(*i),
            VmValue::String(s) => s
                .parse::<i64>()
                .map_err(|_| ExecutionError::type_error("Invalid int literal")),
            _ => Err(ExecutionError::type_error("Cannot coerce to an int")),
        }
    }

    pub fn as_integer(&self) -> Option<i64> {
        match self {
            VmValue::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn expect_integer(&self) -> DomainResult<i64> {
        self.as_integer()
            .ok_or_else(|| ExecutionError::type_error("Expected an integer"))
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            VmValue::Float(i) => Some(*i),
            _ => None,
        }
    }

    pub fn expect_float(&self) -> DomainResult<f64> {
        self.as_float()
            .ok_or_else(|| ExecutionError::type_error("Expected a float"))
    }

    pub fn to_boolean(&self) -> bool {
        match self {
            VmValue::None => false,
            VmValue::Bool(i) => *i,
            VmValue::Int(i) => *i != 0,
            VmValue::Float(i) => *i != 0.0,
            VmValue::String(i) => !i.is_empty(),
            VmValue::List(i) => !i.is_empty(),
            VmValue::Tuple(i) => !i.is_empty(),
            // Most values in Python are truthy
            _ => true,
        }
    }

    pub fn as_code(&self) -> Option<&CodeObject> {
        match self {
            VmValue::Code(i) => Some(i),
            _ => None,
        }
    }

    pub fn expect_code(&self) -> DomainResult<&CodeObject> {
        self.as_code()
            .ok_or_else(|| ExecutionError::type_error("Expected a code object"))
    }

    pub fn as_function(&self) -> Option<&FunctionObject> {
        match self {
            VmValue::Function(i) => Some(i),
            _ => None,
        }
    }

    pub fn expect_function(&self) -> DomainResult<&FunctionObject> {
        self.as_function()
            .ok_or_else(|| ExecutionError::type_error("Expected a function object"))
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

    pub fn as_class(&self) -> Option<&Class> {
        match self {
            VmValue::Class(i) => Some(i),
            _ => None,
        }
    }

    pub fn expect_class(&self) -> DomainResult<&Class> {
        self.as_class()
            .ok_or_else(|| ExecutionError::type_error("Expected a class"))
    }

    pub fn as_coroutine(&self) -> Option<&Container<Coroutine>> {
        match self {
            VmValue::Coroutine(i) => Some(i),
            _ => None,
        }
    }

    pub fn expect_coroutine(&self) -> DomainResult<&Container<Coroutine>> {
        self.as_coroutine()
            .ok_or_else(|| ExecutionError::type_error("Expected a coroutine"))
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
            VmValue::List(i) => {
                let items = i
                    .items
                    .into_iter()
                    .map(VmValue::from)
                    .map(|item| item.into())
                    .collect::<Vec<MemphisValue>>();
                MemphisValue::List(items)
            }
            _ => unimplemented!("Conversion not implemented for type {:?}", value),
        }
    }
}
