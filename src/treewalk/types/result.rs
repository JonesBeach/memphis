use std::fmt;
use std::fmt::{Display, Error, Formatter};
use std::hash::{Hash, Hasher};

#[cfg(feature = "c_stdlib")]
use super::cpython::{CPythonClass, CPythonModule, CPythonObject};
use super::traits::MemberWriter;
use crate::core::{Container, Voidable};
use crate::treewalk::Interpreter;
use crate::types::errors::InterpreterError;

use super::{
    iterators::{
        DictItemsIterator, DictKeysIterator, DictValuesIterator, GeneratorIterator, ListIterator,
        RangeIterator, ReversedIterator, StringIterator, ZipIterator,
    },
    traits::{Callable, DataDescriptor, IndexRead, IndexWrite, MemberReader, NonDataDescriptor},
    types::TypeExpr,
    utils::{BuiltinObject, Dunder, ResolvedArguments},
    ByteArray, Bytes, Cell, Class, Classmethod, Code, Complex, Coroutine, Dict, DictItems,
    DictKeys, DictValues, FrozenSet, Function, List, MappingProxy, Method, Module, Object,
    Property, Range, Set, Slice, Staticmethod, Str, Super, Traceback, Tuple, Type,
};

#[derive(Clone)]
pub enum ExprResult {
    /// This represents the return value for statements, which, unlike expressions, do not return a
    /// value.
    Void,
    None,
    Ellipsis,
    NotImplemented,
    /// Using a [`Container`] here is weird, but we did this because the [`Dunder::Init`] method
    /// modifies the integer in place.
    Integer(Container<i64>),
    FloatingPoint(f64),
    String(Str),
    Class(Container<Class>),
    Object(Container<Object>),
    Module(Container<Module>),
    Super(Container<Super>),
    Classmethod(Classmethod),
    Staticmethod(Staticmethod),
    Property(Property),
    DataDescriptor(Container<Box<dyn DataDescriptor>>),
    NonDataDescriptor(Container<Box<dyn NonDataDescriptor>>),
    Function(Container<Function>),
    Method(Container<Method>),
    BuiltinFunction(Container<Box<dyn Callable>>),
    BuiltinMethod(Container<Box<dyn Callable>>),
    Generator(Container<GeneratorIterator>),
    Coroutine(Container<Coroutine>),
    /// TODO this is a stub, we may need to flesh this out with bytecode if we ever want to support
    /// self-modifying code or whatever this is used for.
    Code(Container<Code>),
    Cell(Container<Cell>),
    /// An immutable string of bytes.
    Bytes(Container<Bytes>),
    ByteArray(Container<ByteArray>),
    Boolean(bool),
    List(Container<List>),
    Set(Container<Set>),
    FrozenSet(Container<FrozenSet>),
    Zip(ZipIterator),
    Slice(Slice),
    Complex(Complex),
    Dict(Container<Dict>),
    DictItems(DictItems),
    DictKeys(DictKeys),
    DictValues(DictValues),
    MappingProxy(Container<MappingProxy>),
    Range(Container<Range>),
    Tuple(Container<Tuple>),
    Exception(Box<InterpreterError>),
    Traceback(Traceback),
    Frame,
    ListIterator(ListIterator),
    ReversedIterator(ReversedIterator),
    // this might need a real SetIterator, I'm not sure yet
    SetIterator(ListIterator),
    DictItemsIterator(DictItemsIterator),
    DictKeysIterator(DictKeysIterator),
    DictValuesIterator(DictValuesIterator),
    RangeIterator(RangeIterator),
    // this might need a real TupleIterator, I'm not sure yet
    TupleIterator(ListIterator),
    StringIterator(StringIterator),
    // TODO use actual iterator here
    BytesIterator(Vec<u8>),
    // TODO use actual iterator here
    ByteArrayIterator(Vec<u8>),
    TypeNode(TypeExpr),
    #[cfg(feature = "c_stdlib")]
    CPythonModule(Container<CPythonModule>),
    #[cfg(feature = "c_stdlib")]
    CPythonObject(CPythonObject),
    #[cfg(feature = "c_stdlib")]
    CPythonClass(CPythonClass),
}

/// Implement PartialEq manually because Py<PyAny> does not implement PartialEq.
impl PartialEq for ExprResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ExprResult::Void, ExprResult::Void) => true,
            (ExprResult::None, ExprResult::None) => true,
            (ExprResult::Integer(a), ExprResult::Integer(b)) => a == b,
            (ExprResult::FloatingPoint(a), ExprResult::FloatingPoint(b)) => a == b,
            (ExprResult::String(a), ExprResult::String(b)) => a.0 == b.0,
            (ExprResult::Bytes(a), ExprResult::Bytes(b)) => a == b,
            (ExprResult::ByteArray(a), ExprResult::ByteArray(b)) => a == b,
            (ExprResult::Boolean(a), ExprResult::Boolean(b)) => a == b,
            (ExprResult::List(a), ExprResult::List(b)) => a == b,
            (ExprResult::Set(a), ExprResult::Set(b)) => a == b,
            (ExprResult::FrozenSet(a), ExprResult::FrozenSet(b)) => a == b,
            (ExprResult::Complex(a), ExprResult::Complex(b)) => a == b,
            (ExprResult::Dict(a), ExprResult::Dict(b)) => a == b,
            (ExprResult::MappingProxy(a), ExprResult::MappingProxy(b)) => a == b,
            (ExprResult::DictItems(a), ExprResult::DictItems(b)) => a == b,
            (ExprResult::DictKeys(a), ExprResult::DictKeys(b)) => a == b,
            (ExprResult::DictValues(a), ExprResult::DictValues(b)) => a == b,
            (ExprResult::Range(a), ExprResult::Range(b)) => a == b,
            (ExprResult::Tuple(a), ExprResult::Tuple(b)) => a == b,
            (ExprResult::Function(a), ExprResult::Function(b)) => a == b,
            (ExprResult::Class(a), ExprResult::Class(b)) => a == b,
            // This uses `Dunder::Eq` and is handled in [`Interpreter::evaluate_binary_operation`].
            (ExprResult::Object(_), ExprResult::Object(_)) => {
                // TODO this is very dangerous. we are ending up here because of `Dict.get`, which
                // uses a HashMap and therefore checks the equality of keys on a collision. We
                // should really be using `Dunder::Eq` here like is mentioned in the comment above.
                true
            } //unreachable!(),
            (ExprResult::Exception(a), ExprResult::Exception(b)) => a == b,
            (ExprResult::BuiltinMethod(a), ExprResult::BuiltinMethod(b)) => a.same_identity(b),
            (ExprResult::DataDescriptor(a), ExprResult::DataDescriptor(b)) => a.same_identity(b),
            (ExprResult::NonDataDescriptor(a), ExprResult::NonDataDescriptor(b)) => {
                a.same_identity(b)
            }
            _ => false,
        }
    }
}
// For some reason, we have to create this here for the Eq trait to be
// satisfied for f64.
impl Eq for ExprResult {}

impl Hash for ExprResult {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        if let ExprResult::Set(set) = self {
            for i in set.borrow().items.clone() {
                i.as_integer().unwrap().borrow().hash(state)
            }
        }
    }
}

impl ExprResult {
    pub fn new(
        interpreter: &Interpreter,
        class: Container<Class>,
        arguments: ResolvedArguments,
    ) -> Result<Self, InterpreterError> {
        // We have to handle calls to `type()` with only one parameter as a special case because
        // this doesn't actually call the `Type::Type` `Dunder::New` method, which expects more
        // arguments and would return a new class. Overloading the `Dunder::Init` method
        // here on `Type::Type` would also create unintended behaviors.
        if class.borrow().is_type(&Type::Type) {
            assert_eq!(arguments.len(), 1);
            return Ok(interpreter.state.get_type(&arguments.get_arg(0)));
        };

        // The [`Class`] must be explicitly passed to the [`Dunder::New`] method as this method is
        // never bound.
        let mut new_args = arguments.clone();
        new_args.bind_new(ExprResult::Class(class.clone()));
        let object = interpreter.evaluate_method(
            ExprResult::Class(class),
            Dunder::New.value(),
            &new_args,
        )?;

        interpreter.evaluate_method(object.clone(), Dunder::Init.value(), &arguments)?;

        Ok(object)
    }

    fn minimized_display(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ExprResult::Void => unreachable!(),
            ExprResult::None => write!(f, "None"),
            ExprResult::Ellipsis => write!(f, "Ellipsis"),
            ExprResult::NotImplemented => write!(f, "NotImplemented"),
            ExprResult::Super(_) => write!(f, "<super>"),
            ExprResult::Classmethod(_) => write!(f, "<classmethod>"),
            ExprResult::Staticmethod(_) => write!(f, "<staticmethod>"),
            ExprResult::Property(_) => write!(f, "<property>"),
            ExprResult::DataDescriptor(_) => write!(f, "<attribute <> of <> objects>"),
            ExprResult::NonDataDescriptor(_) => {
                write!(f, "<non-data attribute <> of <> objects>")
            }
            ExprResult::Class(c) => write!(f, "{}", c),
            ExprResult::Object(o) => write!(f, "{}", o),
            ExprResult::Method(m) => write!(f, "{}", m),
            ExprResult::Function(func) => write!(f, "{}", func),
            ExprResult::Generator(_) => write!(f, "<generator object>"),
            ExprResult::Coroutine(_) => write!(f, "<coroutine object>"),
            ExprResult::BuiltinFunction(func) => {
                write!(f, "<built-in function {}>", func.borrow().name())
            }
            ExprResult::BuiltinMethod(_) => write!(f, "<built-in method>"),
            ExprResult::Integer(i) => write!(f, "{}", i),
            ExprResult::FloatingPoint(i) => write!(f, "{}", i),
            ExprResult::String(s) => write!(f, "{}", s.0),
            ExprResult::Bytes(b) => write!(f, "b'{:?}'", b),
            ExprResult::ByteArray(b) => write!(f, "bytearray(b'{:?}')", b),
            ExprResult::Boolean(b) => {
                if *b {
                    write!(f, "True")
                } else {
                    write!(f, "False")
                }
            }
            ExprResult::List(l) => write!(f, "{}", l),
            ExprResult::Set(s) => write!(f, "{}", s),
            ExprResult::FrozenSet(s) => write!(f, "{}", s),
            ExprResult::Range(r) => write!(f, "{}", r.borrow()),
            ExprResult::Tuple(t) => write!(f, "{}", t),
            ExprResult::Zip(_) => write!(f, "<zip>"),
            ExprResult::Slice(s) => write!(f, "{}", s),
            ExprResult::Complex(c) => write!(f, "{}", c),
            ExprResult::Dict(d) => write!(f, "{}", d),
            ExprResult::MappingProxy(d) => write!(f, "{}", d),
            ExprResult::DictItems(d) => write!(f, "dict_items({})", d),
            ExprResult::DictKeys(d) => write!(f, "dict_keys({})", d),
            ExprResult::DictValues(d) => write!(f, "dict_values({})", d),
            ExprResult::StringIterator(_) => write!(f, "<str_ascii_iterator>"),
            ExprResult::BytesIterator(_) => write!(f, "<bytes_iterator>"),
            ExprResult::ByteArrayIterator(_) => write!(f, "<byte_array_iterator>"),
            ExprResult::ListIterator(_) => write!(f, "<list_iterator>"),
            ExprResult::ReversedIterator(_) => write!(f, "<list_reverseiterator>"),
            ExprResult::SetIterator(_) => write!(f, "<set_iterator>"),
            ExprResult::DictItemsIterator(_) => write!(f, "<dict_itemiterator>"),
            ExprResult::DictKeysIterator(_) => write!(f, "<dict_keyiterator>"),
            ExprResult::DictValuesIterator(_) => write!(f, "<dict_valueiterator>"),
            ExprResult::RangeIterator(_) => write!(f, "<range_iterator>"),
            ExprResult::TupleIterator(_) => write!(f, "<tuple_iterator>"),
            ExprResult::Code(_) => write!(f, "<code object>"),
            ExprResult::Cell(_) => write!(f, "<cell>"),
            ExprResult::Module(m) => write!(f, "{}", m),
            ExprResult::Exception(_) => write!(f, "<exception>"),
            ExprResult::Traceback(_) => write!(f, "<traceback>"),
            ExprResult::Frame => write!(f, "<frame>"),
            ExprResult::TypeNode(t) => write!(f, "<type {:?}>", t),
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonModule(i) => write!(f, "{}", i),
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonObject(_) => write!(f, "<object>"),
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonClass(_) => write!(f, "<class>"),
        }
    }

    pub fn try_into_iter(self) -> Option<ExprResultIterator> {
        match self {
            ExprResult::List(list) => Some(ExprResultIterator::List(list.clone().into_iter())),
            ExprResult::ListIterator(list_iterator) => {
                Some(ExprResultIterator::List(list_iterator))
            }
            ExprResult::ReversedIterator(list_iterator) => {
                Some(ExprResultIterator::Reversed(list_iterator))
            }
            ExprResult::Set(set) => Some(ExprResultIterator::List(set.clone().into_iter())),
            ExprResult::FrozenSet(set) => Some(ExprResultIterator::List(set.clone().into_iter())),
            ExprResult::Zip(zip) => Some(ExprResultIterator::Zip(Box::new(zip))),
            ExprResult::Tuple(list) => Some(ExprResultIterator::List(list.into_iter())),
            ExprResult::Dict(dict) => Some(ExprResultIterator::Dict(dict.into_iter())),
            ExprResult::DictItems(dict) => Some(ExprResultIterator::DictItems(dict.into_iter())),
            ExprResult::Generator(generator) => {
                Some(ExprResultIterator::Generator(generator.into_iter()))
            }
            ExprResult::Range(range) => Some(ExprResultIterator::Range(range.clone().into_iter())),
            ExprResult::StringIterator(string_iterator) => {
                Some(ExprResultIterator::String(string_iterator))
            }
            _ => None,
        }
    }

    /// Check for object identity, as opposed to object value evaluated in `PartialEq` above.
    pub fn is(&self, other: &Self) -> bool {
        match (self, other) {
            (ExprResult::None, ExprResult::None) => true,
            (ExprResult::None, _) | (_, ExprResult::None) => false,
            (ExprResult::Object(ref a), ExprResult::Object(ref b)) => a.same_identity(b),
            _ => unimplemented!(), // Different variants or not both ExprResult::Object
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            ExprResult::Void => unreachable!(),
            ExprResult::None => Type::None,
            ExprResult::Ellipsis => Type::Ellipsis,
            ExprResult::NotImplemented => Type::NotImplemented,
            ExprResult::Class(_) => Type::Type,
            ExprResult::Object(_) => Type::Object,
            ExprResult::Super(_) => Type::Super,
            ExprResult::Classmethod(_) => Type::Classmethod,
            ExprResult::Staticmethod(_) => Type::Staticmethod,
            ExprResult::Property(_) => Type::Property,
            ExprResult::DataDescriptor(_) => Type::GetSetDescriptor,
            ExprResult::NonDataDescriptor(_) => Type::MemberDescriptor,
            ExprResult::Method(_) => Type::Method,
            ExprResult::Function(_) => Type::Function,
            ExprResult::BuiltinFunction(_) => Type::BuiltinFunction,
            ExprResult::BuiltinMethod(_) => Type::BuiltinMethod,
            ExprResult::Generator(_) => Type::Generator,
            ExprResult::Coroutine(_) => Type::Coroutine,
            ExprResult::Integer(_) => Type::Int,
            ExprResult::FloatingPoint(_) => Type::Float,
            ExprResult::Bytes(_) => Type::Bytes,
            ExprResult::ByteArray(_) => Type::ByteArray,
            ExprResult::Boolean(_) => Type::Bool,
            ExprResult::String(_) => Type::Str,
            ExprResult::List(_) => Type::List,
            ExprResult::Set(_) => Type::Set,
            ExprResult::FrozenSet(_) => Type::FrozenSet,
            ExprResult::Zip(_) => Type::Zip,
            ExprResult::Tuple(_) => Type::Tuple,
            ExprResult::Range(_) => Type::Range,
            ExprResult::Slice(_) => Type::Slice,
            ExprResult::Complex(_) => Type::Complex,
            ExprResult::Dict(_) => Type::Dict,
            ExprResult::DictItems(_) => Type::DictItems,
            ExprResult::DictKeys(_) => Type::DictKeys,
            ExprResult::DictValues(_) => Type::DictValues,
            ExprResult::MappingProxy(_) => Type::MappingProxy,
            ExprResult::BytesIterator(_) => Type::BytesIterator,
            ExprResult::ByteArrayIterator(_) => Type::ByteArrayIterator,
            ExprResult::RangeIterator(_) => Type::RangeIterator,
            ExprResult::StringIterator(_) => Type::StringIterator,
            ExprResult::ListIterator(_) => Type::ListIterator,
            ExprResult::ReversedIterator(_) => Type::ReversedIterator,
            ExprResult::SetIterator(_) => Type::SetIterator,
            ExprResult::TupleIterator(_) => Type::TupleIterator,
            ExprResult::DictItemsIterator(_) => Type::DictItemIterator,
            ExprResult::DictKeysIterator(_) => Type::DictKeyIterator,
            ExprResult::DictValuesIterator(_) => Type::DictValueIterator,
            ExprResult::TypeNode(_) => Type::Type,
            ExprResult::Cell(_) => Type::Cell,
            ExprResult::Code(_) => Type::Code,
            ExprResult::Module(_) => Type::Module,
            ExprResult::Exception(_) => Type::Exception,
            ExprResult::Traceback(_) => Type::Traceback,
            ExprResult::Frame => Type::Frame,
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonModule(_) => Type::Module,
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonObject(_) => Type::Object,
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonClass(_) => unimplemented!(),
        }
    }

    pub fn get_class(&self, interpreter: &Interpreter) -> Container<Class> {
        match self {
            ExprResult::Object(o) => o.borrow().class.clone(),
            ExprResult::Class(o) => o.clone(),
            ExprResult::Super(s) => s.borrow().receiver().get_class(interpreter),
            _ => interpreter.state.get_type_class(self.get_type()).clone(),
        }
    }

    /// Return a reference to an integer if this type supports it. To get the value itself, use
    /// `as_integer_val()`.
    pub fn as_integer(&self) -> Option<Container<i64>> {
        match self {
            ExprResult::Integer(i) => Some(i.clone()),
            ExprResult::String(s) => match s.0.parse::<i64>() {
                Ok(i) => Some(Container::new(i)),
                Err(_) => None,
            },
            _ => None,
        }
    }

    /// Return an integer value if this type supports it. To get a reference, use `as_integer()`.
    pub fn as_integer_val(&self) -> Option<i64> {
        self.as_integer().map(|i| *i.borrow())
    }

    pub fn as_fp(&self) -> Option<f64> {
        match self {
            ExprResult::FloatingPoint(i) => Some(*i),
            ExprResult::Integer(i) => Some(*i.borrow() as f64),
            _ => None,
        }
    }

    pub fn as_class(&self) -> Option<Container<Class>> {
        match self {
            ExprResult::Class(i) => Some(i.clone()),
            // TODO should this use a trait interface?
            // #[cfg(feature = "c_stdlib")]
            // ExprResult::CPythonClass(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_module(&self) -> Option<Box<dyn MemberReader>> {
        match self {
            ExprResult::Module(i) => Some(Box::new(i.borrow().clone())),
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonModule(i) => Some(Box::new(i.borrow().clone())),
            _ => None,
        }
    }

    pub fn as_member_reader(&self, interpreter: &Interpreter) -> Box<dyn MemberReader> {
        match self {
            ExprResult::Object(i) => Box::new(i.clone()),
            ExprResult::Class(i) => Box::new(i.clone()),
            ExprResult::Function(i) => Box::new(i.clone()),
            ExprResult::Cell(i) => Box::new(i.borrow().clone()),
            ExprResult::Module(i) => Box::new(i.borrow().clone()),
            ExprResult::Super(i) => Box::new(i.clone()),
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonModule(i) => Box::new(i.borrow().clone()),
            _ => {
                // We need this fallback case for instances of builtin types.
                // i.e. [].append
                // All attributes fetched off the builtin types not explicitly handled above do not
                // support attribute writes, only reads of builtin attributes.
                let class = interpreter.state.get_type_class(self.get_type());
                Box::new(BuiltinObject::new(self.clone(), class))
            }
        }
    }

    pub fn as_member_writer(&self) -> Option<Box<dyn MemberWriter>> {
        match self {
            ExprResult::Object(i) => Some(Box::new(i.clone())),
            ExprResult::Class(i) => Some(Box::new(i.clone())),
            ExprResult::Function(i) => Some(Box::new(i.clone())),
            // #[cfg(feature = "c_stdlib")]
            // ExprResult::CPythonModule(i) => Some(Box::new(i.borrow().clone())),
            _ => None,
        }
    }

    fn hasattr(&self, interpreter: &Interpreter, attr: Dunder) -> bool {
        self.as_member_reader(interpreter)
            .get_member(interpreter, attr.value())
            .unwrap()
            .is_some()
    }

    fn map_hasattr(&self, interpreter: &Interpreter, attr: Dunder) -> Option<ExprResult> {
        match self.hasattr(interpreter, attr) {
            true => Some(self.clone()),
            false => None,
        }
    }

    pub fn as_index_read(&self, interpreter: &Interpreter) -> Option<Box<dyn IndexRead>> {
        match self {
            ExprResult::List(list) => Some(Box::new(list.clone())),
            ExprResult::Tuple(tuple) => Some(Box::new(tuple.clone())),
            ExprResult::Dict(dict) => Some(Box::new(dict.clone())),
            ExprResult::MappingProxy(proxy) => Some(Box::new(proxy.clone())),
            ExprResult::String(s) => Some(Box::new(s.clone())),
            ExprResult::Object(i) => self
                .map_hasattr(interpreter, Dunder::GetItem)
                .map(|_| Box::new(i.clone()) as Box<dyn IndexRead>),
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonObject(o) => match o.hasattr(Dunder::GetItem.value()) {
                true => Some(Box::new(o.clone())),
                false => None,
            },
            _ => None,
        }
    }

    pub fn as_index_write(&self, interpreter: &Interpreter) -> Option<Box<dyn IndexWrite>> {
        match self {
            ExprResult::List(list) => Some(Box::new(list.clone())),
            ExprResult::Dict(dict) => Some(Box::new(dict.clone())),
            ExprResult::Object(i) => self
                .map_hasattr(interpreter, Dunder::SetItem)
                .map(|_| Box::new(i.clone()) as Box<dyn IndexWrite>),
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonObject(o) => match o.hasattr(Dunder::SetItem.value()) {
                true => Some(Box::new(o.clone())),
                false => None,
            },
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<Container<Function>> {
        match self {
            ExprResult::Function(i) => Some(i.clone()),
            _ => None,
        }
    }

    fn as_nondata_descriptor(
        &self,
        interpreter: &Interpreter,
    ) -> Result<Option<Container<Box<dyn NonDataDescriptor>>>, InterpreterError> {
        Ok(match self {
            ExprResult::NonDataDescriptor(i) => Some(i.clone()),
            ExprResult::Object(i) => self
                .map_hasattr(interpreter, Dunder::Get)
                .map(|_| Container::new(Box::new(i.clone()) as Box<dyn NonDataDescriptor>)),
            ExprResult::Classmethod(i) => Some(Container::new(Box::new(i.clone()))),
            ExprResult::Staticmethod(i) => Some(Container::new(Box::new(i.clone()))),
            ExprResult::Property(i) => Some(Container::new(Box::new(i.clone()))),
            _ => None,
        })
    }

    pub fn as_data_descriptor(
        &self,
        interpreter: &Interpreter,
    ) -> Result<Option<Container<Box<dyn DataDescriptor>>>, InterpreterError> {
        Ok(match self {
            ExprResult::Object(i) => self
                .map_hasattr(interpreter, Dunder::Set)
                .map(|_| Container::new(Box::new(i.clone()) as Box<dyn DataDescriptor>)),
            ExprResult::DataDescriptor(i) => Some(i.clone()),
            // TODO handle property here
            // ExprResult::Property(i) => Some(Container::new(Box::new(i.clone()))),
            _ => None,
        })
    }

    pub fn resolve_nondata_descriptor(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        // Similar to callable below, ideally we'd be able to handle this inside
        // `Result::as_nondata_descriptor` but we don't yet have a way to downcast in this way
        // (i.e. treat `S` as a different `dyn T` when `S : T`)
        if let Some(descriptor) = self.as_data_descriptor(interpreter)? {
            return descriptor.borrow().get_attr(interpreter, instance, owner);
        }

        // I'd love to find a way to combine this into [`Result::as_nondata_descriptor`] and move
        // this functionality onto the [`Callable`] trait somehow.
        if let Some(callable) = self.as_callable() {
            // The new method is never bound. When called explicitly inside other metaclasses, the
            // class must be passed in by the calling metaclass.
            if callable.borrow().name() == Dunder::New.value() {
                return Ok(self.clone());
            }

            return Ok(match instance {
                Some(instance) => {
                    ExprResult::Method(Container::new(Method::new(instance, callable)))
                }
                None => self.clone(),
            });
        }

        match self.as_nondata_descriptor(interpreter)? {
            Some(descriptor) => descriptor.borrow().get_attr(interpreter, instance, owner),
            None => Ok(self.clone()),
        }
    }

    pub fn as_callable(&self) -> Option<Container<Box<dyn Callable>>> {
        match self {
            ExprResult::Function(i) => Some(Container::new(Box::new(i.clone()))),
            ExprResult::Method(i) => Some(Container::new(Box::new(i.clone()))),
            ExprResult::BuiltinMethod(i) => Some(i.clone()),
            ExprResult::BuiltinFunction(i) => Some(i.clone()),
            ExprResult::Class(i) => Some(Container::new(Box::new(i.clone()))),
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonObject(i) => Some(Container::new(Box::new(i.clone()))),
            _ => None,
        }
    }

    pub fn as_generator(&self) -> Option<Container<GeneratorIterator>> {
        match self {
            ExprResult::Generator(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn as_coroutine(&self) -> Option<Container<Coroutine>> {
        match self {
            ExprResult::Coroutine(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            ExprResult::Boolean(i) => *i,
            ExprResult::List(i) => i.borrow().len() > 0,
            ExprResult::String(i) => !i.0.is_empty(),
            ExprResult::Integer(i) => *i.borrow() != 0,
            ExprResult::None => false,
            _ => true,
        }
    }

    pub fn as_object(&self) -> Option<Container<Object>> {
        match self {
            ExprResult::Object(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn as_list(&self) -> Option<Container<List>> {
        match self {
            ExprResult::List(list) => Some(list.clone()),
            ExprResult::Set(set) => Some(set.clone().into()),
            ExprResult::Tuple(tuple) => Some(tuple.clone().into()),
            ExprResult::Range(range) => Some(range.clone().into()),
            ExprResult::Generator(g) => Some(g.clone().into()),
            _ => None,
        }
    }

    pub fn as_set(&self) -> Option<Container<Set>> {
        match self {
            ExprResult::Set(set) => Some(set.clone()),
            ExprResult::List(list) => Some(list.clone().into()),
            ExprResult::Tuple(tuple) => Some(tuple.clone().into()),
            ExprResult::Range(range) => Some(range.clone().into()),
            _ => None,
        }
    }

    pub fn as_dict(&self) -> Option<Container<Dict>> {
        match self {
            ExprResult::Dict(i) => Some(i.clone()),
            ExprResult::List(list) => {
                let di: DictItems = list.clone().into();
                let d: Dict = di.into();
                Some(Container::new(d))
            }
            _ => None,
        }
    }

    pub fn as_range(&self) -> Option<Container<Range>> {
        match self {
            ExprResult::Range(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn as_tuple(&self) -> Option<Container<Tuple>> {
        match self {
            ExprResult::List(i) => Some(i.clone().into()),
            ExprResult::Tuple(i) => Some(i.clone()),
            ExprResult::Set(set) => Some(set.clone().into()),
            ExprResult::Range(range) => Some(range.clone().into()),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<String> {
        match self {
            ExprResult::String(i) => Some(i.0.to_string()),
            ExprResult::Integer(i) => Some(i.to_string()),
            _ => None,
        }
    }

    pub fn negated(&self) -> Self {
        match self {
            ExprResult::FloatingPoint(i) => ExprResult::FloatingPoint(-*i),
            ExprResult::Integer(i) => {
                let old_val = *i.borrow();
                *i.borrow_mut() = -old_val;
                ExprResult::Integer(i.clone())
            }
            _ => unreachable!(),
        }
    }

    pub fn inverted(&self) -> Self {
        match self {
            ExprResult::Boolean(i) => ExprResult::Boolean(!i),
            _ => unreachable!(),
        }
    }

    pub fn is_integer(&self) -> bool {
        self.as_integer().is_some()
    }

    pub fn is_fp(&self) -> bool {
        self.as_fp().is_some()
    }

    pub fn is_class(&self) -> bool {
        self.as_class().is_some()
    }

    pub fn is_module(&self) -> bool {
        self.as_module().is_some()
    }
}

impl Voidable for ExprResult {
    fn is_void(&self) -> bool {
        matches!(self, ExprResult::Void)
    }
}

impl Display for ExprResult {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.minimized_display(f)
    }
}

impl fmt::Debug for ExprResult {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.minimized_display(f)
    }
}

#[derive(Clone)]
pub enum ExprResultIterator {
    List(ListIterator),
    Zip(Box<ZipIterator>),
    Reversed(ReversedIterator),
    Dict(DictKeysIterator),
    DictItems(DictItemsIterator),
    Generator(GeneratorIterator),
    Range(RangeIterator),
    String(StringIterator),
}

impl ExprResultIterator {
    pub fn contains(&mut self, item: ExprResult) -> bool {
        for next_item in self.by_ref() {
            if next_item == item {
                return true;
            }
        }

        false
    }
}

impl Iterator for ExprResultIterator {
    type Item = ExprResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ExprResultIterator::List(i) => i.next(),
            ExprResultIterator::Zip(i) => i.next(),
            ExprResultIterator::Reversed(i) => i.next(),
            ExprResultIterator::Dict(i) => i.next(),
            ExprResultIterator::DictItems(i) => i.next(),
            ExprResultIterator::Generator(i) => i.next(),
            ExprResultIterator::Range(i) => i.next(),
            ExprResultIterator::String(i) => i.next(),
        }
    }
}

impl IntoIterator for ExprResult {
    type Item = ExprResult;
    type IntoIter = ExprResultIterator;

    fn into_iter(self) -> Self::IntoIter {
        let type_ = &self.get_type();
        self.try_into_iter()
            .unwrap_or_else(|| panic!("attempted to call IntoIterator on a {}!", type_.value()))
    }
}
