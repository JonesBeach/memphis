use std::{
    cell::{Ref, RefMut},
    cmp::Ordering,
    fmt::{Debug, Display, Error, Formatter},
    hash::{Hash, Hasher},
    ptr,
};

#[cfg(feature = "c_stdlib")]
use crate::treewalk::types::cpython::{CPythonClass, CPythonModule, CPythonObject};
use crate::{
    core::{floats_equal, Container, Voidable},
    domain::{DomainResult, Dunder, ExecutionError, MemphisValue, RuntimeError, Type},
    treewalk::{
        protocols::MemberRead,
        type_system::{
            CloneableCallable, CloneableDataDescriptor, CloneableIterable,
            CloneableNonDataDescriptor,
        },
        types::{
            iterators::{
                DictItemsIter, DictKeysIter, DictValuesIter, GeneratorIter, ListIter, RangeIter,
                ReversedIter, SetIter, StrIter, TupleIter, ZipIterator,
            },
            ByteArray, Cell, Class, Classmethod, Code, Complex, Coroutine, Dict, DictItems,
            DictKeys, DictValues, FrozenSet, Function, List, MappingProxy, Method, Module, Object,
            Property, Range, Set, Slice, Staticmethod, StopIteration, Str, Super, Traceback, Tuple,
        },
        typing::TypeExpr,
        utils::Args,
        SymbolTable, TreewalkInterpreter, TreewalkResult,
    },
};

#[derive(Clone)]
pub enum TreewalkValue {
    None,
    Ellipsis,
    NotImplemented,
    Int(i64),
    Float(f64),
    Str(Str),
    Class(Container<Class>),
    Object(Container<Object>),
    Module(Container<Module>),
    Super(Super),
    Classmethod(Classmethod),
    Staticmethod(Staticmethod),
    Property(Property),
    DataDescriptor(Box<dyn CloneableDataDescriptor>),
    NonDataDescriptor(Box<dyn CloneableNonDataDescriptor>),
    Function(Container<Function>),
    Method(Container<Method>),
    BuiltinFunction(Box<dyn CloneableCallable>),
    BuiltinMethod(Box<dyn CloneableCallable>),
    Generator(GeneratorIter),
    Coroutine(Container<Coroutine>),
    Code(Code),
    Cell(Container<Cell>),
    Bytes(Vec<u8>),
    ByteArray(Container<ByteArray>),
    Bool(bool),
    List(Container<List>),
    Set(Container<Set>),
    FrozenSet(FrozenSet),
    Zip(ZipIterator),
    Slice(Slice),
    Complex(Complex),
    Dict(Container<Dict>),
    DictItems(DictItems),
    DictKeys(DictKeys),
    DictValues(DictValues),
    MappingProxy(MappingProxy),
    Range(Range),
    Tuple(Tuple),
    Exception(RuntimeError),
    /// Only constructed when a `StopIteration` exception is caught and aliased,
    /// e.g., `except StopIteration as e:`. Mirrors CPython behavior to expose `.value`.
    StopIteration(Box<StopIteration>),
    Traceback(Traceback),
    Frame,
    ListIter(ListIter),
    ReversedIter(ReversedIter),
    SetIter(SetIter),
    DictItemsIter(DictItemsIter),
    DictKeysIter(DictKeysIter),
    DictValuesIter(DictValuesIter),
    RangeIter(RangeIter),
    TupleIter(TupleIter),
    StrIter(StrIter),
    BytesIter(Vec<u8>),
    ByteArrayIter(Vec<u8>),
    TypeNode(TypeExpr),
    #[cfg(feature = "c_stdlib")]
    CPythonModule(Container<CPythonModule>),
    #[cfg(feature = "c_stdlib")]
    CPythonObject(CPythonObject),
    #[cfg(feature = "c_stdlib")]
    CPythonClass(CPythonClass),
}

/// Implement PartialEq manually because Py<PyAny> does not implement PartialEq.
impl PartialEq for TreewalkValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TreewalkValue::None, TreewalkValue::None) => true,
            (TreewalkValue::Int(a), TreewalkValue::Int(b)) => a == b,
            (TreewalkValue::Float(a), TreewalkValue::Float(b)) => floats_equal(*a, *b),
            (TreewalkValue::Int(a), TreewalkValue::Float(b)) => floats_equal(*a as f64, *b),
            (TreewalkValue::Float(a), TreewalkValue::Int(b)) => floats_equal(*a, *b as f64),
            (TreewalkValue::Str(a), TreewalkValue::Str(b)) => a == b,
            (TreewalkValue::Bytes(a), TreewalkValue::Bytes(b)) => a == b,
            (TreewalkValue::ByteArray(a), TreewalkValue::ByteArray(b)) => a == b,
            (TreewalkValue::Bool(a), TreewalkValue::Bool(b)) => a == b,
            (TreewalkValue::List(a), TreewalkValue::List(b)) => a == b,
            (TreewalkValue::Set(a), TreewalkValue::Set(b)) => a == b,
            (TreewalkValue::FrozenSet(a), TreewalkValue::FrozenSet(b)) => a == b,
            (TreewalkValue::Complex(a), TreewalkValue::Complex(b)) => a == b,
            (TreewalkValue::Dict(a), TreewalkValue::Dict(b)) => a == b,
            (TreewalkValue::MappingProxy(a), TreewalkValue::MappingProxy(b)) => a == b,
            (TreewalkValue::DictItems(a), TreewalkValue::DictItems(b)) => a == b,
            (TreewalkValue::DictKeys(a), TreewalkValue::DictKeys(b)) => a == b,
            (TreewalkValue::DictValues(a), TreewalkValue::DictValues(b)) => a == b,
            (TreewalkValue::Range(a), TreewalkValue::Range(b)) => a == b,
            (TreewalkValue::Tuple(a), TreewalkValue::Tuple(b)) => a == b,
            (TreewalkValue::Function(a), TreewalkValue::Function(b)) => a == b,
            (TreewalkValue::Class(a), TreewalkValue::Class(b)) => a == b,
            (TreewalkValue::Object(a), TreewalkValue::Object(b)) => a.same_identity(b),
            (TreewalkValue::Exception(a), TreewalkValue::Exception(b)) => a == b,
            (TreewalkValue::BuiltinMethod(a), TreewalkValue::BuiltinMethod(b)) => {
                ptr::eq(a.as_ref(), b.as_ref())
            }
            (TreewalkValue::DataDescriptor(a), TreewalkValue::DataDescriptor(b)) => {
                ptr::eq(a.as_ref(), b.as_ref())
            }
            (TreewalkValue::NonDataDescriptor(a), TreewalkValue::NonDataDescriptor(b)) => {
                ptr::eq(a.as_ref(), b.as_ref())
            }
            _ => false,
        }
    }
}

// This is a marker trait. We are confirming that PartialEq fully satisfies equality semantics.
// We cannot derive Eq because it is not implemented for f64 because of NaN weirdness.
impl Eq for TreewalkValue {}

impl Hash for TreewalkValue {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        if let TreewalkValue::Set(set) = self {
            for i in set.borrow().iter() {
                i.as_int().unwrap().hash(state)
            }
        }
    }
}

impl Ord for TreewalkValue {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (TreewalkValue::Str(s1), TreewalkValue::Str(s2)) => s1.cmp(s2),
            (TreewalkValue::Int(n1), TreewalkValue::Int(n2)) => n1.cmp(n2),
            _ => todo!(),
        }
    }
}

// Implement the PartialOrd trait, required by Ord
impl PartialOrd for TreewalkValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl TreewalkValue {
    pub fn new(
        interpreter: &TreewalkInterpreter,
        class: Container<Class>,
        args: Args,
    ) -> TreewalkResult<Self> {
        // We have to handle calls to `type()` with only one parameter as a special case because
        // this doesn't actually call the `Type::Type` `Dunder::New` method, which expects more
        // arguments and would return a new class. Overloading the `Dunder::Init` method
        // here on `Type::Type` would also create unintended behaviors.
        if class.borrow().is_type(&Type::Type) {
            assert_eq!(args.len(), 1);
            return Ok(interpreter.state.class_of_value(&args.get_arg(0)));
        };

        // The [`Class`] must be explicitly passed to the [`Dunder::New`] method as this method is
        // never bound.
        // We clone here because these args will be consumed by the `Dunder::New` method call and
        // we still need a version of these for method call to `Dunder::Init`.
        let new_args = args
            .clone()
            .with_bound_new(TreewalkValue::Class(class.clone()));
        let object =
            interpreter.invoke_method(&TreewalkValue::Class(class), Dunder::New, new_args)?;

        interpreter.invoke_method(&object, Dunder::Init, args)?;

        Ok(object)
    }

    pub fn hash(&self) -> usize {
        match self {
            TreewalkValue::Object(o) => o.address(),
            TreewalkValue::Class(o) => o.address(),
            TreewalkValue::Int(i) => *i as usize,
            _ => 0,
        }
    }

    fn minimized_display(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            TreewalkValue::None => write!(f, "None"),
            TreewalkValue::Ellipsis => write!(f, "Ellipsis"),
            TreewalkValue::NotImplemented => write!(f, "NotImplemented"),
            TreewalkValue::Super(_) => write!(f, "<super>"),
            TreewalkValue::Classmethod(_) => write!(f, "<classmethod>"),
            TreewalkValue::Staticmethod(_) => write!(f, "<staticmethod>"),
            TreewalkValue::Property(_) => write!(f, "<property>"),
            TreewalkValue::DataDescriptor(_) => write!(f, "<attribute <> of <> objects>"),
            TreewalkValue::NonDataDescriptor(_) => {
                write!(f, "<non-data attribute <> of <> objects>")
            }
            TreewalkValue::Class(c) => write!(f, "{c}"),
            TreewalkValue::Object(o) => write!(f, "{o}"),
            TreewalkValue::Method(m) => write!(f, "{m}"),
            TreewalkValue::Function(func) => write!(f, "{func}"),
            TreewalkValue::Generator(_) => write!(f, "<generator object>"),
            TreewalkValue::Coroutine(_) => write!(f, "<coroutine object>"),
            TreewalkValue::BuiltinFunction(func) => {
                write!(f, "<built-in function {}>", func.name())
            }
            TreewalkValue::BuiltinMethod(_) => write!(f, "<built-in method>"),
            TreewalkValue::Int(i) => write!(f, "{i}"),
            TreewalkValue::Float(i) => {
                // We should probably move this onto Float eventually
                if i.fract() == 0.0 {
                    // integer-like float, force ".0"
                    write!(f, "{}.0", i.trunc())
                } else {
                    write!(f, "{}", i)
                }
            }
            TreewalkValue::Str(s) => write!(f, "{s}"),
            TreewalkValue::Bytes(b) => {
                // Similar to Float, we should probably move this onto Bytes
                write!(f, "b'")?;
                for &byte in b {
                    match byte {
                        b'\n' => write!(f, "\\n")?,
                        b'\r' => write!(f, "\\r")?,
                        b'\t' => write!(f, "\\t")?,
                        b'\'' => write!(f, "\\'")?,
                        b'\\' => write!(f, "\\\\")?,
                        32..=126 => write!(f, "{}", byte as char)?, // printable ASCII
                        _ => write!(f, "\\x{:02x}", byte)?,         // hex escape
                    }
                }
                write!(f, "'")
            }
            TreewalkValue::ByteArray(b) => {
                write!(f, "bytearray(")?;
                Display::fmt(&TreewalkValue::Bytes(b.borrow().raw().to_vec()), f)?;
                write!(f, ")")
            }
            TreewalkValue::Bool(b) => match b {
                true => write!(f, "True"),
                false => write!(f, "False"),
            },
            TreewalkValue::List(l) => write!(f, "{l}"),
            TreewalkValue::Set(s) => write!(f, "{s}"),
            TreewalkValue::FrozenSet(s) => write!(f, "{s}"),
            TreewalkValue::Range(r) => write!(f, "{r}"),
            TreewalkValue::Tuple(t) => write!(f, "{t}"),
            TreewalkValue::Zip(_) => write!(f, "<zip>"),
            TreewalkValue::Slice(s) => write!(f, "{s}"),
            TreewalkValue::Complex(c) => write!(f, "{c}"),
            TreewalkValue::Dict(d) => write!(f, "{d}"),
            TreewalkValue::MappingProxy(d) => write!(f, "{d}"),
            TreewalkValue::DictItems(d) => write!(f, "dict_items({d})"),
            TreewalkValue::DictKeys(d) => write!(f, "dict_keys({d})"),
            TreewalkValue::DictValues(d) => write!(f, "dict_values({d})"),
            TreewalkValue::StrIter(_) => write!(f, "<str_ascii_iterator>"),
            TreewalkValue::BytesIter(_) => write!(f, "<bytes_iterator>"),
            TreewalkValue::ByteArrayIter(_) => write!(f, "<byte_array_iterator>"),
            TreewalkValue::ListIter(_) => write!(f, "<list_iterator>"),
            TreewalkValue::ReversedIter(_) => write!(f, "<list_reverseiterator>"),
            TreewalkValue::SetIter(_) => write!(f, "<set_iterator>"),
            TreewalkValue::DictItemsIter(_) => write!(f, "<dict_itemiterator>"),
            TreewalkValue::DictKeysIter(_) => write!(f, "<dict_keyiterator>"),
            TreewalkValue::DictValuesIter(_) => write!(f, "<dict_valueiterator>"),
            TreewalkValue::RangeIter(_) => write!(f, "<range_iterator>"),
            TreewalkValue::TupleIter(_) => write!(f, "<tuple_iterator>"),
            TreewalkValue::Code(_) => write!(f, "<code object>"),
            TreewalkValue::Cell(_) => write!(f, "<cell>"),
            TreewalkValue::Module(m) => write!(f, "{m}"),
            TreewalkValue::Exception(_) => write!(f, "<exception>"),
            TreewalkValue::StopIteration(_) => write!(f, "<stop_iteration>"),
            TreewalkValue::Traceback(_) => write!(f, "<traceback>"),
            TreewalkValue::Frame => write!(f, "<frame>"),
            TreewalkValue::TypeNode(t) => write!(f, "<type {t:?}>"),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(m) => write!(f, "{m}"),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonObject(o) => write!(f, "{o}"),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonClass(_) => write!(f, "<class>"),
        }
    }

    /// Check for object identity, as opposed to object value evaluated in `PartialEq` above.
    pub fn is(&self, other: &Self) -> bool {
        match (self, other) {
            (TreewalkValue::None, TreewalkValue::None) => true,
            (TreewalkValue::None, _) | (_, TreewalkValue::None) => false,
            (TreewalkValue::Object(ref a), TreewalkValue::Object(ref b)) => a.same_identity(b),
            _ => unimplemented!(), // Different variants or not both TreewalkValue::Object
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            TreewalkValue::None => Type::None,
            TreewalkValue::Ellipsis => Type::Ellipsis,
            TreewalkValue::NotImplemented => Type::NotImplemented,
            TreewalkValue::Class(_) => Type::Type,
            TreewalkValue::Object(_) => Type::Object,
            TreewalkValue::Super(_) => Type::Super,
            TreewalkValue::Classmethod(_) => Type::Classmethod,
            TreewalkValue::Staticmethod(_) => Type::Staticmethod,
            TreewalkValue::Property(_) => Type::Property,
            TreewalkValue::DataDescriptor(_) => Type::GetSetDescriptor,
            TreewalkValue::NonDataDescriptor(_) => Type::MemberDescriptor,
            TreewalkValue::Method(_) => Type::Method,
            TreewalkValue::Function(_) => Type::Function,
            TreewalkValue::BuiltinFunction(_) => Type::BuiltinFunction,
            TreewalkValue::BuiltinMethod(_) => Type::BuiltinMethod,
            TreewalkValue::Generator(_) => Type::Generator,
            TreewalkValue::Coroutine(_) => Type::Coroutine,
            TreewalkValue::Int(_) => Type::Int,
            TreewalkValue::Float(_) => Type::Float,
            TreewalkValue::Bytes(_) => Type::Bytes,
            TreewalkValue::ByteArray(_) => Type::ByteArray,
            TreewalkValue::Bool(_) => Type::Bool,
            TreewalkValue::Str(_) => Type::Str,
            TreewalkValue::List(_) => Type::List,
            TreewalkValue::Set(_) => Type::Set,
            TreewalkValue::FrozenSet(_) => Type::FrozenSet,
            TreewalkValue::Zip(_) => Type::Zip,
            TreewalkValue::Tuple(_) => Type::Tuple,
            TreewalkValue::Range(_) => Type::Range,
            TreewalkValue::Slice(_) => Type::Slice,
            TreewalkValue::Complex(_) => Type::Complex,
            TreewalkValue::Dict(_) => Type::Dict,
            TreewalkValue::DictItems(_) => Type::DictItems,
            TreewalkValue::DictKeys(_) => Type::DictKeys,
            TreewalkValue::DictValues(_) => Type::DictValues,
            TreewalkValue::MappingProxy(_) => Type::MappingProxy,
            TreewalkValue::BytesIter(_) => Type::BytesIter,
            TreewalkValue::ByteArrayIter(_) => Type::ByteArrayIter,
            TreewalkValue::RangeIter(_) => Type::RangeIter,
            TreewalkValue::StrIter(_) => Type::StrIter,
            TreewalkValue::ListIter(_) => Type::ListIter,
            TreewalkValue::ReversedIter(_) => Type::ReversedIter,
            TreewalkValue::SetIter(_) => Type::SetIter,
            TreewalkValue::TupleIter(_) => Type::TupleIter,
            TreewalkValue::DictItemsIter(_) => Type::DictItemIter,
            TreewalkValue::DictKeysIter(_) => Type::DictKeyIter,
            TreewalkValue::DictValuesIter(_) => Type::DictValueIter,
            TreewalkValue::TypeNode(_) => Type::Type,
            TreewalkValue::Cell(_) => Type::Cell,
            TreewalkValue::Code(_) => Type::Code,
            TreewalkValue::Module(_) => Type::Module,
            TreewalkValue::Exception(_) => Type::Exception,
            TreewalkValue::StopIteration(_) => Type::StopIteration,
            TreewalkValue::Traceback(_) => Type::Traceback,
            TreewalkValue::Frame => Type::Frame,
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(_) => Type::Module,
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonObject(_) => Type::Object,
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonClass(_) => unimplemented!(),
        }
    }

    pub fn get_class(&self, interpreter: &TreewalkInterpreter) -> Container<Class> {
        match self {
            TreewalkValue::Object(o) => o.borrow().class(),
            TreewalkValue::Class(o) => o.clone(),
            TreewalkValue::Super(s) => s.receiver().get_class(interpreter),
            _ => interpreter.state.class_of_type(&self.get_type()).clone(),
        }
    }

    pub fn resolve_descriptor(
        self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        // Similar to callable below, ideally we'd be able to handle this inside
        // `Result::as_nondata_descriptor` but we don't yet have a way to downcast in this way
        // (i.e. treat `S` as a different `dyn T` when `S : T`)
        if let Some(descriptor) = self.clone().into_data_descriptor(interpreter)? {
            return descriptor.get_attr(interpreter, instance, owner);
        }

        // I'd love to find a way to combine this into [`Result::as_nondata_descriptor`] and move
        // this functionality onto the [`Callable`] trait somehow.
        if let Ok(callable) = self.clone().as_callable() {
            // The new method is never bound. When called explicitly inside other metaclasses, the
            // class must be passed in by the calling metaclass.
            if callable.name() == String::from(Dunder::New) {
                return Ok(self.clone());
            }

            return Ok(match instance {
                Some(instance) => {
                    TreewalkValue::Method(Container::new(Method::new(instance, callable)))
                }
                None => self.clone(),
            });
        }

        match self.clone().into_nondata_descriptor(interpreter)? {
            Some(descriptor) => descriptor.get_attr(interpreter, instance, owner),
            None => Ok(self.clone()),
        }
    }

    /// Ensure this value is iterable, then convert it to an iterator (if needed).
    ///
    /// This mimics the behavior of calling `iter(x)` in Python.
    /// Used when interpreting `for x in y:` or any construct that expects an iterable.
    /// Raises a TypeError if the object is not iterable.
    pub fn as_iterator(&self) -> DomainResult<Box<dyn CloneableIterable>> {
        self.clone().as_iterable()?.as_iterator_strict()
    }

    /// Convert a value into its iterator form, consuming the value in the process.
    ///
    /// This corresponds to the behavior of Python’s `iter()`:
    /// for a list, string, set, etc., it returns a corresponding iterator.
    /// If the value is already an iterator, it is returned as-is.
    /// If the value is not iterable, returns None.
    #[allow(clippy::wrong_self_convention)]
    pub fn as_iterable(self) -> DomainResult<TreewalkValue> {
        let value = match self {
            TreewalkValue::List(list) => TreewalkValue::ListIter(list.into_iter()),
            TreewalkValue::ListIter(_) => self,
            TreewalkValue::Str(s) => TreewalkValue::StrIter(s.into_iter()),
            TreewalkValue::StrIter(_) => self,
            TreewalkValue::Set(set) => TreewalkValue::SetIter(set.into_iter()),
            TreewalkValue::FrozenSet(set) => TreewalkValue::SetIter(set.into_iter()),
            TreewalkValue::SetIter(_) => self,
            TreewalkValue::Tuple(tuple) => TreewalkValue::TupleIter(tuple.into_iter()),
            TreewalkValue::TupleIter(_) => self,
            TreewalkValue::Dict(dict) => TreewalkValue::DictKeysIter(dict.into_iter()),
            TreewalkValue::DictItems(dict) => TreewalkValue::DictItemsIter(dict.into_iter()),
            TreewalkValue::DictItemsIter(_) => self,
            TreewalkValue::DictKeys(dict) => TreewalkValue::DictKeysIter(dict.into_iter()),
            TreewalkValue::DictKeysIter(_) => self,
            TreewalkValue::DictValues(dict) => TreewalkValue::DictValuesIter(dict.into_iter()),
            TreewalkValue::DictValuesIter(_) => self,
            TreewalkValue::Bytes(b) => TreewalkValue::BytesIter(b),
            TreewalkValue::BytesIter(_) => self,
            TreewalkValue::ByteArray(b) => TreewalkValue::ByteArrayIter(b.borrow().raw().to_vec()),
            TreewalkValue::ByteArrayIter(_) => self,
            TreewalkValue::Range(r) => TreewalkValue::RangeIter(r.into_iter()),
            TreewalkValue::RangeIter(_) => self,
            TreewalkValue::Generator(_) => self,
            TreewalkValue::ReversedIter(_) => self,
            TreewalkValue::Zip(_) => self,
            _ => {
                return Err(ExecutionError::type_error(format!(
                    "'{}' object is not iterable",
                    self.get_type()
                )))
            }
        };

        Ok(value)
    }

    pub fn coerce_to_int(&self) -> DomainResult<i64> {
        match self {
            TreewalkValue::Int(i) => Ok(*i),
            TreewalkValue::Str(s) => s
                .parse::<i64>()
                .map_err(|_| ExecutionError::type_error("invalid int literal")),
            _ => Err(ExecutionError::type_error("Cannot coerce to an int")),
        }
    }

    pub fn coerce_to_float(&self) -> DomainResult<f64> {
        match self {
            TreewalkValue::Float(i) => Ok(*i),
            TreewalkValue::Int(i) => Ok(*i as f64),
            _ => Err(ExecutionError::type_error("Cannot coerce to a float")),
        }
    }

    pub fn coerce_to_boolean(&self) -> bool {
        match self {
            TreewalkValue::Bool(i) => *i,
            TreewalkValue::List(i) => !i.borrow().is_empty(),
            TreewalkValue::Tuple(i) => !i.is_empty(),
            TreewalkValue::Str(i) => !i.is_empty(),
            TreewalkValue::Int(i) => *i != 0,
            TreewalkValue::Float(i) => *i != 0.0,
            TreewalkValue::None => false,
            _ => true,
        }
    }

    pub fn as_native_object<T: 'static>(&self) -> DomainResult<Ref<'_, T>> {
        match self {
            TreewalkValue::Object(obj) => {
                let binding = obj.borrow();
                Ref::filter_map(binding, |any| any.downcast_ref::<T>()).map_err(|_| {
                    ExecutionError::type_error(format!(
                        "Expected native object of type {}",
                        std::any::type_name::<T>()
                    ))
                })
            }
            _ => Err(ExecutionError::type_error(format!(
                "Expected native object of type {}",
                std::any::type_name::<T>()
            ))),
        }
    }

    pub fn as_native_object_mut<T: 'static>(&self) -> DomainResult<RefMut<'_, T>> {
        match self {
            TreewalkValue::Object(obj) => {
                let binding = obj.borrow_mut();
                RefMut::filter_map(binding, |any| any.downcast_mut::<T>()).map_err(|_| {
                    ExecutionError::type_error(format!(
                        "Expected native object of type {}",
                        std::any::type_name::<T>()
                    ))
                })
            }
            _ => Err(ExecutionError::type_error(format!(
                "Expected native object of type {}",
                std::any::type_name::<T>()
            ))),
        }
    }

    pub fn as_module(&self) -> DomainResult<Box<dyn MemberRead>> {
        match self {
            TreewalkValue::Module(i) => Ok(Box::new(i.borrow().clone())),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(i) => Ok(Box::new(i.borrow().clone())),
            _ => Err(ExecutionError::type_error("Expected an int")),
        }
    }

    pub fn as_symbol_table(&self) -> DomainResult<SymbolTable> {
        match self {
            TreewalkValue::Dict(dict) => Ok(dict.borrow().to_symbol_table()?),
            _ => Err(ExecutionError::type_error("Expected a dict with str keys")),
        }
    }

    pub fn as_int(&self) -> DomainResult<i64> {
        match self {
            TreewalkValue::Int(i) => Ok(*i),
            _ => Err(ExecutionError::type_error("Expected an int")),
        }
    }

    pub fn as_float(&self) -> DomainResult<f64> {
        match self {
            TreewalkValue::Float(i) => Ok(*i),
            _ => Err(ExecutionError::type_error("Expected a float")),
        }
    }

    pub fn as_tuple(&self) -> DomainResult<Tuple> {
        match self {
            TreewalkValue::Tuple(i) => Ok(i.clone()),
            _ => Err(ExecutionError::type_error("Expected a tuple")),
        }
    }

    pub fn as_str(&self) -> DomainResult<String> {
        match self {
            TreewalkValue::Str(i) => Ok(i.to_string()),
            _ => Err(ExecutionError::type_error("Expected a string")),
        }
    }

    pub fn as_dict(&self) -> DomainResult<Container<Dict>> {
        match self {
            TreewalkValue::Dict(i) => Ok(i.clone()),
            _ => Err(ExecutionError::type_error("Expected a dict")),
        }
    }

    pub fn as_class(&self) -> DomainResult<Container<Class>> {
        match self {
            TreewalkValue::Class(i) => Ok(i.clone()),
            _ => Err(ExecutionError::type_error("Expected a class")),
        }
    }

    pub fn as_coroutine(&self) -> DomainResult<Container<Coroutine>> {
        match self {
            TreewalkValue::Coroutine(i) => Ok(i.clone()),
            _ => Err(ExecutionError::type_error("Expected a coroutine")),
        }
    }

    pub fn as_function(&self) -> DomainResult<Container<Function>> {
        match self {
            TreewalkValue::Function(i) => Ok(i.clone()),
            _ => Err(ExecutionError::type_error("Expected a function")),
        }
    }

    pub fn as_list(&self) -> DomainResult<Container<List>> {
        match self {
            TreewalkValue::List(i) => Ok(i.clone()),
            _ => Err(ExecutionError::type_error("Expected a list")),
        }
    }

    pub fn as_object(&self) -> DomainResult<Container<Object>> {
        match self {
            TreewalkValue::Object(i) => Ok(i.clone()),
            _ => Err(ExecutionError::type_error("Expected an object")),
        }
    }

    pub fn as_set(&self) -> DomainResult<Container<Set>> {
        match self {
            TreewalkValue::Set(i) => Ok(i.clone()),
            _ => Err(ExecutionError::type_error("Expected a set")),
        }
    }

    pub fn as_bytes(&self) -> DomainResult<Vec<u8>> {
        match self {
            TreewalkValue::Bytes(i) => Ok(i.clone()),
            _ => Err(ExecutionError::type_error("Expected bytes")),
        }
    }

    pub fn negated(&self) -> Option<Self> {
        match self {
            TreewalkValue::Float(i) => Some(TreewalkValue::Float(-i)),
            TreewalkValue::Int(i) => Some(TreewalkValue::Int(-i)),
            _ => None,
        }
    }

    pub fn not(&self) -> Self {
        TreewalkValue::Bool(!self.coerce_to_boolean())
    }
}

impl Voidable for TreewalkValue {
    fn is_none(&self) -> bool {
        matches!(self, TreewalkValue::None)
    }
}

impl Display for TreewalkValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.minimized_display(f)
    }
}

impl Debug for TreewalkValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.minimized_display(f)
    }
}

impl From<TreewalkValue> for MemphisValue {
    fn from(value: TreewalkValue) -> Self {
        match value {
            TreewalkValue::None => MemphisValue::None,
            TreewalkValue::Int(i) => MemphisValue::Integer(i),
            TreewalkValue::Float(i) => MemphisValue::Float(i),
            TreewalkValue::Str(_) => {
                MemphisValue::String(value.as_str().expect("failed to get string"))
            }
            TreewalkValue::Bool(val) => MemphisValue::Boolean(val),
            TreewalkValue::List(i) => {
                let items = i
                    .into_iter()
                    .map(|item| item.into())
                    .collect::<Vec<MemphisValue>>();
                MemphisValue::List(items)
            }
            TreewalkValue::Ellipsis => MemphisValue::Unimplemented("ellipsis"),
            TreewalkValue::NotImplemented => MemphisValue::Unimplemented("not_implemented"),
            TreewalkValue::Class(_) => MemphisValue::Unimplemented("class"),
            TreewalkValue::Object(_) => MemphisValue::Unimplemented("object"),
            TreewalkValue::Module(_) => MemphisValue::Unimplemented("module"),
            TreewalkValue::Super(_) => MemphisValue::Unimplemented("super"),
            TreewalkValue::Classmethod(_) => MemphisValue::Unimplemented("classmethod"),
            TreewalkValue::Staticmethod(_) => MemphisValue::Unimplemented("staticmethod"),
            TreewalkValue::Property(_) => MemphisValue::Unimplemented("property"),
            TreewalkValue::DataDescriptor(_) => MemphisValue::Unimplemented("data_descriptor"),
            TreewalkValue::NonDataDescriptor(_) => {
                MemphisValue::Unimplemented("non_data_descriptor")
            }
            TreewalkValue::Function(_) => MemphisValue::Unimplemented("function"),
            TreewalkValue::Method(_) => MemphisValue::Unimplemented("method"),
            TreewalkValue::BuiltinFunction(_) => MemphisValue::Unimplemented("builtin_func"),
            TreewalkValue::BuiltinMethod(_) => MemphisValue::Unimplemented("builtin_method"),
            TreewalkValue::Generator(_) => MemphisValue::Unimplemented("generator"),
            TreewalkValue::Coroutine(_) => MemphisValue::Unimplemented("coroutine"),
            TreewalkValue::Code(_) => MemphisValue::Unimplemented("code"),
            TreewalkValue::Cell(_) => MemphisValue::Unimplemented("cell"),
            TreewalkValue::Bytes(_) => MemphisValue::Unimplemented("bytes"),
            TreewalkValue::ByteArray(_) => MemphisValue::Unimplemented("byte_array"),
            TreewalkValue::Set(_) => MemphisValue::Unimplemented("set"),
            TreewalkValue::FrozenSet(_) => MemphisValue::Unimplemented("frozenset"),
            TreewalkValue::Zip(_) => MemphisValue::Unimplemented("zip"),
            TreewalkValue::Slice(_) => MemphisValue::Unimplemented("slice"),
            TreewalkValue::Complex(_) => MemphisValue::Unimplemented("complex"),
            TreewalkValue::Dict(_) => MemphisValue::Unimplemented("dict"),
            TreewalkValue::DictItems(_) => MemphisValue::Unimplemented("dict_items"),
            TreewalkValue::DictKeys(_) => MemphisValue::Unimplemented("dict_keys"),
            TreewalkValue::DictValues(_) => MemphisValue::Unimplemented("dict_values"),
            TreewalkValue::MappingProxy(_) => MemphisValue::Unimplemented("mappingproxy"),
            TreewalkValue::Range(_) => MemphisValue::Unimplemented("range"),
            TreewalkValue::Tuple(_) => MemphisValue::Unimplemented("tuple"),
            TreewalkValue::Exception(_) => MemphisValue::Unimplemented("exception"),
            TreewalkValue::StopIteration(_) => MemphisValue::Unimplemented("stop_iteration"),
            TreewalkValue::Traceback(_) => MemphisValue::Unimplemented("traceback"),
            TreewalkValue::Frame => MemphisValue::Unimplemented("frame"),
            TreewalkValue::ListIter(_) => MemphisValue::Unimplemented("list_iter"),
            TreewalkValue::ReversedIter(_) => MemphisValue::Unimplemented("reversed_iter"),
            TreewalkValue::SetIter(_) => MemphisValue::Unimplemented("set_iter"),
            TreewalkValue::DictItemsIter(_) => MemphisValue::Unimplemented("dict_items_iter"),
            TreewalkValue::DictKeysIter(_) => MemphisValue::Unimplemented("dict_keys_iter"),
            TreewalkValue::DictValuesIter(_) => MemphisValue::Unimplemented("dict_values_iter"),
            TreewalkValue::RangeIter(_) => MemphisValue::Unimplemented("range_iter"),
            TreewalkValue::TupleIter(_) => MemphisValue::Unimplemented("tuple_iter"),
            TreewalkValue::StrIter(_) => MemphisValue::Unimplemented("str_iter"),
            TreewalkValue::BytesIter(_) => MemphisValue::Unimplemented("bytes_iter"),
            TreewalkValue::ByteArrayIter(_) => MemphisValue::Unimplemented("bytes_array_iter"),
            TreewalkValue::TypeNode(_) => MemphisValue::Unimplemented("type_node"),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(_) => MemphisValue::Unimplemented("cpython_module"),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonObject(_) => MemphisValue::Unimplemented("cpython_object"),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonClass(_) => MemphisValue::Unimplemented("cpython_class"),
        }
    }
}
