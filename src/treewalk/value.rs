use std::{
    cmp::Ordering,
    fmt::{Debug, Display, Error, Formatter},
    hash::{Hash, Hasher},
    ptr,
};

#[cfg(feature = "c_stdlib")]
use crate::treewalk::types::cpython::{CPythonClass, CPythonModule, CPythonObject};
use crate::{
    core::{Container, Voidable},
    domain::{Dunder, ExecutionError, MemphisValue, Type},
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
            Property, Range, Set, Slice, Staticmethod, Str, Super, Traceback, Tuple,
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
    Exception(ExecutionError),
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
            (TreewalkValue::Float(a), TreewalkValue::Float(b)) => (a - b).abs() < 1e-9,
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
// For some reason, we have to create this here for the Eq trait to be
// satisfied for f64.
impl Eq for TreewalkValue {}

impl Hash for TreewalkValue {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        if let TreewalkValue::Set(set) = self {
            for i in set.borrow().iter() {
                i.as_integer().unwrap().hash(state)
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
            TreewalkValue::Class(c) => write!(f, "{}", c),
            TreewalkValue::Object(o) => write!(f, "{}", o),
            TreewalkValue::Method(m) => write!(f, "{}", m),
            TreewalkValue::Function(func) => write!(f, "{}", func),
            TreewalkValue::Generator(_) => write!(f, "<generator object>"),
            TreewalkValue::Coroutine(_) => write!(f, "<coroutine object>"),
            TreewalkValue::BuiltinFunction(func) => {
                write!(f, "<built-in function {}>", func.name())
            }
            TreewalkValue::BuiltinMethod(_) => write!(f, "<built-in method>"),
            TreewalkValue::Int(i) => write!(f, "{}", i),
            TreewalkValue::Float(i) => write!(f, "{}", i),
            TreewalkValue::Str(s) => write!(f, "{}", s),
            TreewalkValue::Bytes(b) => write!(f, "b'{:?}'", b),
            TreewalkValue::ByteArray(b) => write!(f, "bytearray(b'{:?}')", b),
            TreewalkValue::Bool(b) => match b {
                true => write!(f, "True"),
                false => write!(f, "False"),
            },
            TreewalkValue::List(l) => write!(f, "{}", l),
            TreewalkValue::Set(s) => write!(f, "{}", s),
            TreewalkValue::FrozenSet(s) => write!(f, "{}", s),
            TreewalkValue::Range(r) => write!(f, "{}", r),
            TreewalkValue::Tuple(t) => write!(f, "{}", t),
            TreewalkValue::Zip(_) => write!(f, "<zip>"),
            TreewalkValue::Slice(s) => write!(f, "{}", s),
            TreewalkValue::Complex(c) => write!(f, "{}", c),
            TreewalkValue::Dict(d) => write!(f, "{}", d),
            TreewalkValue::MappingProxy(d) => write!(f, "{}", d),
            TreewalkValue::DictItems(d) => write!(f, "dict_items({})", d),
            TreewalkValue::DictKeys(d) => write!(f, "dict_keys({})", d),
            TreewalkValue::DictValues(d) => write!(f, "dict_values({})", d),
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
            TreewalkValue::Module(m) => write!(f, "{}", m),
            TreewalkValue::Exception(_) => write!(f, "<exception>"),
            TreewalkValue::Traceback(_) => write!(f, "<traceback>"),
            TreewalkValue::Frame => write!(f, "<frame>"),
            TreewalkValue::TypeNode(t) => write!(f, "<type {:?}>", t),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(m) => write!(f, "{}", m),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonObject(o) => write!(f, "{}", o),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonClass(_) => write!(f, "<class>"),
        }
    }

    /// Take ownership and convert to a user-facing value. These are the Python objects which are
    /// returned from `iter()` and on which you can call `next()`.
    pub fn into_iterator_value(self) -> Option<TreewalkValue> {
        let value = match self {
            TreewalkValue::List(list) => TreewalkValue::ListIter(list.into_iter()),
            TreewalkValue::ListIter(_) => self,
            TreewalkValue::Str(s) => TreewalkValue::StrIter(s.into_iter()),
            TreewalkValue::StrIter(_) => self,
            TreewalkValue::Set(set) => TreewalkValue::SetIter(set.into_iter()),
            TreewalkValue::SetIter(_) => self,
            TreewalkValue::Tuple(tuple) => TreewalkValue::TupleIter(tuple.into_iter()),
            TreewalkValue::TupleIter(_) => self,
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
            _ => return None,
        };

        Some(value)
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
            _ => interpreter.state.class_of_type(self.get_type()).clone(),
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
        if let Some(callable) = self.clone().into_callable() {
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

    pub fn expect_callable(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Box<dyn CloneableCallable>> {
        self.clone()
            .into_callable()
            .ok_or_else(|| interpreter.type_error("Expected a callable"))
    }

    pub fn expect_iterable(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Box<dyn CloneableIterable>> {
        self.clone()
            .into_iterable()
            .ok_or_else(|| interpreter.type_error("Expected an iterable"))
    }

    pub fn as_integer(&self) -> Option<i64> {
        match self {
            TreewalkValue::Int(i) => Some(*i),
            TreewalkValue::Str(s) => s.parse::<i64>().ok(),
            _ => None,
        }
    }

    pub fn expect_integer(&self, interpreter: &TreewalkInterpreter) -> TreewalkResult<i64> {
        self.as_integer()
            .ok_or_else(|| interpreter.type_error("Expected an integer"))
    }

    pub fn as_fp(&self) -> Option<f64> {
        match self {
            TreewalkValue::Float(i) => Some(*i),
            TreewalkValue::Int(i) => Some(*i as f64),
            _ => None,
        }
    }

    pub fn expect_fp(&self, interpreter: &TreewalkInterpreter) -> TreewalkResult<f64> {
        self.as_fp()
            .ok_or_else(|| interpreter.type_error("Expected a floating point"))
    }

    pub fn as_class(&self) -> Option<Container<Class>> {
        match self {
            TreewalkValue::Class(i) => Some(i.clone()),
            // TODO should this use a trait interface?
            // #[cfg(feature = "c_stdlib")]
            // TreewalkValue::CPythonClass(i) => Some(i),
            _ => None,
        }
    }

    pub fn expect_class(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Container<Class>> {
        self.as_class()
            .ok_or_else(|| interpreter.type_error("Expected a class"))
    }

    pub fn as_module(&self) -> Option<Box<dyn MemberRead>> {
        match self {
            TreewalkValue::Module(i) => Some(Box::new(i.borrow().clone())),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(i) => Some(Box::new(i.borrow().clone())),
            _ => None,
        }
    }

    pub fn expect_module(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Box<dyn MemberRead>> {
        self.as_module()
            .ok_or_else(|| interpreter.type_error("Expected a module"))
    }

    pub fn as_function(&self) -> Option<Container<Function>> {
        match self {
            TreewalkValue::Function(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn expect_function(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Container<Function>> {
        self.as_function()
            .ok_or_else(|| interpreter.type_error("Expected a function"))
    }

    pub fn as_generator(&self) -> Option<GeneratorIter> {
        match self {
            TreewalkValue::Generator(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn as_coroutine(&self) -> Option<Container<Coroutine>> {
        match self {
            TreewalkValue::Coroutine(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn expect_coroutine(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Container<Coroutine>> {
        self.as_coroutine()
            .ok_or_else(|| interpreter.type_error("Expected a coroutine"))
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            TreewalkValue::Bool(i) => *i,
            TreewalkValue::List(i) => i.borrow().len() > 0,
            TreewalkValue::Str(i) => !i.is_empty(),
            TreewalkValue::Int(i) => *i != 0,
            TreewalkValue::None => false,
            _ => true,
        }
    }

    pub fn as_object(&self) -> Option<Container<Object>> {
        match self {
            TreewalkValue::Object(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn expect_object(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Container<Object>> {
        self.as_object()
            .ok_or_else(|| interpreter.type_error("Expected an object"))
    }

    /// Returns a `Container<List>` with _no_ type coercion. Use `TryFrom<TreewalkValue>` for type
    /// coercion.
    pub fn as_list(&self) -> Option<Container<List>> {
        match self {
            TreewalkValue::List(list) => Some(list.clone()),
            _ => None,
        }
    }

    pub fn expect_list(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Container<List>> {
        self.as_list()
            .ok_or_else(|| interpreter.type_error("Expected a list"))
    }

    /// Returns a `Container<Set>` with _no_ type coercion. Use `TryFrom<TreewalkValue>` for type
    /// coercion.
    pub fn as_set(&self) -> Option<Container<Set>> {
        match self {
            TreewalkValue::Set(set) => Some(set.clone()),
            _ => None,
        }
    }

    pub fn expect_set(&self, interpreter: &TreewalkInterpreter) -> TreewalkResult<Container<Set>> {
        self.as_set()
            .ok_or_else(|| interpreter.type_error("Expected a set"))
    }

    pub fn as_dict(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Option<Container<Dict>>> {
        let result = match self {
            TreewalkValue::Dict(i) => Some(i.clone()),
            TreewalkValue::List(list) => {
                let dict_items = DictItems::from_vec(list.borrow().cloned_items(), interpreter)?;
                Some(Container::new(dict_items.to_dict()))
            }
            _ => None,
        };

        Ok(result)
    }

    pub fn expect_dict(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Container<Dict>> {
        self.as_dict(interpreter)?
            .ok_or_else(|| interpreter.type_error("Expected a dict"))
    }

    pub fn as_symbol_table(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Option<SymbolTable>> {
        let table = match self {
            TreewalkValue::Dict(dict) => Some(dict.borrow().to_symbol_table(interpreter)?),
            _ => None,
        };

        Ok(table)
    }

    pub fn expect_symbol_table(
        &self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<SymbolTable> {
        self.as_symbol_table(interpreter)?
            .ok_or_else(|| interpreter.type_error("Expected a symbol-table-like object"))
    }

    pub fn expect_tuple(&self, interpreter: &TreewalkInterpreter) -> TreewalkResult<Tuple> {
        self.clone()
            .try_into()
            .map_err(|_| interpreter.type_error("Expected a tuple"))
    }

    pub fn as_string(&self) -> Option<String> {
        match self {
            TreewalkValue::Str(i) => Some(i.to_string()),
            TreewalkValue::Int(i) => Some(i.to_string()),
            _ => None,
        }
    }

    pub fn expect_string(&self, interpreter: &TreewalkInterpreter) -> TreewalkResult<String> {
        self.as_string()
            .ok_or_else(|| interpreter.type_error("Expected a string"))
    }

    pub fn negated(&self) -> Self {
        match self {
            TreewalkValue::Float(i) => TreewalkValue::Float(-i),
            TreewalkValue::Int(i) => TreewalkValue::Int(-i),
            _ => unreachable!(),
        }
    }

    pub fn inverted(&self) -> Self {
        match self {
            TreewalkValue::Bool(i) => TreewalkValue::Bool(!i),
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
                MemphisValue::String(value.as_string().expect("failed to get string"))
            }
            TreewalkValue::Bool(val) => MemphisValue::Boolean(val),
            TreewalkValue::List(i) => {
                let items = i
                    .into_iter()
                    .map(|item| item.into())
                    .collect::<Vec<MemphisValue>>();
                MemphisValue::List(items)
            }
            TreewalkValue::Coroutine(_) => MemphisValue::Unimplemented("coroutine"),
            _ => unimplemented!("Conversion not implemented for type '{}'", value.get_type()),
        }
    }
}
