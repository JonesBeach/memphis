use std::fmt::{Display, Error, Formatter};

/// These are all of the builtin types. For the class 'type', see `TypeClass`.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type {
    #[allow(clippy::enum_variant_names)]
    Type,
    // This is a hack to avoid a circular referece from Type to itself. We do the same thing with
    // `ObjectMeta`.
    #[allow(clippy::enum_variant_names)]
    TypeMeta,
    ObjectMeta,
    Object,
    Super,
    GetSetDescriptor,
    MemberDescriptor,
    Method,
    Function,
    BuiltinFunction,
    BuiltinMethod,
    Generator,
    Coroutine,
    Ellipsis,
    None,
    NotImplemented,
    Bool,
    Int,
    Float,
    Str,
    List,
    Set,
    FrozenSet,
    Zip,
    Tuple,
    Range,
    Slice,
    Complex,
    Bytes,
    ByteArray,
    Memoryview,
    Dict,
    DictItems,
    DictKeys,
    DictValues,
    MappingProxy,
    StringIterator,
    ListIterator,
    ReversedIterator,
    SetIterator,
    TupleIterator,
    DictItemIterator,
    DictKeyIterator,
    DictValueIterator,
    BytesIterator,
    ByteArrayIterator,
    RangeIterator,
    Exception,
    Traceback,
    Frame,
    Module,
    Cell,
    Code,
    Classmethod,
    Staticmethod,
    Property,
}

impl Type {
    fn value(&self) -> &'static str {
        match self {
            Type::Type => "type",
            Type::TypeMeta => "type",
            Type::Object => "object",
            Type::ObjectMeta => "object",
            Type::Super => "super",
            Type::GetSetDescriptor => "getset_descriptor",
            Type::MemberDescriptor => "member_descriptor",
            Type::Method => "method",
            Type::Function => "function",
            Type::BuiltinFunction => "builtin_function_or_method",
            Type::BuiltinMethod => "builtin_function_or_method",
            Type::Generator => "generator",
            Type::Coroutine => "coroutine",
            Type::Ellipsis => "ellipsis",
            Type::None => "NoneType",
            Type::NotImplemented => "NotImplementedType",
            Type::Bool => "bool",
            Type::Int => "int",
            Type::Float => "float",
            Type::Str => "str",
            Type::List => "list",
            Type::Set => "set",
            Type::FrozenSet => "frozenset",
            Type::Zip => "zip",
            Type::Tuple => "tuple",
            Type::Range => "range",
            Type::Slice => "slice",
            Type::Complex => "complex",
            Type::Bytes => "bytes",
            Type::ByteArray => "bytearray",
            Type::Memoryview => "memoryview",
            Type::Dict => "dict",
            Type::DictItems => "dict_items",
            Type::DictKeys => "dict_keys",
            Type::DictValues => "dict_values",
            Type::MappingProxy => "mappingproxy",
            Type::StringIterator => "string_iterator",
            Type::ListIterator => "list_iterator",
            // The builtin keyword here is different than the type
            // string: "list_reverseiterator"
            Type::ReversedIterator => "reversed",
            Type::SetIterator => "set_iterator",
            Type::TupleIterator => "tuple_iterator",
            Type::DictItemIterator => "dict_itemiterator",
            Type::DictKeyIterator => "dict_keyiterator",
            Type::DictValueIterator => "dict_valueiterator",
            Type::BytesIterator => "bytes_iterator",
            Type::ByteArrayIterator => "byte_array_iterator",
            Type::RangeIterator => "range_iterator",
            Type::Exception => "Exception",
            Type::Traceback => "traceback",
            Type::Frame => "frame",
            Type::Module => "module",
            Type::Cell => "cell",
            Type::Code => "code",
            Type::Classmethod => "classmethod",
            Type::Staticmethod => "staticmethod",
            Type::Property => "property",
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.value())
    }
}

impl From<Type> for &str {
    fn from(value: Type) -> Self {
        value.value()
    }
}

impl From<&Type> for &str {
    fn from(value: &Type) -> Self {
        value.value()
    }
}
