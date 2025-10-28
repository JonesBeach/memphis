use std::fmt::{Display, Error, Formatter};

/// Represents the logical Python type of a value, as shown in stack traces or `type()` calls.
/// This is not tied to any specific runtime engine.
#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub enum Type {
    #[allow(clippy::enum_variant_names)]
    Type,
    // This is a hack to avoid a circular reference from `Type` to itself.
    // We do the same thing with `ObjectMeta`.
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
    StrIter,
    ListIter,
    ReversedIter,
    SetIter,
    TupleIter,
    DictItemIter,
    DictKeyIter,
    DictValueIter,
    BytesIter,
    ByteArrayIter,
    RangeIter,
    Exception,
    StopIteration,
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
            Type::StrIter => "string_iterator",
            Type::ListIter => "list_iterator",
            // The builtin keyword here is different than the type
            // string: "list_reverseiterator"
            Type::ReversedIter => "reversed",
            Type::SetIter => "set_iterator",
            Type::TupleIter => "tuple_iterator",
            Type::DictItemIter => "dict_itemiterator",
            Type::DictKeyIter => "dict_keyiterator",
            Type::DictValueIter => "dict_valueiterator",
            Type::BytesIter => "bytes_iterator",
            Type::ByteArrayIter => "byte_array_iterator",
            Type::RangeIter => "range_iterator",
            Type::Exception => "Exception",
            Type::StopIteration => "StopIteration",
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
        f.write_str(self.value())
    }
}

impl From<&Type> for String {
    fn from(value: &Type) -> Self {
        value.value().to_string()
    }
}

impl From<&Type> for &str {
    fn from(value: &Type) -> Self {
        value.value()
    }
}
