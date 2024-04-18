use std::fmt::{Display, Error, Formatter};

use crate::parser::types::TypeNode;

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

static TYPE_MAPPINGS: &[(Type, &str)] = &[
    (Type::Type, "type"),
    (Type::TypeMeta, "type"),
    (Type::Object, "object"),
    (Type::ObjectMeta, "object"),
    (Type::Super, "super"),
    (Type::GetSetDescriptor, "getset_descriptor"),
    (Type::MemberDescriptor, "member_descriptor"),
    (Type::Method, "method"),
    (Type::Function, "function"),
    (Type::BuiltinFunction, "builtin_function_or_method"),
    (Type::BuiltinMethod, "builtin_function_or_method"),
    (Type::Generator, "generator"),
    (Type::Coroutine, "coroutine"),
    (Type::Ellipsis, "ellipsis"),
    (Type::None, "NoneType"),
    (Type::NotImplemented, "NotImplementedType"),
    (Type::Bool, "bool"),
    (Type::Int, "int"),
    (Type::Float, "float"),
    (Type::Str, "str"),
    (Type::List, "list"),
    (Type::Set, "set"),
    (Type::FrozenSet, "frozenset"),
    (Type::Zip, "zip"),
    (Type::Tuple, "tuple"),
    (Type::Range, "range"),
    (Type::Slice, "slice"),
    (Type::Complex, "complex"),
    (Type::Bytes, "bytes"),
    (Type::ByteArray, "bytearray"),
    (Type::Memoryview, "memoryview"),
    (Type::Dict, "dict"),
    (Type::DictItems, "dict_items"),
    (Type::DictKeys, "dict_keys"),
    (Type::DictValues, "dict_values"),
    (Type::MappingProxy, "mappingproxy"),
    (Type::StringIterator, "string_iterator"),
    (Type::ListIterator, "list_iterator"),
    // The builtin keyword here is different than the type
    // string: "list_reverseiterator"
    (Type::ReversedIterator, "reversed"),
    (Type::SetIterator, "set_iterator"),
    (Type::TupleIterator, "tuple_iterator"),
    (Type::DictItemIterator, "dict_itemiterator"),
    (Type::DictKeyIterator, "dict_keyiterator"),
    (Type::DictValueIterator, "dict_valueiterator"),
    (Type::BytesIterator, "bytes_iterator"),
    (Type::ByteArrayIterator, "byte_array_iterator"),
    (Type::RangeIterator, "range_iterator"),
    (Type::Exception, "Exception"),
    (Type::Traceback, "traceback"),
    (Type::Frame, "frame"),
    (Type::Module, "module"),
    (Type::Cell, "cell"),
    (Type::Code, "code"),
    (Type::Classmethod, "classmethod"),
    (Type::Staticmethod, "staticmethod"),
    (Type::Property, "property"),
];

impl Type {
    pub fn value(&self) -> &'static str {
        TYPE_MAPPINGS
            .iter()
            .find_map(
                |(variant, name)| {
                    if variant == self {
                        Some(name)
                    } else {
                        None
                    }
                },
            )
            .expect("Invalid Type variant")
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.value())
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Generic {
        base: Box<TypeExpr>,
        parameters: Vec<TypeExpr>,
    },
    Union(Vec<TypeExpr>),
    Ellipsis,
    Int,
    List,
    Str,
}

/// Convert from `TypeNode`, which is used by the parser, to `TypeExpr`, which is used by the
/// interpreter.
fn convert_to_type_expr(type_node: &TypeNode) -> TypeExpr {
    match type_node {
        TypeNode::Generic {
            base_type,
            parameters,
        } => {
            let base_expr = match base_type.as_str() {
                "list" => TypeExpr::List,
                _ => unimplemented!(),
            };

            let param_exprs = parameters.iter().map(convert_to_type_expr).collect();

            TypeExpr::Generic {
                base: Box::new(base_expr),
                parameters: param_exprs,
            }
        }
        TypeNode::Union(parameters) => {
            let param_exprs = parameters.iter().map(convert_to_type_expr).collect();

            TypeExpr::Union(param_exprs)
        }
        TypeNode::Basic(type_str) => match type_str.as_str() {
            "int" => TypeExpr::Int,
            "str" => TypeExpr::Str,
            "..." => TypeExpr::Ellipsis,
            _ => unimplemented!(),
        },
    }
}

impl From<&TypeNode> for TypeExpr {
    fn from(type_node: &TypeNode) -> TypeExpr {
        convert_to_type_expr(type_node)
    }
}
