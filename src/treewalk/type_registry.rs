use std::collections::HashMap;

use crate::{
    core::Container,
    domain::Type,
    treewalk::{
        type_system::{
            CloneableCallable, CloneableNonDataDescriptor, DataDescriptorProvider,
            DescriptorProvider, MethodProvider,
        },
        types::{
            iterators::{ReversedIterator, ZipIterator},
            Bool, ByteArray, Bytes, Class, Classmethod, Complex, Coroutine, Dict, Exception,
            FrozenSet, Function, Int, List, Memoryview, Object, Property, Range, Set, Slice,
            Staticmethod, Str, Super, Traceback, Tuple, TypeClass,
        },
        TreewalkValue,
    },
};

fn register_methods<T: MethodProvider>(
    methods: &mut HashMap<Type, Vec<Box<dyn CloneableCallable>>>,
) {
    methods.insert(T::get_type(), T::get_methods());
}

fn register_descriptors<T: DescriptorProvider>(
    methods: &mut HashMap<Type, Vec<Box<dyn CloneableNonDataDescriptor>>>,
) {
    methods.insert(T::get_type(), T::get_descriptors());
}

/// Register methods for each type that implements `MethodProvider`. [`Type::Type`] and
/// [`Type::Object`] are excluded here because they are initialized separately.
fn builtin_methods() -> HashMap<Type, Vec<Box<dyn CloneableCallable>>> {
    let mut methods = HashMap::new();

    register_methods::<Dict>(&mut methods);
    register_methods::<Super>(&mut methods);
    register_methods::<Bool>(&mut methods);
    register_methods::<Int>(&mut methods);
    register_methods::<Str>(&mut methods);
    register_methods::<List>(&mut methods);
    register_methods::<Set>(&mut methods);
    register_methods::<FrozenSet>(&mut methods);
    register_methods::<Tuple>(&mut methods);
    register_methods::<Dict>(&mut methods);
    register_methods::<Range>(&mut methods);
    register_methods::<Slice>(&mut methods);
    register_methods::<ZipIterator>(&mut methods);
    register_methods::<ReversedIterator>(&mut methods);
    register_methods::<Bytes>(&mut methods);
    register_methods::<Complex>(&mut methods);
    register_methods::<ByteArray>(&mut methods);
    register_methods::<Memoryview>(&mut methods);
    register_methods::<Coroutine>(&mut methods);
    register_methods::<Classmethod>(&mut methods);
    register_methods::<Staticmethod>(&mut methods);
    register_methods::<Property>(&mut methods);

    methods
}

/// Register attributes that implement `DescriptorProvider`. [`Type::Type`] and
/// [`Type::Object`] are excluded here because they are initialized separately.
fn descriptors() -> HashMap<Type, Vec<Box<dyn CloneableNonDataDescriptor>>> {
    let mut methods = HashMap::new();

    register_descriptors::<Function>(&mut methods);
    register_descriptors::<Exception>(&mut methods);
    register_descriptors::<Traceback>(&mut methods);

    methods
}

/// A list of all the variants of [`Type`] which should have a type class created. As of 2024-02-16,
/// this is all the variants.
///
/// We leave [`Type::Type`] out of here beacuse it must be initialized first as it is the metaclass
/// for all the these type classes.
///
/// We also leave [`Type::Object`] out of here because it must be initialized first as it is the
/// parent class for all of these type classes.
static ALL_TYPES: [Type; 51] = [
    Type::Super,
    Type::GetSetDescriptor,
    Type::MemberDescriptor,
    Type::Method,
    Type::Function,
    Type::BuiltinFunction,
    Type::BuiltinMethod,
    Type::Generator,
    Type::Coroutine,
    Type::None,
    Type::Ellipsis,
    Type::NotImplemented,
    Type::Bool,
    Type::Int,
    Type::Str,
    Type::List,
    Type::Set,
    Type::FrozenSet,
    Type::Zip,
    Type::Tuple,
    Type::Range,
    Type::Slice,
    Type::Complex,
    Type::Bytes,
    Type::ByteArray,
    Type::Memoryview,
    Type::Dict,
    Type::DictItems,
    Type::DictKeys,
    Type::DictValues,
    Type::MappingProxy,
    Type::DictItemIterator,
    Type::DictKeyIterator,
    Type::DictValueIterator,
    Type::BytesIterator,
    Type::ByteArrayIterator,
    Type::RangeIterator,
    Type::StringIterator,
    Type::ListIterator,
    Type::ReversedIterator,
    Type::SetIterator,
    Type::TupleIterator,
    Type::Exception,
    Type::Traceback,
    Type::Frame,
    Type::Module,
    Type::Cell,
    Type::Code,
    Type::Classmethod,
    Type::Staticmethod,
    Type::Property,
];

/// These types are callable and behave like a builtin function.
static CALLABLE_TYPES: [Type; 23] = [
    Type::Type,
    Type::Object,
    Type::Super,
    Type::Bool,
    Type::Int,
    Type::Str,
    Type::List,
    Type::Dict,
    Type::Set,
    Type::FrozenSet,
    Type::Tuple,
    Type::Range,
    Type::Slice,
    Type::Complex,
    //Type::Float,
    Type::Bytes,
    Type::ByteArray,
    Type::Memoryview,
    Type::Zip, // this refers to the iterator itself
    Type::ReversedIterator,
    Type::Classmethod,
    Type::Staticmethod,
    Type::Property,
    // ----------------------------------------------------------------------------------------
    // Technically not a builtin, but it is callable. We may need to handle builtin class such
    // as these separately.
    Type::Exception,
];

/// Create the [`Type::Type`] class which is the metaclass to all classes, including itself.
///
/// For the hierarchy to work, we give it a parent class of [`Type::ObjectMeta`], which contains
/// all the builtin methods of [`Type::Object`], and a metaclass of [`Type::TypeMeta`], which
/// contains all the builtin methods of [`Type::Type`]. The "meta" types should never be used
/// directly, but a cycle is created if we try to make Type inherit from Object while Object's
/// metaclass is Type.
fn type_class() -> Container<Class> {
    let object_base = Class::new_builtin(Type::ObjectMeta, None, vec![]);
    for method in Object::get_methods().into_iter() {
        object_base.set_on_class(&method.name(), TreewalkValue::BuiltinMethod(method));
    }

    let type_base = Class::new_builtin(Type::TypeMeta, None, vec![]);
    for method in TypeClass::get_methods().into_iter() {
        type_base.set_on_class(&method.name(), TreewalkValue::BuiltinMethod(method));
    }

    for attr in TypeClass::get_descriptors().into_iter() {
        type_base.set_on_class(&attr.name(), TreewalkValue::NonDataDescriptor(attr));
    }

    let type_class = Class::new_builtin(Type::Type, Some(type_base), vec![object_base]);
    for method in TypeClass::get_methods().into_iter() {
        type_class.set_on_class(&method.name(), TreewalkValue::BuiltinMethod(method));
    }

    for attr in TypeClass::get_descriptors().into_iter() {
        type_class.set_on_class(&attr.name(), TreewalkValue::NonDataDescriptor(attr));
    }

    type_class
}

/// Create the [`Type::Object`] class which is the parent class to all classes, including
/// [`Type::Type`], except itself.
fn object_class(metaclass: Container<Class>) -> Container<Class> {
    let object_class = Class::new_builtin(Type::Object, Some(metaclass), vec![]);
    for method in Object::get_methods().into_iter() {
        object_class.set_on_class(&method.name(), TreewalkValue::BuiltinMethod(method));
    }

    for attr in Object::get_data_descriptors().into_iter() {
        object_class.set_on_class(&attr.name(), TreewalkValue::DataDescriptor(attr));
    }

    object_class
}

fn init_type_classes() -> HashMap<Type, Container<Class>> {
    let mut type_classes = HashMap::new();

    // Create the `Type::Type` class and use it as the metaclass for all the other type classes.
    let type_class = type_class();
    type_classes.insert(Type::Type, type_class.clone());

    // Create the `Type::Object` and use it as the parent class for `Type::Type` and all other type
    // classes.
    let object_class = object_class(type_class.clone());
    type_classes.insert(Type::Object, object_class.clone());

    // TODO in theory, the parent of `Type::Type` should be `Type::Object`. The code is hanging
    // with this line presumably due to a cycle. Maybe there's a way to break this since this is a
    // known and expected case.
    //type_class.borrow_mut().parent_class = Some(object_class.clone());

    // Create all the other type classes using `Type::Type` and `Type::Object`.
    let mut methods = builtin_methods();
    let mut attributes = descriptors();
    for type_ in ALL_TYPES.iter() {
        let class = Class::new_builtin(
            type_.clone(),
            Some(type_class.clone()),
            vec![object_class.clone()],
        );
        let builtin_type = class.borrow().builtin_type().clone();

        // Add the builtin methods for this type class.
        //
        // Calling `.remove` here allows us to transfer ownership of the methods to the class,
        // which is what we want since this is just initialization code.
        if let Some(methods_for_type) = methods.remove(&builtin_type) {
            for method in methods_for_type.into_iter().by_ref() {
                class.set_on_class(&method.name(), TreewalkValue::BuiltinMethod(method));
            }
        }

        // Add the dynamic attributes for this type class.
        if let Some(attributes_for_type) = attributes.remove(&builtin_type) {
            for attr in attributes_for_type.into_iter().by_ref() {
                class.set_on_class(&attr.name(), TreewalkValue::NonDataDescriptor(attr));
            }
        }

        type_classes.insert(builtin_type, class);
    }

    type_classes
}

/// This struct holds a singleton [`Class`] for each variant of [`Type`] supported in Python. The
/// [`Class`] will contain any builtin methods which are supported.
pub struct TypeRegistry {
    type_classes: HashMap<Type, Container<Class>>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self {
            type_classes: init_type_classes(),
        }
    }

    /// Safe to call `unwrap()` here because we will have a type class for all `Type`s.
    /// TODO we still need to enforce this at compile-time ideally.
    pub fn get_type_class(&self, type_: Type) -> Container<Class> {
        self.type_classes
            .get(&type_)
            .unwrap_or_else(|| panic!("TypeRegistry initialization failed for <{}>!", type_))
            .clone()
    }

    /// We need a way to expose the builtin types so they can be stored in the builtin scope inside
    /// the `ScopeManager`.
    pub fn get_callable_builtin_types(&self) -> Vec<Container<Class>> {
        CALLABLE_TYPES
            .iter()
            .map(|callable_type| self.get_type_class(callable_type.clone()))
            .collect()
    }
}
