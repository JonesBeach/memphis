use std::{
    any::Any,
    fmt::{Display, Error, Formatter},
};

use crate::{
    core::{Container, LogLevel, log},
    domain::{Dunder, Type},
    treewalk::{
        Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue, macros::*, protocols::{
            Callable, DataDescriptor, IndexRead, IndexWrite, MemberRead, MemberWrite,
            NonDataDescriptor,
        }, result::Raise, types::{Class, Exception, Str}, utils::{Args, args, check_args}
    },
};

#[derive(Debug)]
pub struct Object {
    class: Container<Class>,
    scope: Scope,
    native_payload: Option<Box<dyn Any>>,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        // If these are both native objects, compare pointer identity
        if let (Some(a), Some(b)) = (&self.native_payload, &other.native_payload) {
            if (**a).type_id() == (**b).type_id() {
                return std::ptr::eq(a.as_ref(), b.as_ref());
            }
        }

        self.class == other.class && self.scope == other.scope
    }
}

impl_typed!(Object, Type::Object);
impl_method_provider!(
    Object,
    [
        NewBuiltin,
        InitBuiltin,
        EqBuiltin,
        NeBuiltin,
        AddBuiltin,
        SubBuiltin,
        MulBuiltin,
        TruedivBuiltin,
        LtBuiltin,
        LeBuiltin,
        GtBuiltin,
        GeBuiltin,
        ContainsBuiltin,
        HashBuiltin,
        StrBuiltin
    ]
);
impl_data_descriptor_provider!(Object, [DictDescriptor]);

impl Object {
    /// Create the object with an empty symbol table. This is also called by the [`Dunder::New`]
    /// for [`Type::Object`] builtin.
    pub fn new(class: Container<Class>) -> Object {
        Self {
            class,
            scope: Scope::default(),
            native_payload: None,
        }
    }

    pub fn with_payload<T: Any>(class: Container<Class>, payload: T) -> Object {
        Self {
            class,
            scope: Scope::default(),
            native_payload: Some(Box::new(payload)),
        }
    }

    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.native_payload.as_ref()?.downcast_ref::<T>()
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        self.native_payload.as_mut()?.downcast_mut::<T>()
    }

    pub fn class(&self) -> Container<Class> {
        self.class.clone()
    }

    pub fn class_ref(&self) -> &Container<Class> {
        &self.class
    }
}

impl IndexWrite for Container<Object> {
    fn setitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        let _ = interpreter.call_method(
            &TreewalkValue::Object(self.clone()),
            Dunder::SetItem,
            args![index, value],
        )?;

        Ok(())
    }

    fn delitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<()> {
        let _ = interpreter.call_method(
            &TreewalkValue::Object(self.clone()),
            Dunder::DelItem,
            args![index],
        )?;

        Ok(())
    }
}

impl IndexRead for Container<Object> {
    fn getitem(
        &self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        let result = interpreter.call_method(
            &TreewalkValue::Object(self.clone()),
            Dunder::GetItem,
            args![index],
        )?;

        Ok(Some(result))
    }
}

impl MemberRead for Container<Object> {
    /// According to Python's rules, when searching for a member of an object, we must look at
    /// itself and its class (following its MRO), but NOT its class' metaclasses.
    fn get_member(
        &self,
        interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        log(LogLevel::Debug, || format!("Searching for: {self}.{name}"));

        if let Some(attr) = self.borrow().scope.get(name) {
            log(LogLevel::Debug, || {
                format!("Found: {self}.{name} on instance")
            });
            return Ok(Some(attr));
        }

        if let Some(attr) = self.borrow().class.get_from_class(name) {
            log(LogLevel::Debug, || {
                format!(
                    "Found: {}::{} on class [from object]",
                    self.borrow().class,
                    name
                )
            });
            let instance = TreewalkValue::Object(self.clone());
            let owner = instance.get_class(interpreter);
            return Ok(Some(attr.resolve_descriptor(
                interpreter,
                Some(instance),
                owner,
            )?));
        }

        Ok(None)
    }

    fn dir(&self) -> Vec<String> {
        let mut symbols = self.borrow().scope.symbols();
        symbols.sort();
        symbols
    }
}

impl MemberWrite for Container<Object> {
    fn set_member(
        &mut self,
        interpreter: &TreewalkInterpreter,
        name: &str,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        if let Some(attr) = self.borrow().class.get_from_class(name) {
            log(LogLevel::Debug, || {
                format!(
                    "Found data descriptor: {}::{} on class",
                    self.borrow().class,
                    name
                )
            });
            if let Some(descriptor) = attr.into_data_descriptor(interpreter)? {
                descriptor.set_attr(interpreter, TreewalkValue::Object(self.clone()), value)?;
                return Ok(());
            }
        }

        log(LogLevel::Debug, || {
            format!("Setting: {self}.{name} on instance")
        });
        self.borrow_mut().scope.insert(name, value);
        Ok(())
    }

    fn delete_member(
        &mut self,
        interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<()> {
        if let Some(attr) = self.borrow().class.get_from_class(name) {
            log(LogLevel::Debug, || {
                format!(
                    "Found data descriptor: {}::{} on class",
                    self.borrow().class,
                    name
                )
            });
            if let Some(descriptor) = attr.into_data_descriptor(interpreter)? {
                descriptor.delete_attr(interpreter, TreewalkValue::Object(self.clone()))?;
                return Ok(());
            }
        }

        // A delete operation will throw an error if that field is not present in the
        // instance's `Dunder::Dict`, meaning we should not do a class + MRO lookup which
        // would happen if we called `get_member`.
        let result = TreewalkValue::Object(self.clone());
        if !result
            .clone()
            .into_member_reader(interpreter)
            .get_member(interpreter, &Dunder::Dict)?
            .ok_or_else(|| {
                Exception::attribute_error(
                    result.class_name(interpreter),
                    Dunder::Dict.as_ref(),
                )
            })
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?
            .borrow()
            .has(interpreter.clone(), &TreewalkValue::Str(Str::new(name)))
        {
            return Exception::attribute_error(result.class_name(interpreter), name)
                .raise(interpreter);
        }

        log(LogLevel::Debug, || {
            format!("Deleting: {self}.{name} on instance")
        });
        self.borrow_mut().scope.delete(name);
        Ok(())
    }
}

/// This is for when an [`Object`] _is_ a descriptor, not when an object _has_ a descriptor.
impl NonDataDescriptor for Container<Object> {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        interpreter.call_method(
            &TreewalkValue::Object(self.clone()),
            Dunder::Get,
            args![
                instance.unwrap_or(TreewalkValue::None),
                TreewalkValue::Class(owner)
            ],
        )
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}

impl DataDescriptor for Container<Object> {
    fn set_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        interpreter.call_method(
            &TreewalkValue::Object(self.clone()),
            Dunder::Set,
            args![instance, value],
        )?;

        Ok(())
    }

    fn delete_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: TreewalkValue,
    ) -> TreewalkResult<()> {
        interpreter.call_method(
            &TreewalkValue::Object(self.clone()),
            Dunder::Delete,
            args![instance],
        )?;

        Ok(())
    }
}

impl Display for Container<Object> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(
            f,
            "<{} object at {:p}>",
            self.borrow().class.borrow().name(),
            self
        )
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct InitBuiltin;
#[derive(Clone)]
struct EqBuiltin;
#[derive(Clone)]
struct NeBuiltin;
#[derive(Clone)]
struct AddBuiltin;
#[derive(Clone)]
struct SubBuiltin;
#[derive(Clone)]
struct MulBuiltin;
#[derive(Clone)]
struct TruedivBuiltin;
#[derive(Clone)]
struct LeBuiltin;
#[derive(Clone)]
struct LtBuiltin;
#[derive(Clone)]
struct GeBuiltin;
#[derive(Clone)]
struct GtBuiltin;
#[derive(Clone)]
struct ContainsBuiltin;
#[derive(Clone)]
struct HashBuiltin;
#[derive(Clone)]
struct StrBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        // This is builtin for 'object' but the instance is created from the `cls` passed in as the
        // first argument.
        let class = args.get_arg(0).as_class().raise(interpreter)?;
        Ok(TreewalkValue::Object(Container::new(Object::new(class))))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for InitBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::Init.into()
    }
}

impl Callable for EqBuiltin {
    /// The default behavior in Python for the `==` sign is to compare the object identity. This is
    /// only used when `Dunder::Eq` is not overridden by a user-defined class.
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a = args.get_self().raise(interpreter)?;
        let b = args.get_arg(0);

        Ok(TreewalkValue::Bool(a == b))
    }

    fn name(&self) -> String {
        Dunder::Eq.into()
    }
}

impl Callable for ContainsBuiltin {
    /// The default behavior in Python for the "in" operator is to iterate and compare element by
    /// element. This is used when `Dunder::Contains` is not overridden by another type or
    /// user-defined class.
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let left = args.get_self().raise(interpreter)?;
        let right = args.get_arg(0);

        let mut iterable = left.as_iterator().raise(interpreter)?;
        Ok(TreewalkValue::Bool(iterable.any(|i| i == right)))
    }

    fn name(&self) -> String {
        Dunder::Contains.into()
    }
}

impl Callable for HashBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;
        let object = args.get_self().raise(interpreter)?;
        Ok(TreewalkValue::Int(object.hash() as i64))
    }

    fn name(&self) -> String {
        Dunder::Hash.into()
    }
}

impl Callable for NeBuiltin {
    /// The default behavior in Python for the `!=` sign is to call the `Dunder::Eq` and invert the
    /// result. This is only used when `Dunder::Ne` is not overridden by a user-defined class.
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        let receiver = args.get_self().raise(interpreter)?;
        let result = interpreter.call_method(&receiver, Dunder::Eq, args![args.get_arg(0)])?;

        Ok(result.not())
    }

    fn name(&self) -> String {
        Dunder::Ne.into()
    }
}

impl Callable for AddBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a_type = args.get_self().raise(interpreter)?.get_type();
        let b_type = args.get_arg(0).get_type();

        // This is only implemented for int and float
        Exception::type_error(format!(
            "unsupported operand type(s) for +: '{}' and '{}'",
            a_type, b_type
        ))
        .raise(interpreter)
    }

    fn name(&self) -> String {
        Dunder::Add.into()
    }
}

impl Callable for SubBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a_type = args.get_self().raise(interpreter)?.get_type();
        let b_type = args.get_arg(0).get_type();

        // This is only implemented for int and float
        Exception::type_error(format!(
            "unsupported operand type(s) for -: '{}' and '{}'",
            a_type, b_type
        ))
        .raise(interpreter)
    }

    fn name(&self) -> String {
        Dunder::Sub.into()
    }
}

impl Callable for MulBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a_type = args.get_self().raise(interpreter)?.get_type();
        let b_type = args.get_arg(0).get_type();

        // This is only implemented for int and float
        Exception::type_error(format!(
            "unsupported operand type(s) for *: '{}' and '{}'",
            a_type, b_type
        ))
        .raise(interpreter)
    }

    fn name(&self) -> String {
        Dunder::Mul.into()
    }
}

impl Callable for TruedivBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a_type = args.get_self().raise(interpreter)?.get_type();
        let b_type = args.get_arg(0).get_type();

        // This is only implemented for int and float
        Exception::type_error(format!(
            "unsupported operand type(s) for /: '{}' and '{}'",
            a_type, b_type
        ))
        .raise(interpreter)
    }

    fn name(&self) -> String {
        Dunder::Truediv.into()
    }
}

impl Callable for LtBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a_type = args.get_self().raise(interpreter)?.get_type();
        let b_type = args.get_arg(0).get_type();

        // This is only implemented for int and float
        Exception::type_error(format!(
            "unsupported operand type(s) for <: '{}' and '{}'",
            a_type, b_type
        ))
        .raise(interpreter)
    }

    fn name(&self) -> String {
        Dunder::Lt.into()
    }
}

impl Callable for LeBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a_type = args.get_self().raise(interpreter)?.get_type();
        let b_type = args.get_arg(0).get_type();

        // This is only implemented for int and float
        Exception::type_error(format!(
            "unsupported operand type(s) for <=: '{}' and '{}'",
            a_type, b_type
        ))
        .raise(interpreter)
    }

    fn name(&self) -> String {
        Dunder::Le.into()
    }
}

impl Callable for GtBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a_type = args.get_self().raise(interpreter)?.get_type();
        let b_type = args.get_arg(0).get_type();

        // This is only implemented for int and float
        Exception::type_error(format!(
            "unsupported operand type(s) for >: '{}' and '{}'",
            a_type, b_type
        ))
        .raise(interpreter)
    }

    fn name(&self) -> String {
        Dunder::Gt.into()
    }
}

impl Callable for GeBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a_type = args.get_self().raise(interpreter)?.get_type();
        let b_type = args.get_arg(0).get_type();

        // This is only implemented for int and float
        Exception::type_error(format!(
            "unsupported operand type(s) for >=: '{}' and '{}'",
            a_type, b_type
        ))
        .raise(interpreter)
    }

    fn name(&self) -> String {
        Dunder::Ge.into()
    }
}

impl Callable for StrBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!()
    }

    fn name(&self) -> String {
        Dunder::Str.into()
    }
}

#[derive(Clone)]
struct DictDescriptor;

impl NonDataDescriptor for DictDescriptor {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        let scope = match instance {
            Some(i) => i.as_object().raise(interpreter)?.borrow().scope.clone(),
            None => owner.borrow().scope.clone(),
        };
        Ok(TreewalkValue::Dict(scope.as_dict(interpreter)))
    }

    fn name(&self) -> String {
        Dunder::Dict.into()
    }
}

impl DataDescriptor for DictDescriptor {
    fn set_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        _instance: TreewalkValue,
        _value: TreewalkValue,
    ) -> TreewalkResult<()> {
        todo!();
    }

    fn delete_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        _instance: TreewalkValue,
    ) -> TreewalkResult<()> {
        todo!();
    }
}
