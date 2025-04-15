use std::fmt::{Display, Error, Formatter};

use crate::{
    core::{log, Container, LogLevel},
    domain::{Dunder, Type},
    treewalk::{
        protocols::{
            Callable, DataDescriptor, DataDescriptorProvider, DescriptorProvider, IndexRead,
            IndexWrite, MemberReader, MemberWriter, MethodProvider, NonDataDescriptor, Typed,
        },
        types::{Class, Str},
        utils::{args, check_args, Arguments},
        Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct Object {
    pub class: Container<Class>,
    scope: Scope,
}

impl Typed for Object {
    fn get_type() -> Type {
        Type::Object
    }
}

impl MethodProvider for Object {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![
            Box::new(InitBuiltin),
            Box::new(NewBuiltin),
            Box::new(EqBuiltin),
            Box::new(NeBuiltin),
            Box::new(HashBuiltin),
            Box::new(StrBuiltin),
        ]
    }
}

impl DescriptorProvider for Object {
    fn get_descriptors() -> Vec<Box<dyn NonDataDescriptor>> {
        vec![]
    }
}

impl DataDescriptorProvider for Object {
    fn get_data_descriptors() -> Vec<Box<dyn DataDescriptor>> {
        vec![Box::new(DictDescriptor)]
    }
}

impl Object {
    /// Create the object with an empty symbol table. This is also called by the [`Dunder::New`]
    /// for [`Type::Object`] builtin.
    fn new_object_base(class: Container<Class>) -> TreewalkResult<Container<Object>> {
        Ok(Container::new(Self {
            class,
            scope: Scope::default(),
        }))
    }
}

impl IndexWrite for Container<Object> {
    fn setitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        let _ = interpreter.invoke_method(
            TreewalkValue::Object(self.clone()),
            Dunder::SetItem,
            &args![index, value],
        )?;

        Ok(())
    }

    fn delitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<()> {
        let _ = interpreter.invoke_method(
            TreewalkValue::Object(self.clone()),
            Dunder::DelItem,
            &args![index],
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
        let result = interpreter.invoke_method(
            TreewalkValue::Object(self.clone()),
            Dunder::GetItem,
            &args![index],
        )?;

        Ok(Some(result))
    }
}

impl MemberReader for Container<Object> {
    /// According to Python's rules, when searching for a member of an object, we must look at
    /// itself and its class (following its MRO), but NOT its class' metaclasses.
    fn get_member(
        &self,
        interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        log(LogLevel::Debug, || {
            format!("Searching for: {}.{}", self, name)
        });

        if let Some(attr) = self.borrow().scope.get(name) {
            log(LogLevel::Debug, || {
                format!("Found: {}.{} on instance", self, name)
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
            return Ok(Some(attr.resolve_nondata_descriptor(
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

impl MemberWriter for Container<Object> {
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
            if let Some(descriptor) = attr.as_data_descriptor(interpreter)? {
                descriptor.borrow().set_attr(
                    interpreter,
                    TreewalkValue::Object(self.clone()),
                    value,
                )?;
                return Ok(());
            }
        }

        log(LogLevel::Debug, || {
            format!("Setting: {}.{} on instance", self, name)
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
            if let Some(descriptor) = attr.as_data_descriptor(interpreter)? {
                descriptor
                    .borrow()
                    .delete_attr(interpreter, TreewalkValue::Object(self.clone()))?;
                return Ok(());
            }
        }

        // A delete operation will throw an error if that field is not present in the
        // instance's `Dunder::Dict`, meaning we should not do a class + MRO lookup which
        // would happen if we called `get_member`.
        let result = TreewalkValue::Object(self.clone());
        if !result
            .as_member_reader(interpreter)
            .get_member(interpreter, &Dunder::Dict)?
            .ok_or_else(|| interpreter.attribute_error(result.clone(), Dunder::Dict.as_ref()))?
            .expect_dict(interpreter)?
            .borrow()
            .has(
                interpreter.clone(),
                &TreewalkValue::String(Str::new(name.to_owned())),
            )
        {
            return Err(interpreter.attribute_error(result, name));
        }

        log(LogLevel::Debug, || {
            format!("Deleting: {}.{} on instance", self, name)
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
        interpreter.invoke_method(
            TreewalkValue::Object(self.clone()),
            Dunder::Get,
            &args![
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
        interpreter.invoke_method(
            TreewalkValue::Object(self.clone()),
            Dunder::Set,
            &args![instance, value],
        )?;

        Ok(())
    }

    fn delete_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: TreewalkValue,
    ) -> TreewalkResult<()> {
        interpreter.invoke_method(
            TreewalkValue::Object(self.clone()),
            Dunder::Delete,
            &args![instance],
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

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
        // This is builtin for 'object' but the instance is created from the `cls` passed in as the
        // first argument.
        let class = args.get_arg(0).expect_class(interpreter)?;
        Ok(TreewalkValue::Object(Object::new_object_base(class)?))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

struct InitBuiltin;

impl Callable for InitBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::Init.into()
    }
}

/// The default behavior in Python for the `==` sign is to compare the object identity. This is
/// only used when `Dunder::Eq` is not overridden by a user-defined class.
struct EqBuiltin;

impl Callable for EqBuiltin {
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args.expect_self(interpreter)?;
        let b = args.get_arg(0);

        Ok(TreewalkValue::Boolean(a == b))
    }

    fn name(&self) -> String {
        Dunder::Eq.into()
    }
}

struct HashBuiltin;

impl Callable for HashBuiltin {
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0, interpreter)?;
        let object = args.expect_self(interpreter)?;
        Ok(TreewalkValue::Integer(object.hash() as i64))
    }

    fn name(&self) -> String {
        Dunder::Hash.into()
    }
}

/// The default behavior in Python for the `!=` sign is to call the `Dunder::Eq` and invert the
/// result. This is only used when `Dunder::Ne` is not overridden by a user-defined class.
struct NeBuiltin;

impl Callable for NeBuiltin {
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
        let receiver = args.expect_self(interpreter)?;
        let result = interpreter.invoke_method(receiver, Dunder::Eq, &args![args.get_arg(0)])?;

        Ok(result.inverted())
    }

    fn name(&self) -> String {
        Dunder::Ne.into()
    }
}

struct StrBuiltin;

impl Callable for StrBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Arguments,
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
            Some(i) => i.expect_object(interpreter)?.borrow().scope.clone(),
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
