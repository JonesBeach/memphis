use std::fmt::{Display, Error, Formatter};

use crate::{
    core::{log, Container, LogLevel},
    domain::Type,
    treewalk::{
        protocols::{Callable, MemberRead, MemberWrite},
        utils::Args,
        Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, PartialEq)]
pub struct Class {
    name: String,

    /// This is semantically required to be non-empty, similar to `metaclass`.
    parent_classes: Vec<Container<Class>>,

    /// This is semantically required. See `Class::metaclass()` for an explanation of why it is
    /// optional in the struct definition.
    metaclass: Option<Container<Class>>,
    pub scope: Scope,

    // TODO consider whether we can deprecate this field.
    builtin_type: Option<Type>,
}

impl Class {
    /// Create a class directly, bypassing metaclass logic.
    /// Used for native types and the standard library.
    pub fn new_direct(
        name: impl Into<String>,
        metaclass: Option<Container<Class>>,
        parent_classes: Vec<Container<Class>>,
    ) -> Self {
        Self {
            name: name.into(),
            parent_classes,
            metaclass,
            scope: Scope::default(),
            builtin_type: None,
        }
    }

    pub fn new_builtin(
        type_: Type,
        metaclass: Option<Container<Class>>,
        parent_classes: Vec<Container<Class>>,
    ) -> Self {
        Self {
            name: String::from(&type_),
            parent_classes,
            metaclass,
            scope: Scope::default(),
            builtin_type: Some(type_),
        }
    }

    /// The primary accessor for the metaclass of a class. The property is optional because of
    /// the boot-strapping problem where the `Type::Type` class is the metaclass of itself.
    pub fn metaclass(&self) -> Container<Class> {
        self.metaclass
            .clone()
            .unwrap_or_else(|| panic!("attempted to access beyond the metaclass hierarchy!"))
    }

    /// This should only be used in a context that is known to contain only builtin types.
    pub fn builtin_type(&self) -> &Type {
        self.builtin_type.as_ref().unwrap_or_else(|| {
            panic!("attempted to access the builtin type for a user-defined type!")
        })
    }

    pub fn is_type(&self, type_: &Type) -> bool {
        if let Some(ref builtin_type) = self.builtin_type {
            builtin_type == type_
        } else {
            false
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    fn find_metaclass_inner(
        parent_classes: Vec<Container<Class>>,
        type_class: Container<Class>,
    ) -> Option<Container<Class>> {
        for parent_class in parent_classes.iter() {
            let metaclass = parent_class.borrow().metaclass();

            if !metaclass.same_identity(&type_class) {
                return Some(metaclass);
            } else {
                let parents = parent_class.borrow().parent_classes.clone();
                if let Some(metaclass) = Self::find_metaclass_inner(parents, type_class.clone()) {
                    return Some(metaclass);
                }
            }
        }

        None
    }

    /// - If a child class explicitly specifies a metaclass, that metaclass is used.
    /// - If the child class does not specify a metaclass:
    ///   - The child class will inherit the metaclass of its parent class. This means if the
    ///     parent class had a specific metaclass (other than type), the child class will also use
    ///     that metaclass, unless it explicitly specifies a different one.
    ///   - If neither the child class nor any of its parents specify a metaclass, then the default
    ///     metaclass type is used.
    pub fn find_metaclass(
        metaclass: Option<Container<Class>>,
        parent_classes: Vec<Container<Class>>,
        type_class: Container<Class>,
    ) -> Container<Class> {
        if let Some(metaclass) = metaclass {
            return metaclass;
        }

        // We cannot use MRO here because the class doesn't exist yet.
        if let Some(metaclass) = Self::find_metaclass_inner(parent_classes, type_class.clone()) {
            return metaclass;
        }

        type_class
    }

    /// Insert into the class scope. Used by `MemberWriter` or anywhere we do not have an
    /// `Interpreter`, like in the `TypeRegistry` on startup.
    pub fn set_on_class(&mut self, name: &str, value: TreewalkValue) {
        self.scope.insert(name, value);
    }
}

impl Container<Class> {
    fn mro_inner(&self) -> Vec<Container<Class>> {
        let mut hierarchy = vec![self.clone()];

        for parent in &self.borrow().parent_classes {
            hierarchy.push(parent.clone());
            let mut additional_parents = parent.mro_inner();
            hierarchy.append(&mut additional_parents);
        }

        hierarchy
    }

    /// Produce the Method Resolution Order (MRO) for this class. I could see this being a
    /// generator in the future, since many consumers do not require the full chain.
    pub fn mro(&self) -> Vec<Container<Class>> {
        let original = self.mro_inner();

        // The Python spec states that for any duplicates, the farthest right item should be kept.
        let mut hierarchy = vec![];
        for class in original.iter().rev() {
            if hierarchy
                .iter()
                .any(|c: &Container<Class>| c.same_identity(class))
            {
                continue;
            }

            hierarchy.push(class.clone());
        }

        hierarchy.iter().cloned().rev().collect()
    }

    pub fn super_mro(&self) -> Vec<Container<Class>> {
        self.mro()
            .iter()
            .skip(1)
            .cloned()
            .collect::<Vec<Container<Class>>>()
    }

    pub fn get_from_class(&self, name: &str) -> Option<TreewalkValue> {
        log(LogLevel::Debug, || format!("Searching for: {self}::{name}"));
        search(&self.mro(), name)
    }

    pub fn get_from_metaclass(&self, name: &str) -> Option<TreewalkValue> {
        log(LogLevel::Debug, || {
            format!("Searching for: {}::{}", self.borrow().metaclass(), name)
        });
        search(&self.borrow().metaclass().mro(), name)
    }
}

/// Search for the give attribute in the list of classes. MRO should happen before this!
fn search(iterable: &[Container<Class>], name: &str) -> Option<TreewalkValue> {
    for class in iterable {
        if let Some(attr) = class.borrow().scope.get(name) {
            return Some(attr);
        }
    }

    None
}

impl MemberRead for Container<Class> {
    /// Attribute access for a class uses this order:
    /// 1. the class itself
    /// 2. parent class MRO
    /// 3. metaclass of the class
    /// 4. metclass MRO
    fn get_member(
        &self,
        interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        if let Some(attr) = self.get_from_class(name) {
            log(LogLevel::Debug, || {
                format!("Found: {self}::{name} on class [from class]")
            });
            return Ok(Some(attr.resolve_descriptor(
                interpreter,
                None,
                self.clone(),
            )?));
        }

        if let Some(attr) = self.get_from_metaclass(name) {
            log(LogLevel::Debug, || {
                format!("Found: {self}::{name} on metaclass")
            });
            return Ok(Some(attr.resolve_descriptor(
                interpreter,
                Some(TreewalkValue::Class(self.clone())),
                self.borrow().metaclass(),
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

impl MemberWrite for Container<Class> {
    fn delete_member(
        &mut self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<()> {
        self.borrow_mut().scope.delete(name);

        // TODO support delete attributes from parent classes?
        todo!();
    }

    fn set_member(
        &mut self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        self.borrow_mut().set_on_class(name, value);
        Ok(())
    }
}

impl Display for Container<Class> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<class '{}'>", self.borrow().name())
    }
}

impl Callable for Container<Class> {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        TreewalkValue::new(interpreter, self.clone(), args)
    }

    fn name(&self) -> String {
        self.borrow().name.clone()
    }
}
