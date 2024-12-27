use std::fmt::{Display, Error, Formatter};

use crate::{
    core::{log, Container, LogLevel},
    resolved_args,
    treewalk::{Interpreter, Scope},
    types::errors::InterpreterError,
};

use super::{
    domain::{
        traits::{Callable, MemberReader, MemberWriter},
        Type,
    },
    utils::{Dunder, ResolvedArguments},
    ExprResult, Str, Tuple,
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
    builtin_type: Option<Type>,
}

impl Class {
    /// The primary public interface to create a class. A metaclass will be used if one is found to
    /// have a `Dunder::New` method, falling back to the `Type::Type` metaclass.
    pub fn new(
        interpreter: &Interpreter,
        name: &str,
        parent_classes: Vec<Container<Class>>,
        metaclass: Option<Container<Class>>,
    ) -> Result<Container<Self>, InterpreterError> {
        let type_class = interpreter.state.get_type_class(Type::Type);
        let metaclass = Self::find_metaclass(metaclass, parent_classes.clone(), type_class);

        let bases = if parent_classes.is_empty() {
            ExprResult::Tuple(Tuple::default())
        } else {
            let bases = parent_classes
                .iter()
                .cloned()
                .map(ExprResult::Class)
                .collect::<Vec<ExprResult>>();
            ExprResult::Tuple(Tuple::new(bases))
        };

        let args = &resolved_args!(
            ExprResult::Class(metaclass.clone()),
            ExprResult::String(Str::new(name.into())),
            bases,
            ExprResult::Dict(Scope::default().as_dict(interpreter))
        );
        interpreter
            .invoke_method(ExprResult::Class(metaclass), &Dunder::New, args)?
            .as_class()
            .ok_or(InterpreterError::ExpectedClass(
                interpreter.state.call_stack(),
            ))
    }

    /// Create the class. This is used by `Dunder::New` for `Type::Type` under the hood.
    pub fn new_base(
        name: String,
        parent_classes: Vec<Container<Class>>,
        metaclass: Option<Container<Class>>,
        scope: Scope,
    ) -> Container<Self> {
        Container::new(Self {
            name,
            parent_classes,
            metaclass,
            scope,
            builtin_type: None,
        })
    }

    pub fn new_builtin(
        name: Type,
        metaclass: Option<Container<Class>>,
        parent_classes: Vec<Container<Class>>,
    ) -> Container<Self> {
        Container::new(Self {
            name: name.to_string(),
            parent_classes,
            metaclass,
            scope: Scope::default(),
            builtin_type: Some(name),
        })
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

    pub fn is_builtin_type(&self) -> bool {
        self.builtin_type.is_some()
    }

    pub fn is_metaclass(&self) -> bool {
        // is this correct?
        self.parent_classes
            .iter()
            .any(|c| c.borrow().is_type(&Type::Type))
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
    fn find_metaclass(
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

    /// Use the class MRO to search for an attribute. This does not consider metaclasses but it
    /// does consider the class itself.
    fn search(iterable: &[Container<Class>], name: &str) -> Option<ExprResult> {
        for class in iterable {
            if let Some(attr) = class.borrow().scope.get(name) {
                return Some(attr);
            }
        }

        None
    }

    fn search_mro(&self, name: &str) -> Option<ExprResult> {
        Self::search(&self.mro(), name)
    }

    pub fn get_from_class(&self, name: &str) -> Option<ExprResult> {
        log(LogLevel::Debug, || {
            format!("Searching for: {}::{}", self, name)
        });

        self.search_mro(name)
    }

    pub fn get_from_metaclass(&self, name: &str) -> Option<ExprResult> {
        log(LogLevel::Debug, || {
            format!("Searching for: {}::{}", self.borrow().metaclass(), name)
        });

        self.borrow().metaclass().search_mro(name)
    }

    /// Insert into the class scope. Used by `MemberWriter` or anywhere we do not have an
    /// `Interpreter`, like in the `TypeRegistry` on startup.
    pub fn set_on_class(&self, name: &str, value: ExprResult) {
        self.borrow_mut().scope.insert(name, value);
    }
}

impl MemberReader for Container<Class> {
    /// Attribute access for a class uses this order:
    /// 1. the class itself
    /// 2. parent class MRO
    /// 3. metaclass of the class
    /// 4. metclass MRO
    fn get_member(
        &self,
        interpreter: &Interpreter,
        name: &str,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        if let Some(attr) = self.get_from_class(name) {
            log(LogLevel::Debug, || {
                format!("Found: {}::{} on class [from class]", self, name)
            });
            return Ok(Some(attr.resolve_nondata_descriptor(
                interpreter,
                None,
                self.clone(),
            )?));
        }

        if let Some(attr) = self.get_from_metaclass(name) {
            log(LogLevel::Debug, || {
                format!("Found: {}::{} on metaclass", self, name)
            });
            return Ok(Some(attr.resolve_nondata_descriptor(
                interpreter,
                Some(ExprResult::Class(self.clone())),
                self.borrow().metaclass(),
            )?));
        }

        Ok(None)
    }
}

impl MemberWriter for Container<Class> {
    fn delete_member(
        &mut self,
        _interpreter: &Interpreter,
        name: &str,
    ) -> Result<(), InterpreterError> {
        self.borrow_mut().scope.delete(name);

        // TODO support delete attributes from parent classes?
        todo!();
    }

    fn set_member(
        &mut self,
        _interpreter: &Interpreter,
        name: &str,
        value: ExprResult,
    ) -> Result<(), InterpreterError> {
        self.set_on_class(name, value);
        Ok(())
    }
}

impl Display for Container<Class> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<class '{}'>", self.borrow().name())
    }
}

impl Callable for Container<Class> {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        ExprResult::new(interpreter, self.clone(), args)
    }

    fn name(&self) -> String {
        self.borrow().name.clone()
    }
}
