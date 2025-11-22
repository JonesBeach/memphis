#[cfg(feature = "c_stdlib")]
use std::collections::hash_map::Iter;
use std::{
    fmt::{Display, Error, Formatter},
    path::PathBuf,
};

use crate::{
    core::{log, Container, LogLevel},
    domain::{DebugStackFrame, Dunder, ExecutionError, ModuleName, Source, ToDebugStackFrame},
    errors::MemphisError,
    treewalk::{
        protocols::MemberRead,
        result::Raise,
        types::{Dict, Str},
        Scope, TreewalkContext, TreewalkDisruption, TreewalkInterpreter, TreewalkResult,
        TreewalkValue,
    },
};

#[derive(Debug, PartialEq, Clone)]
enum ModuleOrigin {
    File(Source),
    Builtin,
    Synthetic,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    name: ModuleName,
    scope: Scope,
    origin: ModuleOrigin,
}

impl Module {
    pub fn import(
        interpreter: &TreewalkInterpreter,
        module_name: &ModuleName,
    ) -> TreewalkResult<Container<Self>> {
        log(LogLevel::Debug, || format!("Reading {module_name}"));

        let source = interpreter
            .state
            .memphis_state()
            .load_source(module_name)
            .raise(interpreter)?;

        let module = Container::new(Module::new(module_name.clone(), source.clone()));

        // Before we parse and evaluate this module, store an empty module as a placeholder. This
        // is necessary to indicate to downstream modules that the upstream module which called
        // them but hasn't yet finished importing is in progress. Without this, you would get an
        // infinite loop from any circular imports.
        //
        // We don't need to store again after evaluating this module because the object pushed onto
        // the module stack during execution uses `Container<_>` and refers to this same module.
        interpreter.state.store_module(module.clone());

        interpreter.state.save_line_number();
        interpreter.state.push_stack_frame(&*module.borrow());
        interpreter.state.push_module(module);

        let mut context = TreewalkContext::from_state(source, interpreter.state.clone());

        match context.run() {
            Ok(_) => {}
            Err(MemphisError::Execution(e)) => return Err(TreewalkDisruption::Error(e)),
            Err(MemphisError::Parser(e)) => {
                println!("{e}");
                return Err(interpreter.raise(ExecutionError::SyntaxError));
            }
            _ => unreachable!(),
        };

        interpreter
            .state
            .pop_stack_frame()
            .ok_or_else(ExecutionError::runtime_error)
            .raise(interpreter)?;
        let module = interpreter
            .state
            .pop_module()
            .ok_or_else(ExecutionError::runtime_error)
            .raise(interpreter)?;
        Ok(module)
    }

    pub fn new(name: ModuleName, source: Source) -> Self {
        Self::_new(name, ModuleOrigin::File(source))
    }

    fn _new(name: ModuleName, origin: ModuleOrigin) -> Self {
        let mut scope = Scope::default();
        scope.insert(&Dunder::Name, TreewalkValue::Str(Str::new(&name.as_str())));
        Self {
            name,
            scope,
            origin,
        }
    }

    pub fn new_builtin(name: ModuleName) -> Self {
        Self::_new(name, ModuleOrigin::Builtin)
    }

    pub fn new_empty(name: ModuleName) -> Self {
        Self::_new(name, ModuleOrigin::Synthetic)
    }

    pub fn path(&self) -> PathBuf {
        match &self.origin {
            ModuleOrigin::File(s) => s.display_path().to_path_buf(),
            ModuleOrigin::Builtin => PathBuf::from("builtin"),
            ModuleOrigin::Synthetic => PathBuf::from("synthetic"),
        }
    }

    pub fn name(&self) -> &ModuleName {
        &self.name
    }

    pub fn get(&self, name: &str) -> Option<TreewalkValue> {
        self.scope.get(name)
    }

    pub fn insert(&mut self, name: &str, value: TreewalkValue) {
        self.scope.insert(name, value);
    }

    pub fn delete(&mut self, name: &str) -> Option<TreewalkValue> {
        self.scope.delete(name)
    }

    // Should this return an actual dict? We chose not to do that right now because a
    // `Container<Dict>` requires a reference to the interpreter.
    #[cfg(feature = "c_stdlib")]
    pub fn dict(&self) -> Iter<'_, String, TreewalkValue> {
        self.scope.into_iter()
    }

    pub fn as_dict(&self, interpreter: &TreewalkInterpreter) -> Container<Dict> {
        self.scope.as_dict(interpreter)
    }
}

impl MemberRead for Module {
    fn get_member(
        &self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(self.scope.get(name))
    }

    fn dir(&self) -> Vec<String> {
        self.scope.symbols()
    }
}

impl Display for Container<Module> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<module '{}'>", self.borrow().name())
    }
}

impl ToDebugStackFrame for Module {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new("<module>", self.path(), 1)
    }
}
