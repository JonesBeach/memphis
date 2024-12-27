use std::{
    collections::hash_map::Iter,
    fmt::{Display, Error, Formatter},
    path::Path,
};

use crate::{
    core::{log, Container, LogLevel},
    init::MemphisContext,
    parser::types::ImportPath,
    treewalk::{Interpreter, ModuleSource, Scope},
    types::errors::{InterpreterError, MemphisError},
};

use super::{domain::traits::MemberReader, Dict, ExprResult};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Module {
    scope: Scope,
    loaded_module: ModuleSource,
}

impl Module {
    pub fn import(
        interpreter: &Interpreter,
        import_path: &ImportPath,
    ) -> Result<Container<Self>, InterpreterError> {
        log(LogLevel::Debug, || format!("Reading {}", import_path));
        if let Some(module) = interpreter.state.fetch_module(import_path) {
            return Ok(module);
        }

        // Before we parse and evaluate this module, store an empty module as a placeholder. This
        // is necessary to indicate to downstream modules that the upstream module which called
        // them but hasn't yet finished importing is in progress. Without this, you would get an
        // infinite loop from any circular imports.
        interpreter
            .state
            .store_module(import_path, Container::new(Module::default()));

        // Fetch the call stack separately so we don't produce a mutable borrow error in the
        // next statement.
        let call_stack = interpreter.state.call_stack();
        let loaded_module = interpreter
            .state
            .load_module(import_path, call_stack.current_path())
            .ok_or(InterpreterError::ModuleNotFound(
                import_path.as_str().to_string(),
                call_stack,
            ))?;

        let mut context = MemphisContext::from_module_with_state(
            loaded_module.clone(),
            Some(interpreter.state.clone()),
        );

        interpreter
            .state
            .push_module(Container::new(Module::new(loaded_module, Scope::default())));
        match context.run() {
            Ok(_) => {}
            Err(MemphisError::Interpreter(e)) => return Err(e),
            Err(MemphisError::Parser(e)) => {
                println!("{}", e);
                return Err(InterpreterError::SyntaxError(
                    interpreter.state.call_stack(),
                ));
            }
            _ => unreachable!(),
        };

        interpreter.state.pop_context();
        let module = interpreter
            .state
            .pop_module()
            .ok_or(InterpreterError::RuntimeError)?;
        interpreter.state.store_module(import_path, module.clone());
        Ok(module)
    }

    pub fn new(loaded_module: ModuleSource, scope: Scope) -> Self {
        Self {
            loaded_module,
            scope,
        }
    }

    pub fn path(&self) -> &Path {
        self.loaded_module.path()
    }

    pub fn name(&self) -> &str {
        self.loaded_module.name()
    }

    pub fn get(&self, name: &str) -> Option<ExprResult> {
        self.scope.get(name)
    }

    pub fn insert(&mut self, name: &str, value: ExprResult) {
        self.scope.insert(name, value);
    }

    pub fn delete(&mut self, name: &str) -> Option<ExprResult> {
        self.scope.delete(name)
    }

    // Should this return an actual dict? We chose not to do that right now because a
    // `Container<Dict>` requires a reference to the interpreter.
    pub fn dict(&self) -> Iter<String, ExprResult> {
        self.scope.into_iter()
    }

    // Should this return an actual dict? We chose not to do that right now because a
    // `Container<Dict>` requires a reference to the interpreter.
    pub fn as_dict(&self, interpreter: &Interpreter) -> Container<Dict> {
        self.scope.as_dict(interpreter)
    }
}

impl MemberReader for Module {
    fn get_member(
        &self,
        _interpreter: &Interpreter,
        name: &str,
    ) -> Result<Option<ExprResult>, InterpreterError> {
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
