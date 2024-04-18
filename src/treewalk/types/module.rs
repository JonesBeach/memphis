use std::fmt::{Display, Error, Formatter};
use std::path::PathBuf;

use crate::core::{log, Container, InterpreterEntrypoint, LogLevel};
use crate::init::Builder;
use crate::parser::types::ImportPath;
use crate::treewalk::{Interpreter, LoadedModule, Scope};
use crate::types::errors::{InterpreterError, MemphisError};

use super::{traits::MemberAccessor, ExprResult};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Module {
    pub scope: Scope,
    loaded_module: Option<LoadedModule>,
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

        let (mut parser, mut sub_interpreter) = Builder::new()
            .state(interpreter.state.clone())
            .module(loaded_module.clone())
            .build();

        interpreter
            .state
            .push_module(Container::new(Module::new(loaded_module, Scope::default())));
        match sub_interpreter.run(&mut parser) {
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

    pub fn new(loaded_module: LoadedModule, scope: Scope) -> Self {
        Self {
            loaded_module: Some(loaded_module),
            scope,
        }
    }

    pub fn path(&self) -> PathBuf {
        self.loaded_module
            .clone()
            .map_or(LoadedModule::empty_path(), |m| m.path())
    }

    pub fn name(&self) -> String {
        self.loaded_module
            .clone()
            .map_or(LoadedModule::empty_name(), |m| m.name())
    }
}

impl MemberAccessor for Module {
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

    fn delete_member(&mut self, _name: &str) -> Option<ExprResult> {
        unimplemented!();
    }

    fn set_member(&mut self, _name: &str, _value: ExprResult) {
        unimplemented!();
    }
}

impl Display for Container<Module> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<module '{}'>", self.borrow().name())
    }
}
