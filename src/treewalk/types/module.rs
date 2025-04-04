#[cfg(feature = "c_stdlib")]
use std::collections::hash_map::Iter;
use std::{
    fmt::{Display, Error, Formatter},
    path::Path,
};

use crate::{
    core::{log, Container, LogLevel},
    domain::{ExecutionErrorKind, Source},
    init::MemphisContext,
    parser::types::ImportPath,
    treewalk::{
        interpreter::{TreewalkDisruption, TreewalkResult},
        Interpreter, Scope,
    },
    types::errors::MemphisError,
};

use super::{domain::traits::MemberReader, Dict, ExprResult};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Module {
    scope: Scope,
    source: Source,
}

impl Module {
    pub fn import(
        interpreter: &Interpreter,
        import_path: &ImportPath,
    ) -> TreewalkResult<Container<Self>> {
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

        let source = interpreter
            .state
            .load_source(import_path)
            .ok_or_else(|| interpreter.import_error(import_path.as_str()))?;

        interpreter.state.save_line_number();
        interpreter.state.push_stack_frame(&source);
        interpreter
            .state
            .push_module(Container::new(Module::new(source.clone())));

        let mut context = MemphisContext::from_module_from_treewalk(
            source,
            interpreter.state.memphis_state(),
            interpreter.state.clone(),
        );

        match context.evaluate() {
            Ok(_) => {}
            Err(MemphisError::Execution(e)) => return Err(TreewalkDisruption::Error(e)),
            Err(MemphisError::Parser(e)) => {
                println!("{}", e);
                return Err(interpreter.error(ExecutionErrorKind::SyntaxError));
            }
            _ => unreachable!(),
        };

        interpreter.state.pop_stack_frame();
        let module = interpreter
            .state
            .pop_module()
            .ok_or_else(|| interpreter.runtime_error())?;

        interpreter.state.store_module(import_path, module.clone());
        Ok(module)
    }

    pub fn new(source: Source) -> Self {
        Self::with_scope(source, Scope::default())
    }

    pub fn with_scope(source: Source, scope: Scope) -> Self {
        Self { scope, source }
    }

    pub fn path(&self) -> &Path {
        self.source.display_path()
    }

    pub fn name(&self) -> &str {
        self.source.name()
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
    #[cfg(feature = "c_stdlib")]
    pub fn dict(&self) -> Iter<String, ExprResult> {
        self.scope.into_iter()
    }

    pub fn as_dict(&self, interpreter: &Interpreter) -> Container<Dict> {
        self.scope.as_dict(interpreter)
    }
}

impl MemberReader for Module {
    fn get_member(
        &self,
        _interpreter: &Interpreter,
        name: &str,
    ) -> TreewalkResult<Option<ExprResult>> {
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
