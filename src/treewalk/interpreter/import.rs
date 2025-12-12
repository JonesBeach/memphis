#[cfg(feature = "c_stdlib")]
use crate::treewalk::types::cpython::import_from_cpython;
use crate::{
    core::Container,
    domain::{DomainResult, ExecutionError, ModuleName, Source},
    errors::MemphisError,
    treewalk::{
        import_utils, result::Raise, types::Module, TreewalkContext, TreewalkDisruption,
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn load_module(&self, module_name: &ModuleName) -> TreewalkResult<TreewalkValue> {
        if let Some(module) = self.state.fetch_module(module_name) {
            return Ok(TreewalkValue::Module(module));
        }

        #[cfg(feature = "c_stdlib")]
        if let Some(result) = import_from_cpython(self, module_name) {
            return Ok(result);
        }

        let module = self.import_module(module_name)?;
        Ok(TreewalkValue::Module(module))
    }

    pub fn execute_import(
        &self,
        module_name: &ModuleName,
        module: TreewalkValue,
        alias: &Option<String>,
    ) -> TreewalkResult<()> {
        // This is a case where it's simpler if we have an alias: just make the module available
        // at the alias.
        if let Some(alias) = alias {
            self.state.write(alias, module);
        } else {
            // Otherwise, we must create a module chain. For example:
            //
            // import mypackage.myothermodule
            //
            // must be used as
            //
            // mypackage.myothermodule.add('1', '1')
            let outer_module = import_utils::build_module_chain(module_name, module).raise(self)?;
            let symbol_name = module_name
                .head()
                .ok_or_else(ExecutionError::runtime_error)
                .raise(self)?;
            self.state.write(symbol_name, outer_module);
        }

        Ok(())
    }

    fn prepare_imported_module(
        &self,
        module_name: &ModuleName,
        source: &Source,
    ) -> Container<Module> {
        let module = Container::new(Module::new(module_name.clone(), source.clone()));

        // Before we parse and evaluate this module, store an empty module as a placeholder. This
        // is necessary to indicate to downstream modules that the upstream module which called
        // them but hasn't yet finished importing is in progress. Without this, you would get an
        // infinite loop from any circular imports.
        //
        // We don't need to store again after evaluating this module because the object pushed onto
        // the module stack during execution uses `Container<_>` and refers to this same module.
        self.state.store_module(module.clone());

        module
    }

    fn enter_imported_module(&self, module: Container<Module>) {
        self.state.save_line_number();
        self.state.push_module_context(module);
    }

    fn exit_imported_module(&self) -> DomainResult<Container<Module>> {
        self.state
            .pop_stack_frame()
            .ok_or_else(ExecutionError::runtime_error)?;
        self.state
            .pop_module()
            .ok_or_else(ExecutionError::runtime_error)
    }

    fn import_module(&self, module_name: &ModuleName) -> TreewalkResult<Container<Module>> {
        let source = self
            .state
            .memphis_state()
            .load_source(module_name)
            .raise(self)?;

        let module = self.prepare_imported_module(module_name, &source);
        self.enter_imported_module(module);

        match TreewalkContext::from_state(source, self.state.clone()).run_inner() {
            Ok(_) => {}
            Err(MemphisError::Execution(e)) => return Err(TreewalkDisruption::Error(e)),
            Err(MemphisError::Parser(e)) => {
                println!("{e}");
                return Err(self.raise(ExecutionError::SyntaxError));
            }
            _ => unreachable!(),
        };

        let module = self.exit_imported_module().raise(self)?;
        Ok(module)
    }
}
