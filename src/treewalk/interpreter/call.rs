use crate::{
    core::{log, Container, LogLevel},
    domain::FunctionType,
    treewalk::{
        result::Raise,
        type_system::CloneableCallable,
        types::{iterators::GeneratorIter, Coroutine, Exception, Function, Generator},
        utils::Args,
        Scope, TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal,
        TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn call(
        &self,
        callable: Box<dyn CloneableCallable>,
        args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        let args = args.with_bound_receiver(callable.receiver());
        self.dispatch_callable(callable, args)
    }

    pub fn call_method<S>(
        &self,
        receiver: &TreewalkValue,
        name: S,
        args: Args,
    ) -> TreewalkResult<TreewalkValue>
    where
        S: AsRef<str>,
    {
        log(LogLevel::Debug, || {
            format!("Calling method {}.{}", receiver, name.as_ref())
        });
        log(LogLevel::Trace, || {
            format!("... from module: {}", self.state.current_module())
        });
        log(LogLevel::Trace, || {
            format!(
                "... from path: {}",
                self.state.current_module().borrow().path().display()
            )
        });
        if let Some(class) = self.state.current_class() {
            log(LogLevel::Trace, || format!("... from class: {class}"));
        }

        let method = self.load_method(receiver, name)?;
        self.call(method, args)
    }

    fn dispatch_callable(
        &self,
        callable: Box<dyn CloneableCallable>,
        args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        match callable.function_type() {
            FunctionType::Generator => {
                // TODO we may want to support builtin generators in the future. For now, we only
                // support user-defined so we are safe to downcast to `Container<Function>`.
                let function = callable
                    .as_any()
                    .downcast_ref::<Container<Function>>()
                    .cloned()
                    .ok_or_else(|| Exception::type_error("Expected a function"))
                    .raise(self)?;
                let symbol_table = function.borrow().bind_args(&args, self)?;
                let scope = Container::new(Scope::new(symbol_table));
                let generator_function = Generator::new(scope, function);
                let generator_iterator = GeneratorIter::new(generator_function, self.clone());
                Ok(TreewalkValue::Generator(generator_iterator))
            }
            FunctionType::Async => {
                let function = callable
                    .as_any()
                    .downcast_ref::<Container<Function>>()
                    .cloned()
                    .ok_or_else(|| Exception::type_error("Expected a function"))
                    .raise(self)?;
                let symbol_table = function.borrow().bind_args(&args, self)?;
                let scope = Container::new(Scope::new(symbol_table));
                let coroutine = Coroutine::new(scope, function);
                Ok(TreewalkValue::Coroutine(Container::new(coroutine)))
            }
            FunctionType::Regular => match callable.call(self, args) {
                Err(TreewalkDisruption::Signal(TreewalkSignal::Return(result))) => Ok(result),
                Err(e) => Err(e),
                Ok(result) => Ok(result),
            },
        }
    }
}
