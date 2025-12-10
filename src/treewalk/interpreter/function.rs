use crate::{
    core::Container,
    parser::types::Expr,
    treewalk::{
        result::Raise, types::Function, utils::args, Scope, TreewalkDisruption,
        TreewalkInterpreter, TreewalkResult, TreewalkSignal, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub(crate) fn enter_function(
        &self,
        function: Container<Function>,
        scope: Container<Scope>,
    ) -> TreewalkResult<TreewalkValue> {
        let cross_module = !self
            .state
            .current_module()
            .same_identity(&function.borrow().module);
        if cross_module {
            self.state.push_module(function.borrow().module.clone());
        }

        self.state
            .push_captured_env(function.borrow().captured_env.clone());
        self.state.push_local(scope);

        // If we don't save the current line number, we won't properly record where in the current
        // file we called the next function from.
        self.state.save_line_number();
        self.state.push_stack_frame(&*function.borrow());
        self.state.push_function(function.clone());

        // We do not propagate errors here because we still must restore the scopes and things
        // before returning.
        let result = self.execute_ast(&function.borrow().body);

        // If an error is thrown, we should return that immediately without restoring any state.
        if matches!(
            result,
            Ok(_) | Err(TreewalkDisruption::Signal(TreewalkSignal::Return(_)))
        ) {
            self.state.pop_stack_frame();
            self.state.pop_function();
            self.state.pop_local();
            self.state.pop_captured_env();
            if cross_module {
                self.state.pop_module();
            }
        }

        result
    }

    pub fn apply_decorators(
        &self,
        function: Container<Function>,
        decorators: &[Expr],
    ) -> TreewalkResult<TreewalkValue> {
        let mut value = TreewalkValue::Function(function);

        for deco_expr in decorators {
            let decorator = self.evaluate_expr(deco_expr)?.as_callable().raise(self)?;
            value = self.call(decorator, args![value])?;
        }

        Ok(value)
    }
}
