use std::collections::{HashMap, HashSet};
use std::fmt::Write;

#[cfg(feature = "c_stdlib")]
use super::types::cpython::import_from_cpython;
use super::{evaluators, Scope, State};
use crate::{
    core::{log, Container, InterpreterEntrypoint, LogLevel},
    domain::{Dunder, ExceptionLiteral, ExecutionError, ExecutionErrorKind},
    parser::{
        types::{
            Ast, BinOp, CallArgs, CompoundOperator, ConditionalBlock, DictOperation, ExceptClause,
            ExceptionInstance, Expr, FStringPart, ForClause, ImportPath, ImportedItem, LogicalOp,
            LoopIndex, Params, RegularImport, SliceParams, Statement, StatementKind, TypeNode,
            UnaryOp, Variable,
        },
        Parser,
    },
    resolved_args,
    treewalk::types::{
        domain::traits::{Callable, MemberReader},
        function::FunctionType,
        iterators::GeneratorIterator,
        utils::ResolvedArguments,
        Class, Coroutine, Dict, ExprResult, Function, Generator, List, Module, Set, Slice, Str,
        Tuple,
    },
    types::errors::MemphisError,
};

#[derive(Debug, PartialEq, Clone)]
pub enum TreewalkDisruption {
    Signal(TreewalkSignal), // Control flow (not errors)
    Error(ExecutionError),  // Actual Python runtime errors
}

#[derive(Debug, PartialEq, Clone)]
pub enum TreewalkSignal {
    Return(ExprResult),
    Raise,
    Await,
    Sleep,
    Break,
    Continue,
}

pub type TreewalkResult<T> = Result<T, TreewalkDisruption>;

#[derive(Clone)]
pub struct Interpreter {
    pub state: Container<State>,
}

impl Interpreter {
    pub fn new(state: Container<State>) -> Self {
        Interpreter { state }
    }

    pub fn error(&self, error_kind: ExecutionErrorKind) -> TreewalkDisruption {
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            error_kind,
        ))
    }

    pub fn type_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.type_error_optional_message(Some(message.into()))
    }

    pub fn type_error_optional_message(&self, message: Option<String>) -> TreewalkDisruption {
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::TypeError(message),
        ))
    }

    pub fn value_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::ValueError(message.into()),
        ))
    }

    pub fn key_error(&self, key: impl Into<String>) -> TreewalkDisruption {
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::KeyError(key.into()),
        ))
    }

    pub fn name_error(&self, name: impl Into<String>) -> TreewalkDisruption {
        self.state.save_line_number();
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::NameError(name.into()),
        ))
    }

    pub fn import_error(&self, name: impl Into<String>) -> TreewalkDisruption {
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::ImportError(name.into()),
        ))
    }

    pub fn runtime_error(&self) -> TreewalkDisruption {
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::RuntimeError,
        ))
    }

    pub fn assertion_error(&self) -> TreewalkDisruption {
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::AssertionError,
        ))
    }

    pub fn stop_iteration(&self) -> TreewalkDisruption {
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::StopIteration,
        ))
    }

    pub fn attribute_error(
        &self,
        object: ExprResult,
        attr: impl Into<String>,
    ) -> TreewalkDisruption {
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::AttributeError(
                object.get_class(self).borrow().name().to_string(),
                attr.into(),
            ),
        ))
    }

    pub fn call(
        &self,
        callable: Container<Box<dyn Callable>>,
        arguments: &ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        let mut bound_args = arguments.clone();
        if let Some(receiver) = callable.borrow().receiver() {
            bound_args.bind(receiver);
        }

        match callable.borrow().call(self, bound_args) {
            Err(TreewalkDisruption::Signal(TreewalkSignal::Return(result))) => Ok(result),
            Err(e) => Err(e),
            Ok(result) => Ok(result),
        }
    }

    pub fn call_function(
        &self,
        name: &str,
        arguments: &ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        let function = self.read_callable(name)?;
        self.call_function_inner(function, arguments)
    }

    pub fn invoke_function(
        &self,
        function: Container<Function>,
        scope: Container<Scope>,
    ) -> TreewalkResult<ExprResult> {
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
        self.state.save_line_number();
        self.state.push_stack_frame(&*function.borrow());
        self.state.push_function(function.clone());

        // We do not propagate errors here because we still must restore the scopes and things
        // before returning.
        let result = self.evaluate_ast(&function.borrow().body);

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

    pub fn resolve_method<S>(
        &self,
        receiver: ExprResult,
        name: S,
    ) -> TreewalkResult<Container<Box<dyn Callable>>>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();
        self.evaluate_member_access_inner(&receiver, name)?
            .expect_callable(self)
    }

    pub fn invoke_method<S>(
        &self,
        receiver: ExprResult,
        name: S,
        arguments: &ResolvedArguments,
    ) -> TreewalkResult<ExprResult>
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
            log(LogLevel::Trace, || format!("... from class: {}", class));
        }

        let method = self.resolve_method(receiver, name)?;
        self.call(method, arguments)
    }

    fn call_function_inner(
        &self,
        function: Container<Box<dyn Callable>>,
        arguments: &ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        let function_type = function.borrow().function_type();
        match function_type {
            FunctionType::Generator => {
                // TODO we may want to support builtin generators in the future. For now, we only
                // support user-defined so we are safe to downcast to `Container<Function>`.
                let function = function
                    .borrow()
                    .as_any()
                    .downcast_ref::<Container<Function>>()
                    .cloned()
                    .ok_or_else(|| self.type_error("Expected a function"))?;
                let scope = Scope::new(self, &function, arguments)?;
                let generator_function = Generator::new(scope, function);
                let generator_iterator = GeneratorIterator::new(generator_function, self.clone());
                Ok(ExprResult::Generator(Container::new(generator_iterator)))
            }
            FunctionType::Async => {
                let function = function
                    .borrow()
                    .as_any()
                    .downcast_ref::<Container<Function>>()
                    .cloned()
                    .ok_or_else(|| self.type_error("Expected a function"))?;
                let scope = Scope::new(self, &function, arguments)?;
                let coroutine = Coroutine::new(scope, function);
                Ok(ExprResult::Coroutine(Container::new(coroutine)))
            }
            FunctionType::Regular => self.call(function, arguments),
        }
    }

    // -----------------------------
    // End of higher-order functions
    // -----------------------------

    pub fn write_loop_index(&self, index: &LoopIndex, value: ExprResult) {
        match index {
            LoopIndex::Variable(var) => {
                self.state.write(var, value);
            }
            LoopIndex::Tuple(tuple_index) => {
                for (key, value) in tuple_index.iter().zip(value) {
                    self.state.write(key, value);
                }
            }
        };
    }

    fn read_callable(&self, name: &str) -> TreewalkResult<Container<Box<dyn Callable>>> {
        self.state
            .read_or_disrupt(name, self)?
            .expect_callable(self)
    }

    pub fn read_index(
        &self,
        object: &ExprResult,
        index: &ExprResult,
    ) -> TreewalkResult<ExprResult> {
        object
            .as_index_read(self)
            .ok_or_else(|| {
                self.type_error(format!(
                    "'{}' object is not subscriptable",
                    object.get_type()
                ))
            })?
            .getitem(self, index.clone())?
            .ok_or_else(|| self.key_error(index.to_string()))
    }

    fn evaluate_binary_operation_inner(
        &self,
        left: ExprResult,
        op: &BinOp,
        right: ExprResult,
    ) -> TreewalkResult<ExprResult> {
        if matches!(op, BinOp::In) {
            if let Some(mut iterable) = right.try_into_iter() {
                return Ok(ExprResult::Boolean(iterable.contains(left)));
            }
            return Err(self.type_error("Expected an iterable"));
        }

        if matches!(op, BinOp::NotIn) {
            if let Some(mut iterable) = right.try_into_iter() {
                return Ok(ExprResult::Boolean(!iterable.contains(left)));
            }
            return Err(self.type_error("Expected an iterable"));
        }

        if left.is_integer() && right.is_integer() {
            let left = left.expect_integer(self)?;
            let right = right.expect_integer(self)?;
            evaluators::evaluate_integer_operation(left, op, right, self)
        } else if left.is_fp() && right.is_fp() {
            let left = left.expect_fp(self)?;
            let right = right.expect_fp(self)?;
            evaluators::evaluate_floating_point_operation(left, op, right, self)
        } else if left.as_list().is_some() && right.as_list().is_some() {
            let left = left.expect_list(self)?;
            let right = right.expect_list(self)?;
            evaluators::evaluate_list_operation(left, op, right)
        } else if left.as_set().is_some() && right.as_set().is_some() {
            let left = left.expect_set(self)?;
            let right = right.expect_set(self)?;
            evaluators::evaluate_set_operation(left, op, right)
        } else if left.as_object().is_some()
            && right.as_object().is_some()
            && Dunder::try_from(op).is_ok()
        {
            let dunder = Dunder::try_from(op).unwrap_or_else(|_| unreachable!());
            self.invoke_method(left, &dunder, &resolved_args![right])
        } else {
            evaluators::evaluate_object_comparison(left, op, right)
        }
    }

    fn evaluate_member_access_inner<S>(
        &self,
        result: &ExprResult,
        field: S,
    ) -> TreewalkResult<ExprResult>
    where
        S: AsRef<str>,
    {
        log(LogLevel::Debug, || {
            format!("Member access {}.{}", result, field.as_ref())
        });
        result
            .as_member_reader(self)
            .get_member(self, field.as_ref())?
            .ok_or_else(|| self.attribute_error(result.clone(), field.as_ref()))
    }

    // -----------------------------
    // End of medium-order functions
    // -----------------------------

    fn evaluate_module_import(&self, import_path: &ImportPath) -> TreewalkResult<ExprResult> {
        // is this useful? is it valuable to read a module directly from the scope as opposed from
        // the module cache
        if let Some(module) = self.state.read(&import_path.as_str()) {
            return Ok(module);
        }

        #[cfg(feature = "c_stdlib")]
        if let Some(result) = import_from_cpython(self, import_path) {
            return Ok(result);
        }

        Ok(ExprResult::Module(Module::import(self, import_path)?))
    }

    fn evaluate_unary_operation(&self, op: &UnaryOp, right: &Expr) -> TreewalkResult<ExprResult> {
        let right = self.evaluate_expr(right)?;
        evaluators::evaluate_unary_operation(op, right, self)
    }

    fn evaluate_ternary_operation(
        &self,
        condition: &Expr,
        if_value: &Expr,
        else_value: &Expr,
    ) -> TreewalkResult<ExprResult> {
        if self.evaluate_expr(condition)?.as_boolean() {
            self.evaluate_expr(if_value)
        } else {
            self.evaluate_expr(else_value)
        }
    }

    fn evaluate_logical_operation(
        &self,
        left: &Expr,
        op: &LogicalOp,
        right: &Expr,
    ) -> TreewalkResult<ExprResult> {
        let left = self.evaluate_expr(left)?.as_boolean();
        let right = self.evaluate_expr(right)?.as_boolean();
        evaluators::evaluate_logical_op(left, op, right)
    }

    fn evaluate_binary_operation(
        &self,
        left: &Expr,
        op: &BinOp,
        right: &Expr,
    ) -> TreewalkResult<ExprResult> {
        let left = self.evaluate_expr(left)?;
        let right = self.evaluate_expr(right)?;

        self.evaluate_binary_operation_inner(left, op, right)
    }

    fn evaluate_member_access<S>(&self, object: &Expr, field: S) -> TreewalkResult<ExprResult>
    where
        S: AsRef<str>,
    {
        let result = self.evaluate_expr(object)?;
        self.evaluate_member_access_inner(&result, field)
    }

    fn evaluate_slice_operation(
        &self,
        object: &Expr,
        params: &SliceParams,
    ) -> TreewalkResult<ExprResult> {
        let object_result = self.evaluate_expr(object)?;
        let slice = Slice::resolve(self, params)?;

        self.read_index(&object_result, &ExprResult::Slice(slice))
    }

    fn evaluate_index_access(&self, object: &Expr, index: &Expr) -> TreewalkResult<ExprResult> {
        let object_result = self.evaluate_expr(object)?;
        let index_result = self.evaluate_expr(index)?;

        self.read_index(&object_result, &index_result)
    }

    fn evaluate_list(&self, items: &[Expr]) -> TreewalkResult<ExprResult> {
        items
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<Vec<_>, _>>()
            .map(|l| ExprResult::List(Container::new(List::new(l))))
    }

    fn evaluate_tuple(&self, items: &[Expr]) -> TreewalkResult<ExprResult> {
        let mut results = vec![];
        for item in items {
            let evaluated = self.evaluate_expr(item)?;
            match item {
                Expr::UnaryOperation {
                    op: UnaryOp::Unpack,
                    ..
                } => {
                    let list: Container<List> = evaluated
                        .try_into()
                        .map_err(|_| self.type_error("Expected a list"))?;
                    for elem in list {
                        results.push(elem);
                    }
                }
                _ => {
                    results.push(evaluated);
                }
            }
        }

        Ok(ExprResult::Tuple(Tuple::new(results)))
    }

    fn evaluate_set(&self, items: &HashSet<Expr>) -> TreewalkResult<ExprResult> {
        items
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<HashSet<_>, _>>()
            .map(Set::new)
            .map(Container::new)
            .map(ExprResult::Set)
    }

    fn evaluate_dict(&self, dict_ops: &[DictOperation]) -> TreewalkResult<ExprResult> {
        // TODO we are suppressing this clippy error for now because `ExprResult` allows interior
        // mutability which can lead to issues when used as a key for a `HashMap` or `HashSet`.
        #[allow(clippy::mutable_key_type)]
        let mut result = HashMap::new();
        for op in dict_ops {
            match op {
                DictOperation::Pair(key, value) => {
                    result.insert(self.evaluate_expr(key)?, self.evaluate_expr(value)?);
                }
                DictOperation::Unpack(expr) => {
                    let unpacked = self.evaluate_expr(expr)?;
                    for key in unpacked.clone() {
                        let value = self.read_index(&unpacked, &key)?;
                        result.insert(key, value); // later keys overwrite earlier ones
                    }
                }
            }
        }
        Ok(ExprResult::Dict(Container::new(Dict::new(self, result))))
    }

    fn evaluate_await(&self, expr: &Expr) -> TreewalkResult<ExprResult> {
        let coroutine_to_await = self.evaluate_expr(expr)?.expect_coroutine(self)?;

        if let Some(result) = coroutine_to_await.clone().borrow().is_finished() {
            Ok(result)
        } else if let Some(current_coroutine) = self
            .state
            .get_executor()
            .borrow()
            .current_coroutine
            .borrow()
            .clone()
        {
            self.state
                .get_executor()
                .borrow()
                .set_wait_on(current_coroutine, coroutine_to_await);
            Err(TreewalkDisruption::Signal(TreewalkSignal::Await))
        } else {
            Err(self.type_error("Expected a coroutine"))
        }
    }

    fn evaluate_delete(&self, exprs: &[Expr]) -> TreewalkResult<()> {
        for expr in exprs {
            match expr {
                Expr::Variable(name) => {
                    self.state.delete(name);
                }
                Expr::IndexAccess { object, index } => {
                    let index_result = self.evaluate_expr(index)?;
                    let object_result = self.evaluate_expr(object)?;
                    object_result
                        .as_index_write(self)
                        .ok_or_else(|| {
                            self.type_error(format!(
                                "'{}' object does not support item deletion",
                                object_result.get_type()
                            ))
                        })?
                        .delitem(self, index_result)?;
                }
                Expr::MemberAccess { object, field } => {
                    let object_result = self.evaluate_expr(object)?;
                    object_result
                        .as_member_writer()
                        .ok_or_else(|| self.attribute_error(object_result.clone(), field))?
                        .delete_member(self, field)?;
                }
                _ => return Err(self.type_error("cannot delete")),
            }
        }

        Ok(())
    }

    fn evaluate_return(&self, exprs: &[Expr]) -> TreewalkResult<ExprResult> {
        assert!(!exprs.is_empty());

        let results = exprs
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<Vec<_>, _>>()?;

        let return_val = if results.len() > 1 {
            ExprResult::Tuple(Tuple::new(results))
        } else {
            results[0].clone()
        };

        Err(TreewalkDisruption::Signal(TreewalkSignal::Return(
            return_val,
        )))
    }

    fn evaluate_assert(&self, expr: &Expr) -> TreewalkResult<()> {
        if self.evaluate_expr(expr)?.as_boolean() {
            Ok(())
        } else {
            Err(self.assertion_error())
        }
    }

    fn evaluate_f_string(&self, parts: &[FStringPart]) -> TreewalkResult<ExprResult> {
        let mut result = String::new();
        for part in parts {
            match part {
                FStringPart::String(s) => {
                    result.push_str(s);
                }
                FStringPart::Expr(e) => {
                    let r = self.evaluate_expr(&e.expr)?;
                    write!(result, "{}", r).unwrap();
                }
            }
        }

        Ok(ExprResult::String(Str::new(result)))
    }

    fn evaluate_ast(&self, ast: &Ast) -> TreewalkResult<ExprResult> {
        ast.iter()
            .try_fold(ExprResult::None, |_, stmt| self.evaluate_statement(stmt))
    }

    fn evaluate_function_call(
        &self,
        name: &str,
        arguments: &CallArgs,
        callee: &Option<Box<Expr>>,
    ) -> TreewalkResult<ExprResult> {
        let arguments = ResolvedArguments::from(self, arguments)?;

        let function = if let Some(callee) = callee {
            self.evaluate_expr(callee)?.expect_callable(self)?
        } else {
            self.read_callable(name)?
        };

        self.call_function_inner(function, &arguments)
    }

    fn evaluate_method_call<S>(
        &self,
        obj: &Expr,
        name: S,
        arguments: &CallArgs,
    ) -> TreewalkResult<ExprResult>
    where
        S: AsRef<str>,
    {
        let arguments = ResolvedArguments::from(self, arguments)?;
        let result = self.evaluate_expr(obj)?;

        self.invoke_method(result, name, &arguments)
    }

    fn evaluate_class_instantiation(
        &self,
        name: &str,
        arguments: &CallArgs,
    ) -> TreewalkResult<ExprResult> {
        log(LogLevel::Debug, || format!("Instantiating: {}", name));
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
            log(LogLevel::Trace, || format!("... from class: {}", class));
        }

        let class = self.read_callable(name)?;
        let arguments = ResolvedArguments::from(self, arguments)?;
        self.call(class, &arguments)
    }

    fn evaluate_compound_assignment(
        &self,
        operator: &CompoundOperator,
        target: &Expr,
        value: &Expr,
    ) -> TreewalkResult<()> {
        let bin_op = BinOp::from(operator);
        let result = self.evaluate_binary_operation(target, &bin_op, value)?;
        self.evaluate_assignment_inner(target, result)
    }

    /// Assignment functionality shared by traditional assignment such as `a = 1` and compound
    /// assignment such as `a += 1`.
    fn evaluate_assignment_inner(&self, name: &Expr, value: ExprResult) -> TreewalkResult<()> {
        match name {
            Expr::Variable(name) => {
                self.state.write(name, value.clone());
            }
            Expr::IndexAccess { object, index } => {
                let index_result = self.evaluate_expr(index)?;
                let object_result = self.evaluate_expr(object)?;
                object_result
                    .as_index_write(self)
                    .ok_or_else(|| {
                        self.type_error(format!(
                            "'{}' object does not support item assignment",
                            object_result.get_type()
                        ))
                    })?
                    .setitem(self, index_result, value)?;
            }
            Expr::MemberAccess { object, field } => {
                let result = self.evaluate_expr(object)?;
                result
                    .as_member_writer()
                    .ok_or_else(|| self.attribute_error(result, field))?
                    .set_member(self, field, value)?;
            }
            _ => return Err(self.type_error("cannot assign")),
        }

        Ok(())
    }

    fn evaluate_assignment(&self, name: &Expr, expr: &Expr) -> TreewalkResult<()> {
        let result = self.evaluate_expr(expr)?;
        self.evaluate_assignment_inner(name, result)
    }

    fn evaluate_multiple_assignment(&self, left: &[Expr], expr: &Expr) -> TreewalkResult<()> {
        let value = self.evaluate_expr(expr)?;
        for name in left {
            self.evaluate_assignment_inner(name, value.clone())?;
        }

        Ok(())
    }

    /// Python can unpack any iterables, not any index reads.
    fn evaluate_unpacking_assignment(&self, left: &[Expr], expr: &Expr) -> TreewalkResult<()> {
        let results = self.evaluate_expr(expr)?.into_iter();
        let right_len = results.clone().count();
        let left_len = left.len();

        if left_len < right_len {
            return Err(
                self.value_error(format!("too many values to unpack (expected {})", left_len,))
            );
        }

        if left.len() > right_len {
            return Err(self.value_error(format!(
                "not enough values to unpack (expected {}, got {})",
                left_len, right_len
            )));
        }

        for (key, value) in left.iter().zip(results) {
            self.evaluate_assignment_inner(key, value)?;
        }

        Ok(())
    }

    fn evaluate_lambda(&self, arguments: &Params, expr: &Expr) -> TreewalkResult<ExprResult> {
        let block = Ast::from_expr(expr.clone());

        let function = Container::new(Function::new_lambda(
            self.state.clone(),
            arguments.clone(),
            block,
        ));

        Ok(ExprResult::Function(function))
    }

    fn evaluate_function_def(
        &self,
        name: &str,
        arguments: &Params,
        body: &Ast,
        decorators: &[Expr],
        is_async: &bool,
        line_number: usize,
    ) -> TreewalkResult<()> {
        let function = Container::new(Function::new(
            self.state.clone(),
            name,
            arguments.clone(),
            body.clone(),
            decorators,
            *is_async,
            line_number,
        ));

        // Decorators are applied to a function when it is defined and then the decorated version
        // is written into the symbol table.
        let result = function.apply_decorators(self)?;

        // We should note that what we write here it not always a `Function` or even a `Callable`.
        // In the case of the `@property` decorator, what is written to the symbol table is a
        // `MemberDescriptor`.
        self.state.write(name, result);
        Ok(())
    }

    fn evaluate_class_definition(
        &self,
        name: &str,
        parents: &[Expr],
        metaclass: &Option<String>,
        body: &Ast,
    ) -> TreewalkResult<()> {
        log(LogLevel::Debug, || format!("Defining class: {}", name));
        let parent_classes = parents
            .iter()
            .map(|p| self.evaluate_expr(p))
            .collect::<Result<Vec<_>, _>>()?
            .iter()
            .map(|f| f.expect_class(self))
            .collect::<Result<Vec<_>, _>>()?;

        let metaclass = metaclass
            .clone()
            .and_then(|p| self.state.read(p.as_str()))
            .map(|d| d.expect_class(self))
            .transpose()?;

        // We will update the scope on this class before we write it to the symbol table, but we
        // must instantiate the class here so we can get a reference that can be associated with
        // each function defined inside it.
        let class = Class::new(self, name, parent_classes, metaclass)?;

        // We must use the class scope here in case it received any initialization from its
        // metaclass `Dunder::New` method.
        self.state
            .push_local(Container::new(class.borrow().scope.clone()));
        self.state.push_class(class.clone());
        self.evaluate_ast(body)?;
        self.state.pop_class();
        class.borrow_mut().scope = self
            .state
            .pop_local()
            .ok_or_else(|| self.runtime_error())?
            .borrow()
            .clone();

        self.state.write(name, ExprResult::Class(class));

        Ok(())
    }

    /// At most one of the Blocks will be evaluated, once we know which one we can return the
    /// result early.
    fn evaluate_if_else(
        &self,
        if_part: &ConditionalBlock,
        elif_parts: &[ConditionalBlock],
        else_part: &Option<Ast>,
    ) -> TreewalkResult<()> {
        let if_condition_result = self.evaluate_expr(&if_part.condition)?;
        if if_condition_result.as_boolean() {
            self.evaluate_ast(&if_part.block)?;
            return Ok(());
        }

        for elif_part in elif_parts {
            let elif_condition_result = self.evaluate_expr(&elif_part.condition)?;
            if elif_condition_result.as_boolean() {
                self.evaluate_ast(&elif_part.block)?;
                return Ok(());
            }
        }

        if let Some(else_part) = else_part {
            self.evaluate_ast(else_part)?;
            return Ok(());
        }

        Ok(())
    }

    fn evaluate_while_loop(&self, condition: &Expr, body: &Ast) -> TreewalkResult<()> {
        while self.evaluate_expr(condition)?.as_boolean() {
            match self.evaluate_ast(body) {
                Err(TreewalkDisruption::Signal(TreewalkSignal::Break)) => {
                    break;
                }
                Err(TreewalkDisruption::Signal(TreewalkSignal::Continue)) => {}
                Err(e) => return Err(e),
                _ => {}
            }
        }

        Ok(())
    }

    fn evaluate_generator_comprehension(
        &self,
        body: &Expr,
        clauses: &[ForClause],
    ) -> TreewalkResult<ExprResult> {
        let generator = Generator::new_from_comprehension(self.state.clone(), body, clauses);
        let iterator = GeneratorIterator::new(generator, self.clone());
        Ok(ExprResult::Generator(Container::new(iterator)))
    }

    fn evaluate_list_comprehension(
        &self,
        body: &Expr,
        clauses: &[ForClause],
    ) -> TreewalkResult<ExprResult> {
        if let Some((first_clause, remaining_clauses)) = clauses.split_first() {
            // Recursive case: Process the first ForClause
            let mut output = vec![];
            for i in self.evaluate_expr(&first_clause.iterable)? {
                if first_clause.indices.len() == 1 {
                    self.state.write(&first_clause.indices[0], i.clone());
                } else {
                    for (key, value) in first_clause.indices.iter().zip(i) {
                        self.state.write(key, value);
                    }
                }

                if let Some(condition) = first_clause.condition.as_ref() {
                    if !self.evaluate_expr(condition)?.as_boolean() {
                        continue;
                    }
                }

                // Recursively handle the rest of the clauses. If `remaining_clauses` is empty,
                // we'll hit the base case on the next call.
                match self.evaluate_list_comprehension(body, remaining_clauses)? {
                    ExprResult::List(list) => output.extend(list),
                    single => output.push(single),
                }
            }

            Ok(ExprResult::List(Container::new(List::new(output))))
        } else {
            // Base case: Evaluate the expression. We drop into this case when `clauses` is empty.
            self.evaluate_expr(body)
        }
    }

    fn evaluate_set_comprehension(
        &self,
        body: &Expr,
        clauses: &[ForClause],
    ) -> TreewalkResult<ExprResult> {
        self.evaluate_list_comprehension(body, clauses)?
            .try_into()
            .map_err(|_| self.type_error("Expected a set"))
            .map(ExprResult::Set)
    }

    fn evaluate_dict_comprehension(
        &self,
        clauses: &[ForClause],
        key_body: &Expr,
        value_body: &Expr,
    ) -> TreewalkResult<ExprResult> {
        let first_clause = match clauses {
            [first] => first,
            _ => unimplemented!(),
        };

        // TODO we are suppressing this clippy error for now because `ExprResult` allows interior
        // mutability which can lead to issues when used as a key for a `HashMap` or `HashSet`.
        #[allow(clippy::mutable_key_type)]
        let mut output = HashMap::new();
        for i in self.evaluate_expr(&first_clause.iterable)? {
            for (key, value) in first_clause.indices.iter().zip(i) {
                self.state.write(key, value);
            }
            let key_result = self.evaluate_expr(key_body)?;
            let value_result = self.evaluate_expr(value_body)?;
            output.insert(key_result, value_result);
        }
        Ok(ExprResult::Dict(Container::new(Dict::new(self, output))))
    }

    fn evaluate_for_in_loop(
        &self,
        index: &LoopIndex,
        range: &Expr,
        body: &Ast,
        else_block: &Option<Ast>,
    ) -> TreewalkResult<()> {
        let range_expr = self.evaluate_expr(range)?;
        let mut encountered_break = false;

        for val_for_iteration in range_expr {
            self.write_loop_index(index, val_for_iteration);

            match self.evaluate_ast(body) {
                Err(TreewalkDisruption::Signal(TreewalkSignal::Break)) => {
                    encountered_break = true;
                    break;
                }
                Err(TreewalkDisruption::Signal(TreewalkSignal::Continue)) => {}
                Err(e) => return Err(e),
                _ => {}
            }
        }

        if !encountered_break {
            if let Some(else_block) = else_block {
                self.evaluate_ast(else_block)?;
            }
        }

        Ok(())
    }

    fn evaluate_regular_import(&self, items: &[RegularImport]) -> TreewalkResult<()> {
        for item in items.iter() {
            self.evaluate_regular_import_inner(&item.import_path, &item.alias)?;
        }

        Ok(())
    }

    fn evaluate_regular_import_inner(
        &self,
        import_path: &ImportPath,
        alias: &Option<String>,
    ) -> TreewalkResult<()> {
        // A mutable ExprResult::Module that will be updated on each loop iteration
        let mut inner_module = self.evaluate_module_import(import_path)?;

        // This is a case where it's simpler if we have an alias: just make the module available
        // at the alias.
        if let Some(alias) = alias {
            self.state.write(alias, inner_module);
        } else {
            // Otherwise, we must create a module chain. For example:
            //
            // import mypackage.myothermodule
            //
            // must be used as
            //
            // mypackage.myothermodule.add('1', '1')

            // Iterate over the segments in reverse, skipping the last one
            let segments = import_path.segments();
            for segment in segments.iter().rev().take(segments.len() - 1) {
                let mut new_outer_module = Module::default();
                new_outer_module.insert(segment, inner_module);
                inner_module = ExprResult::Module(Container::new(new_outer_module));
            }

            self.state
                .write(import_path.segments().first().unwrap(), inner_module);
        }

        Ok(())
    }

    fn evaluate_selective_import(
        &self,
        import_path: &ImportPath,
        arguments: &[ImportedItem],
        wildcard: &bool,
    ) -> TreewalkResult<()> {
        let module = self
            .evaluate_module_import(import_path)?
            .as_module()
            .ok_or_else(|| self.import_error(import_path.as_str()))?;

        let mapped_imports = arguments
            .iter()
            .map(|arg| {
                let original = arg.as_original_symbol();
                let imported = arg.as_imported_symbol();

                (original, imported)
            })
            .collect::<HashMap<_, _>>();

        for module_symbol in module.dir() {
            let aliased_symbol = match wildcard {
                true => module_symbol.clone(),
                false => {
                    if !mapped_imports.contains_key(&module_symbol) {
                        continue;
                    }
                    mapped_imports[&module_symbol].clone()
                }
            };

            if let Some(value) = module.get_member(self, module_symbol.as_str())? {
                self.state.write(&aliased_symbol, value.clone());
            } else {
                return Err(self.name_error(aliased_symbol));
            }
        }

        Ok(())
    }

    fn evaluate_context_manager(
        &self,
        expr: &Expr,
        variable: &Option<String>,
        block: &Ast,
    ) -> TreewalkResult<()> {
        let expr_result = self.evaluate_expr(expr)?;
        let object = expr_result.expect_object(self)?;

        if object.get_member(self, &Dunder::Enter)?.is_none()
            || object.get_member(self, &Dunder::Exit)?.is_none()
        {
            return Err(self.error(ExecutionErrorKind::MissingContextManagerProtocol));
        }

        let result = self.invoke_method(expr_result.clone(), Dunder::Enter, &resolved_args![])?;

        if let Some(variable) = variable {
            self.state.write(variable, result);
        }
        let block_result = self.evaluate_ast(block);

        self.invoke_method(
            expr_result.clone(),
            Dunder::Exit,
            &resolved_args![ExprResult::None, ExprResult::None, ExprResult::None],
        )?;

        // Return the exception if one is called.
        block_result?;

        Ok(())
    }

    fn evaluate_raise(&self, instance: &Option<ExceptionInstance>) -> TreewalkResult<()> {
        // TODO we should throw a 'RuntimeError: No active exception to reraise'
        if instance.is_none() {
            return Err(TreewalkDisruption::Signal(TreewalkSignal::Raise));
        }

        let instance = instance.as_ref().unwrap();
        let args = ResolvedArguments::from(self, &instance.args)?;
        let error = match instance.literal {
            ExceptionLiteral::TypeError => {
                let message = if args.len() == 1 {
                    Some(args.get_arg(0).expect_string(self)?)
                } else {
                    None
                };

                self.type_error_optional_message(message)
            }
            _ => unimplemented!(),
        };

        Err(error)
    }

    fn evaluate_try_except(
        &self,
        try_block: &Ast,
        except_clauses: &[ExceptClause],
        else_block: &Option<Ast>,
        finally_block: &Option<Ast>,
    ) -> TreewalkResult<()> {
        if let Err(TreewalkDisruption::Error(error)) = self.evaluate_ast(try_block) {
            // Only the first matching clause should be evaluated. They will still be in order
            // here from the parsed code.
            if let Some(except_clause) = except_clauses
                .iter()
                .find(|clause| error.matches_except_clause(&clause.exception_types))
            {
                if let Some(alias) = &except_clause.alias {
                    self.state
                        .write(alias, ExprResult::Exception(error.clone()));
                }

                match self.evaluate_ast(&except_clause.block) {
                    Err(TreewalkDisruption::Signal(TreewalkSignal::Raise)) => {
                        return Err(TreewalkDisruption::Error(error))
                    }
                    Err(second_error) => return Err(second_error),
                    Ok(_) => {}
                }
            } else {
                // Uncaught errors should be raised
                return Err(TreewalkDisruption::Error(error));
            }
        } else if let Some(else_block) = else_block {
            // Else block is only evaluated if an error was not thrown
            self.evaluate_ast(else_block)?;
        }

        // Finally block is evaluated always if it exists
        if let Some(finally_block) = finally_block {
            self.evaluate_ast(finally_block)?;
        }

        Ok(())
    }

    /// TODO This should be moved to the semantic analysis
    fn validate_nonlocal_context(&self, name: &str) -> TreewalkResult<()> {
        // We could not find the variable `name` in an enclosing context.
        if let Some(env) = self.state.read_captured_env() {
            if env.borrow().read(name).is_none() {
                return Err(self.error(ExecutionErrorKind::SyntaxError));
            }
        }

        // `nonlocal` cannot be used at the module-level (outside of a function,
        // i.e. captured environment).
        if self.state.read_captured_env().is_none() {
            return Err(self.error(ExecutionErrorKind::SyntaxError));
        }

        Ok(())
    }

    fn evaluate_nonlocal(&self, names: &[Variable]) -> TreewalkResult<()> {
        for name in names {
            self.validate_nonlocal_context(name)?;
            self.state.mark_nonlocal(name);
        }

        Ok(())
    }

    fn evaluate_global(&self, names: &[Variable]) -> TreewalkResult<()> {
        for name in names {
            self.state.mark_global(name);
        }

        Ok(())
    }

    fn evaluate_type_node(&self, type_node: &TypeNode) -> TreewalkResult<ExprResult> {
        Ok(ExprResult::TypeNode(type_node.into()))
    }

    pub fn evaluate_expr(&self, expr: &Expr) -> TreewalkResult<ExprResult> {
        match expr {
            Expr::None => Ok(ExprResult::None),
            Expr::Ellipsis => Ok(ExprResult::Ellipsis),
            Expr::NotImplemented => Ok(ExprResult::NotImplemented),
            Expr::Integer(value) => Ok(ExprResult::Integer(*value)),
            Expr::FloatingPoint(value) => Ok(ExprResult::FloatingPoint(*value)),
            Expr::Boolean(value) => Ok(ExprResult::Boolean(*value)),
            Expr::StringLiteral(value) => Ok(ExprResult::String(Str::new(value.clone()))),
            Expr::ByteStringLiteral(value) => Ok(ExprResult::Bytes(value.clone())),
            Expr::Variable(name) => self.state.read_or_disrupt(name, self),
            Expr::List(items) => self.evaluate_list(items),
            Expr::Set(items) => self.evaluate_set(items),
            Expr::Dict(dict_ops) => self.evaluate_dict(dict_ops),
            Expr::Tuple(items) => self.evaluate_tuple(items),
            Expr::GeneratorComprehension { body, clauses } => {
                self.evaluate_generator_comprehension(body, clauses)
            }
            Expr::ListComprehension { body, clauses } => {
                self.evaluate_list_comprehension(body, clauses)
            }
            Expr::SetComprehension { body, clauses } => {
                self.evaluate_set_comprehension(body, clauses)
            }
            Expr::DictComprehension {
                clauses,
                key_body,
                value_body,
            } => self.evaluate_dict_comprehension(clauses, key_body, value_body),
            Expr::UnaryOperation { op, right } => self.evaluate_unary_operation(op, right),
            Expr::BinaryOperation { left, op, right } => {
                self.evaluate_binary_operation(left, op, right)
            }
            Expr::Await(right) => self.evaluate_await(right),
            Expr::FunctionCall { name, args, callee } => {
                self.evaluate_function_call(name, args, callee)
            }
            Expr::ClassInstantiation { name, args } => {
                self.evaluate_class_instantiation(name, args)
            }
            Expr::LogicalOperation { left, op, right } => {
                self.evaluate_logical_operation(left, op, right)
            }
            Expr::TernaryOp {
                condition,
                if_value,
                else_value,
            } => self.evaluate_ternary_operation(condition, if_value, else_value),
            Expr::MethodCall { object, name, args } => {
                self.evaluate_method_call(object, name, args)
            }
            Expr::MemberAccess { object, field } => self.evaluate_member_access(object, field),
            Expr::IndexAccess { object, index } => self.evaluate_index_access(object, index),
            Expr::SliceOperation { object, params } => {
                self.evaluate_slice_operation(object, params)
            }
            Expr::FString(parts) => self.evaluate_f_string(parts),
            Expr::Lambda { args, expr } => self.evaluate_lambda(args, expr),
            Expr::TypeNode(type_node) => self.evaluate_type_node(type_node),
            // This is unreachable because it should be handled inside `GeneratorExecutor`.
            Expr::Yield(_) | Expr::YieldFrom(_) => unreachable!(),
        }
    }

    pub fn evaluate_statement(&self, stmt: &Statement) -> TreewalkResult<ExprResult> {
        self.state.set_line_number(stmt.start_line);

        // These are the only types of statements that will return a value.
        match &stmt.kind {
            StatementKind::Expression(expr) => return self.evaluate_expr(expr),
            StatementKind::Return(expr) => return self.evaluate_return(expr),
            _ => {}
        };

        let result = match &stmt.kind {
            // These are handled above
            StatementKind::Expression(_) | StatementKind::Return(_) => unreachable!(),
            StatementKind::Pass => Ok(()),
            StatementKind::Break => Err(TreewalkDisruption::Signal(TreewalkSignal::Break)),
            StatementKind::Continue => Err(TreewalkDisruption::Signal(TreewalkSignal::Continue)),
            StatementKind::Assert(expr) => self.evaluate_assert(expr),
            StatementKind::Delete(expr) => self.evaluate_delete(expr),
            StatementKind::Nonlocal(names) => self.evaluate_nonlocal(names),
            StatementKind::Global(names) => self.evaluate_global(names),
            StatementKind::Assignment { left, right } => self.evaluate_assignment(left, right),
            StatementKind::MultipleAssignment { left, right } => {
                self.evaluate_multiple_assignment(left, right)
            }
            StatementKind::UnpackingAssignment { left, right } => {
                self.evaluate_unpacking_assignment(left, right)
            }
            StatementKind::CompoundAssignment {
                operator,
                target,
                value,
            } => self.evaluate_compound_assignment(operator, target, value),
            StatementKind::FunctionDef {
                name,
                args,
                body,
                decorators,
                is_async,
            } => {
                self.evaluate_function_def(name, args, body, decorators, is_async, stmt.start_line)
            }
            StatementKind::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => self.evaluate_if_else(if_part, elif_parts, else_part),
            StatementKind::WhileLoop { condition, body } => {
                self.evaluate_while_loop(condition, body)
            }
            StatementKind::ForInLoop {
                index,
                iterable: range,
                body,
                else_block,
            } => self.evaluate_for_in_loop(index, range, body, else_block),
            StatementKind::ClassDef {
                name,
                parents,
                metaclass,
                body,
            } => self.evaluate_class_definition(name, parents, metaclass, body),
            StatementKind::RegularImport(items) => self.evaluate_regular_import(items),
            StatementKind::SelectiveImport {
                import_path,
                items,
                wildcard,
            } => self.evaluate_selective_import(import_path, items, wildcard),
            StatementKind::TryExcept {
                try_block,
                except_clauses,
                else_block,
                finally_block,
            } => self.evaluate_try_except(try_block, except_clauses, else_block, finally_block),
            StatementKind::Raise(exception) => self.evaluate_raise(exception),
            StatementKind::ContextManager {
                expr,
                variable,
                block,
            } => self.evaluate_context_manager(expr, variable, block),
        };

        // Return an error if one is thrown, otherwise all statements will return None.
        result?;

        Ok(ExprResult::None)
    }
}

impl InterpreterEntrypoint for Interpreter {
    type Return = ExprResult;

    fn run(&mut self, parser: &mut Parser) -> Result<Self::Return, MemphisError> {
        let mut result = ExprResult::None;
        while !parser.is_finished() {
            let stmt = parser.parse_statement().map_err(MemphisError::Parser)?;
            result = match self.evaluate_statement(&stmt) {
                Ok(result) => result,
                Err(TreewalkDisruption::Error(e)) => return Err(MemphisError::Execution(e)),
                Err(TreewalkDisruption::Signal(_)) => todo!(),
            }
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast,
        domain::test_utils,
        init::MemphisContext,
        treewalk::types::{
            domain::Type, ByteArray, Complex, DictItems, DictKeys, DictValues, FrozenSet,
        },
    };

    fn init_path(path: &str) -> MemphisContext {
        MemphisContext::from_path(path)
    }

    fn init(text: &str) -> MemphisContext {
        MemphisContext::from_text(text)
    }

    fn eval_inner(text: &str) -> Result<ExprResult, MemphisError> {
        init(text).evaluate()
    }

    fn evaluate(text: &str) -> ExprResult {
        eval_inner(text).expect("Failed to evaluate test string!")
    }

    fn eval_expect_error(text: &str) -> ExecutionError {
        match eval_inner(text) {
            Ok(_) => panic!("Expected an error!"),
            Err(MemphisError::Execution(e)) => return e,
            Err(_) => panic!("Expected an execution error!"),
        };
    }

    /// Run the treewalk interpreter to completion and return a reference to the [`Interpreter`].
    fn run<'a>(context: &'a mut MemphisContext) -> &'a Interpreter {
        context.evaluate().expect("Treewalk evaluation failed!");
        context.ensure_treewalk()
    }

    fn run_expect_error(context: &mut MemphisContext) -> ExecutionError {
        match context.evaluate() {
            Ok(_) => panic!("Expected an error!"),
            Err(MemphisError::Execution(e)) => return e,
            Err(_) => panic!("Expected an execution error!"),
        };
    }

    fn stmt(kind: StatementKind) -> Statement {
        Statement::new(1, kind)
    }

    fn read_optional(interpreter: &Interpreter, name: &str) -> Option<ExprResult> {
        interpreter.state.read(name)
    }

    fn read(interpreter: &Interpreter, name: &str) -> ExprResult {
        read_optional(interpreter, name).expect("Failed to read var")
    }

    fn read_type(interpreter: &Interpreter, name: &str) -> Type {
        read(interpreter, name).get_type()
    }

    fn assert_type_class(interpreter: &Interpreter, name: &str, type_: Type) {
        assert_eq!(
            read(interpreter, name).as_class().unwrap(),
            interpreter.state.get_type_class(type_)
        );
    }

    macro_rules! assert_type_is {
        ($interp:expr, $input:expr, $pattern:ident) => {
            assert!(matches!(read($interp, $input), ExprResult::$pattern(_)));
        };
    }

    macro_rules! assert_member_eq {
        ($interp:expr, $input:expr, $field:expr, $expected:expr) => {
            assert_eq!(extract_member!($interp, $input, $field), $expected);
        };
    }

    macro_rules! extract {
        ($interp:expr, $input:expr, $variant:ident) => {{
            match read($interp, $input) {
                ExprResult::$variant(v) => v,
                _ => panic!("Expected {}: {}", stringify!($variant), $input),
            }
        }};
    }

    macro_rules! extract_member {
        ($interp:expr, $input:expr, $field:expr) => {
            extract!($interp, $input, Object)
                .get_member($interp, $field)
                .expect(&format!("Failed to get field: {}", $field))
                .expect(&format!("Failed to get field: {}", $field))
        };
        ($interp:expr, $input:expr, $field:expr, $pattern:ident) => {
            extract!($interp, $input, $pattern)
                .get_member($interp, $field)
                .expect(&format!("Failed to get field: {}", $field))
                .expect(&format!("Failed to get field: {}", $field))
        };
    }

    macro_rules! str {
        ($input:expr) => {
            ExprResult::String(Str::new($input.to_string()))
        };
    }

    macro_rules! int {
        ($val:expr) => {
            ExprResult::Integer($val)
        };
    }

    macro_rules! float {
        ($val:expr) => {
            ExprResult::FloatingPoint($val)
        };
    }

    macro_rules! complex {
        ($real:expr, $imag:expr) => {
            ExprResult::Complex(Complex::new($real, $imag))
        };
    }

    macro_rules! bool {
        ($val:expr) => {
            ExprResult::Boolean($val)
        };
    }

    macro_rules! list {
        ($($expr:expr),* $(,)?) => {
            ExprResult::List(Container::new(List::new(vec![
                $($expr),*
            ])))
        };
    }

    macro_rules! tuple {
        ($($expr:expr),* $(,)?) => {
            ExprResult::Tuple(Tuple::new(vec![
                $($expr),*
            ]))
        };
    }

    macro_rules! set {
        ($($expr:expr),* $(,)?) => {
            ExprResult::Set(Container::new(Set::new(HashSet::from([
                $($expr),*
            ]))))
        };
    }

    macro_rules! dict {
        ($interp:expr, { $($key:expr => $value:expr),* $(,)? }) => {
            ExprResult::Dict(Container::new(Dict::new(
                $interp,
                vec![
                    $(($key, $value)),*
                ].into_iter().collect()
            )))
        };
    }

    #[test]
    fn undefined_variable() {
        let input = "x + 1";
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_name_error(&e, "x");
    }

    #[test]
    fn division_by_zero() {
        let input = "1 / 0";
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(
            &e,
            ExecutionErrorKind::DivisionByZero("integer division or modulo by zero".to_string()),
        );
    }

    #[test]
    fn expression() {
        let input = "2 + 3 * (4 - 1)";
        assert_eq!(evaluate(input), int!(11));
    }

    #[test]
    fn integer_division() {
        let input = "2 // 3";
        assert_eq!(evaluate(input), int!(0));

        let input = "5 // 3";
        assert_eq!(evaluate(input), int!(1));

        let input = "5 // 0";
        let e = eval_expect_error(input);
        test_utils::assert_error_kind(
            &e,
            ExecutionErrorKind::DivisionByZero("integer division or modulo by zero".to_string()),
        );
    }

    #[test]
    fn integer_assignment() {
        let input = r#"
a = 2 + 3 * 4
b = a + 5
c = None
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(14));
        assert_eq!(read(interpreter, "b"), int!(19));
        assert_eq!(read(interpreter, "c"), ExprResult::None);
    }

    #[test]
    fn strings() {
        let input = r#"
a = "foo"
# TODO Python shows <class 'method_descriptor'>
b = type(str.join)
# TODO Python shows <class 'builtin_function_or_method'>
c = type(a.join)
d = type(str.maketrans)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), str!("foo"));
        assert_type_class(interpreter, "b", Type::BuiltinMethod);
        assert_type_class(interpreter, "c", Type::Method);
        assert_type_class(interpreter, "d", Type::BuiltinMethod);
    }

    #[test]
    fn boolean_operators() {
        let input = "True and False";
        assert_eq!(evaluate(input), bool!(false));

        let input = "True or False";
        assert_eq!(evaluate(input), bool!(true));

        let input = "not (True or False)";
        assert_eq!(evaluate(input), bool!(false));

        let input = "True and not False";
        assert_eq!(evaluate(input), bool!(true));

        let input = "not False";
        assert_eq!(evaluate(input), bool!(true));

        let input = "not True";
        assert_eq!(evaluate(input), bool!(false));
    }

    // Confirm that the interpreter can evaluate boolean expressions.
    #[test]
    fn comparison_operators() {
        let input = "2 == 1";
        assert_eq!(evaluate(input), bool!(false));

        let input = "2 == 2";
        assert_eq!(evaluate(input), bool!(true));

        let input = "2 != 1";
        assert_eq!(evaluate(input), bool!(true));

        let input = "2 != 2";
        assert_eq!(evaluate(input), bool!(false));

        let input = "2 > 1";
        assert_eq!(evaluate(input), bool!(true));

        let input = "2 < 1";
        assert_eq!(evaluate(input), bool!(false));

        let input = "1 <= 1";
        assert_eq!(evaluate(input), bool!(true));

        let input = "1 >= 1";
        assert_eq!(evaluate(input), bool!(true));

        let input = "4 in range(5)";
        assert_eq!(evaluate(input), bool!(true));

        let input = "4 in range(3)";
        assert_eq!(evaluate(input), bool!(false));

        let input = "4 not in range(5)";
        assert_eq!(evaluate(input), bool!(false));

        let input = "4 not in range(3)";
        assert_eq!(evaluate(input), bool!(true));

        let input = "4 is None";
        assert_eq!(evaluate(input), bool!(false));

        let input = "4 is not None";
        assert_eq!(evaluate(input), bool!(true));
    }

    #[test]
    fn print_builtin() {
        // this test has no assertions because output capture only works in the integration tests
        // and not the unit tests at the moment.
        let input = r#"
print(3)
a = type(print)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::BuiltinFunction);
    }

    #[test]
    fn string_literal() {
        let input = r#"
print("Hello, World!")

a = iter("")
b = type(iter(""))

for i in iter("abcde"):
    print(i)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", StringIterator);
        assert_type_class(interpreter, "b", Type::StringIterator);
    }

    #[test]
    fn function_definition() {
        let input = r#"
def foo(a, b):
    return a + b

foo(2, 3)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "foo", Function);

        let input = r#"
def add(x, y):
    return x + y

a = add(2, 3)

b = lambda: 4
c = b()

d = lambda: (yield)
e = d()

f = type((lambda: (yield))())

async def g(): pass
h = g()
i = type(h)
h.close()

async def j(): yield
k = g()
l = type(h)

def _f(): pass
m = _f.__code__
n = type(_f.__code__)
o = type(type(_f).__code__)
p = type(_f.__globals__)
q = type(type(_f).__globals__)
r = type(_f.__closure__)
s = type(type(_f).__closure__)

t = _f.__module__
u = _f.__doc__
v = _f.__name__
w = _f.__qualname__
x = _f.__annotations__
y = _f.__type_params__
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));
        let b = interpreter
            .state
            .read("b")
            .unwrap()
            .as_function()
            .unwrap()
            .borrow()
            .clone();
        let Function { ref body, .. } = b;
        assert_eq!(
            body,
            &Ast::new(vec![stmt(StatementKind::Expression(Expr::Integer(4)))])
        );
        assert_eq!(read(interpreter, "c"), int!(4));
        let d = interpreter
            .state
            .read("d")
            .unwrap()
            .as_function()
            .unwrap()
            .borrow()
            .clone();
        let Function { ref body, .. } = d;
        assert_eq!(
            body,
            &ast![stmt(StatementKind::Expression(Expr::Yield(None)))]
        );
        assert_type_is!(interpreter, "e", Generator);
        assert_type_class(interpreter, "f", Type::Generator);
        assert_type_is!(interpreter, "h", Coroutine);
        // I commented this out when we removed Clone from Class.
        //assert!(matches!(
        //    read(interpreter, "i").as_class().unwrap().borrow(),
        //    Class { name, .. } if name == "coroutine"
        //));
        // TODO add support for async generators, which will change the next two assertions
        assert_type_is!(interpreter, "k", Coroutine);
        //assert!(matches!(
        //    read_and_expect(interpreter, "l").as_class().unwrap().borrow().clone(),
        //    Class { name, .. } if name == "coroutine"
        //));
        assert_type_is!(interpreter, "m", Code);
        assert_type_class(interpreter, "n", Type::Code);
        assert_type_class(interpreter, "o", Type::GetSetDescriptor);
        assert_type_class(interpreter, "p", Type::Dict);
        assert_type_class(interpreter, "q", Type::MemberDescriptor);
        assert_type_class(interpreter, "r", Type::None);
        assert_type_class(interpreter, "s", Type::MemberDescriptor);
        assert_eq!(read(interpreter, "t"), str!("__main__"));
        assert_eq!(read(interpreter, "u"), str!(""));
        assert_eq!(read(interpreter, "v"), str!("_f"));
        assert_eq!(read(interpreter, "w"), str!("_f"));
        assert_eq!(read(interpreter, "x"), dict!(interpreter, {}));
        assert_eq!(read(interpreter, "y"), tuple![]);

        // Test early return
        let input = r#"
def foo():
    return 4
    return 5

a = foo()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(4));
    }

    #[test]
    fn if_else() {
        let input = r#"
z = "Empty"
y = 5
if y > 0:
    z = "Greater than 0"
elif y > -10:
    z = "Greater than -10"
elif y > -20:
    z = "Greater than -20"
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), str!("Greater than 0"));

        let input = r#"
z = "Empty"
y = -5
if y > 0:
    z = "Greater than 0"
elif y > -10:
    z = "Greater than -10"
elif y > -20:
    z = "Greater than -20"
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), str!("Greater than -10"));

        let input = r#"
z = "Empty"
y = -15
if y > 0:
    z = "Greater than 0"
elif y > -10:
    z = "Greater than -10"
elif y > -20:
    z = "Greater than -20"
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), str!("Greater than -20"));

        let input = r#"
z = "Empty"
y = -25
if y > 0:
    z = "Greater than 0"
elif y > -10:
    z = "Greater than -10"
elif y > -20:
    z = "Greater than -20"
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), str!("Empty"));

        let input = r#"
z = "Empty"
y = -25
if y > 0:
    z = "Greater than 0"
elif y > -10:
    z = "Greater than -10"
elif y > -20:
    z = "Greater than -20"
else:
    z = "Else"
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), str!("Else"));

        let input = r#"
z = 0
if 4 in range(5):
    z = 1
else:
    z = 2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), int!(1));
    }

    #[test]
    fn while_loop() {
        let input = r#"
z = 0
while z < 10:
    z = z + 1
    print("done")
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), int!(10));
    }

    #[test]
    fn class_definition() {
        let input = r#"
class Foo:
    def __init__(self):
        self.x = 0

    def bar(self):
        print(self.x)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert!(interpreter.state.is_class("Foo"));
    }

    #[test]
    fn class_instantiation() {
        let input = r#"
class Foo:
    def __init__(self, y):
        self.x = 0
        self.y = y

foo = Foo(3)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert!(interpreter.state.is_class("Foo"));
        assert!(!interpreter.state.is_class("foo"));

        assert_member_eq!(interpreter, "foo", "y", int!(3));
        assert_member_eq!(interpreter, "foo", "x", int!(0));

        let input = r#"
class Foo:
    def __init__(self, y):
        self.x = 0
        self.y = y
        a = 4

    def bar(self):
        print(self.x)

foo = Foo(3)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert!(interpreter.state.is_class("Foo"));
        assert!(!interpreter.state.is_class("foo"));

        // This should be an object with foo.y == 3 and foo.x == 0 even
        // when the last line of the constructor did not touch self.
        assert_member_eq!(interpreter, "foo", "y", int!(3));
        assert_member_eq!(interpreter, "foo", "x", int!(0));
    }

    #[test]
    fn method_invocation() {
        let input = r#"
class Foo:
    def __init__(self, y):
        self.x = 0
        self.y = y

    def bar(self):
        print(self.bash())
        return self.y

    def bash(self):
        return self.x

foo = Foo(3)
x = foo.bar()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "x"), int!(3));

        // Try the same test but with no constructor
        let input = r#"
class Foo:
    def bar(self):
        self.x = 0
        self.y = 3
        print(self.x)

foo = Foo()
foo.bar()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert!(interpreter.state.is_class("Foo"));
        assert!(!interpreter.state.is_class("foo"));

        // These should be set even when it's not a constructor
        assert_member_eq!(interpreter, "foo", "y", int!(3));
        assert_member_eq!(interpreter, "foo", "x", int!(0));
    }

    #[test]
    fn regular_import() {
        let mut context = init_path("src/fixtures/imports/regular_import.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "x"), int!(5));
        assert_eq!(read(interpreter, "y"), int!(6));
        // This previously returned [`Type::Method`], which was an issue with binding
        // classes (as callables) to their module.
        assert_eq!(read(interpreter, "z").get_type(), Type::Type);

        let mut context = init_path("src/fixtures/imports/regular_import_b.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "y"), int!(7));
    }

    #[test]
    fn regular_import_relative() {
        let mut context = init_path("src/fixtures/imports/relative/main_b.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "x"), int!(2));

        let mut context = init_path("src/fixtures/imports/relative/main_c.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "x"), int!(2));
    }

    #[test]
    fn selective_import() {
        let mut context = init_path("src/fixtures/imports/selective_import_a.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "x"), int!(5));

        let mut context = init_path("src/fixtures/imports/selective_import_b.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "y"), int!(6));
        assert_eq!(read(interpreter, "z"), int!(6));

        let mut context = init_path("src/fixtures/imports/selective_import_c.py");
        let e = run_expect_error(&mut context);

        test_utils::assert_name_error(&e, "something_third");

        let mut context = init_path("src/fixtures/imports/selective_import_d.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), int!(8));

        let mut context = init_path("src/fixtures/imports/selective_import_e.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "x"), int!(8));

        let mut context = init_path("src/fixtures/imports/selective_import_f.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "y"), int!(6));
        assert_eq!(read(interpreter, "z"), int!(6));
    }

    #[test]
    fn selective_import_relative() {
        let mut context = init_path("src/fixtures/imports/relative/main_a.py");
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "x"), int!(2));
    }

    #[test]
    fn floating_point() {
        let input = r#"
a = 3.14
b = a + 2.5e-3
c = 4 + 2.1
d = 1.9 + 4
e = d == 5.9
f = d != 5.9
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), float!(3.14));
        assert_eq!(read(interpreter, "b"), float!(3.1425));
        assert_eq!(read(interpreter, "c"), float!(6.1));
        assert_eq!(read(interpreter, "d"), float!(5.9));
        assert_eq!(read(interpreter, "e"), bool!(true));
        assert_eq!(read(interpreter, "f"), bool!(false));

        let input = r#"
def add(x, y):
    return x + y

z = add(2.1, 3)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), float!(5.1));
    }

    #[test]
    fn negative_numbers() {
        let input = r#"
a = -3.14
b = -3
c = 2 - 3
d = -2e-3
e = 2 + -3
f = 2+-3
g = -(3)
h = -(2+3)
i = +3
j = +(-3)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), float!(-3.14));
        assert_eq!(read(interpreter, "b"), int!(-3));
        assert_eq!(read(interpreter, "c"), int!(-1));
        assert_eq!(read(interpreter, "d"), float!(-2e-3));
        assert_eq!(read(interpreter, "e"), int!(-1));
        assert_eq!(read(interpreter, "f"), int!(-1));
        assert_eq!(read(interpreter, "g"), int!(-3));
        assert_eq!(read(interpreter, "h"), int!(-5));
        assert_eq!(read(interpreter, "i"), int!(3));
        assert_eq!(read(interpreter, "j"), int!(-3));
    }

    #[test]
    fn call_stack() {
        let mut context = init_path("src/fixtures/call_stack/call_stack.py");
        let e = run_expect_error(&mut context);

        let call_stack = context.ensure_treewalk().state.debug_call_stack();
        test_utils::assert_name_error(&e, "unknown");

        assert_eq!(call_stack.len(), 3);
        assert!(call_stack
            .get(0)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack.py"));
        assert!(call_stack
            .get(1)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/other.py"));
        assert!(call_stack
            .get(2)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/other.py"));

        assert_eq!(call_stack.get(0).name(), "<module>");
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert_eq!(call_stack.get(0).line_number(), 2);
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).line_number(), 5);

        let mut context = init_path("src/fixtures/call_stack/call_stack_one_file.py");
        let e = run_expect_error(&mut context);

        let call_stack = context.ensure_treewalk().state.debug_call_stack();
        test_utils::assert_name_error(&e, "unknown");

        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert_eq!(call_stack.get(0).line_number(), 7);
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).line_number(), 5);
        assert!(call_stack
            .get(0)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(0).file_path_str().starts_with("/"));
        assert!(call_stack
            .get(1)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(1).file_path_str().starts_with("/"));
        assert!(call_stack
            .get(2)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(2).file_path_str().starts_with("/"));

        let input = r#"
"""
multiline string
we want to test that this is included in the line number in the call stack


more space here
"""
a = 4
b = 10
c = foo()
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        let call_stack = context.ensure_treewalk().state.debug_call_stack();
        test_utils::assert_name_error(&e, "foo");

        assert_eq!(call_stack.len(), 1);
        assert_eq!(call_stack.get(0).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert_eq!(call_stack.get(0).line_number(), 11);
    }

    #[test]
    fn lists() {
        let input = r#"
a = [1,2,3]
print(a)
b = [1,2.1]
c = list([1,2])
d = list({1,2})
e = list((1,2))
f = list(range(2))
g = [
    1,
    2,
]
h = c + g
i = []
j = iter([])
k = type(iter([]))

l = len([1])
m = len([1,2,3,4,5])
n = len([])

o = [].append
p = type([].append)
q = [1,2]
q.append(3)

r = [3,4]
s = r.append
s(5)

t = [1,2]
t.extend([3,4])
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), list![int!(1), int!(2), int!(3),]);
        assert_eq!(
            read(interpreter, "b"),
            list![int!(1), ExprResult::FloatingPoint(2.1)]
        );
        assert_eq!(read(interpreter, "c"), list![int!(1), int!(2)]);
        assert_eq!(read(interpreter, "d"), list![int!(1), int!(2)]);
        assert_eq!(read(interpreter, "e"), list![int!(1), int!(2)]);
        assert_eq!(read(interpreter, "f"), list![int!(0), int!(1)]);
        assert_eq!(read(interpreter, "g"), list![int!(1), int!(2)]);
        assert_eq!(
            read(interpreter, "h"),
            list![int!(1), int!(2), int!(1), int!(2)]
        );
        assert_eq!(read(interpreter, "i"), list![]);
        assert_type_is!(interpreter, "j", ListIterator);
        assert_type_class(interpreter, "k", Type::ListIterator);
        assert_eq!(read(interpreter, "l"), int!(1));
        assert_eq!(read(interpreter, "m"), int!(5));
        assert_eq!(read(interpreter, "n"), int!(0));
        assert_type_is!(interpreter, "o", Method);
        assert_type_class(interpreter, "p", Type::Method);
        assert_eq!(read(interpreter, "q"), list![int!(1), int!(2), int!(3),]);
        assert_type_is!(interpreter, "s", Method);
        assert_eq!(read(interpreter, "r"), list![int!(3), int!(4), int!(5),]);
        assert_eq!(
            read(interpreter, "t"),
            list![int!(1), int!(2), int!(3), int!(4)]
        );

        let input = "list([1,2,3], [1,2])";
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "Found 3 args");
    }

    #[test]
    fn sets() {
        let input = r#"
a = {1,2,3}
print(a)
b = {1,2.1}
c = set({1,2})
d = {1,2,2,1}
e = set([1,2])
f = set((1,2))
g = set(range(2))
h = set()
i = iter(set())
j = type(iter(set()))

new_set = set()
new_set.add("five")

k = {1} <= {1,2}
l = {1} <= {2}
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), set![int!(1), int!(2), int!(3),]);
        assert_eq!(
            read(interpreter, "b"),
            set![ExprResult::FloatingPoint(2.1), int!(1),]
        );
        assert_eq!(read(interpreter, "c"), set![int!(1), int!(2),]);
        assert_eq!(read(interpreter, "d"), set![int!(1), int!(2),]);
        assert_eq!(read(interpreter, "e"), set![int!(1), int!(2),]);
        assert_eq!(read(interpreter, "f"), set![int!(1), int!(2),]);
        assert_eq!(read(interpreter, "g"), set![int!(0), int!(1),]);
        assert_eq!(read(interpreter, "h"), set![]);
        assert_type_is!(interpreter, "i", SetIterator);
        assert_type_class(interpreter, "j", Type::SetIterator);
        assert_eq!(read(interpreter, "new_set"), set![str!("five")]);
        assert_eq!(read(interpreter, "k"), bool!(true));
        assert_eq!(read(interpreter, "l"), bool!(false));

        let input = "set({1,2,3}, {1,2})";
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "Found 3 args");
    }

    #[test]
    fn tuples() {
        let input = r#"
a = (1,2,3)
print(a)
b = (1,2.1)
c = tuple([1,2])
d = tuple({1,2})
e = tuple((1,2))
f = tuple(range(2))
g = iter(())
h = type(iter(()))
i = (4,)
j = 9, 10
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), tuple![int!(1), int!(2), int!(3),]);
        assert_eq!(
            read(interpreter, "b"),
            tuple![int!(1), ExprResult::FloatingPoint(2.1)]
        );
        assert_eq!(read(interpreter, "c"), tuple![int!(1), int!(2)]);
        assert_eq!(read(interpreter, "d"), tuple![int!(1), int!(2)]);
        assert_eq!(read(interpreter, "e"), tuple![int!(1), int!(2)]);
        assert_eq!(read(interpreter, "f"), tuple![int!(0), int!(1)]);
        assert_type_is!(interpreter, "g", TupleIterator);
        assert_type_class(interpreter, "h", Type::TupleIterator);
        assert_eq!(read(interpreter, "i"), tuple![int!(4),]);
        assert_eq!(read(interpreter, "j"), tuple![int!(9), int!(10),]);

        let input = "tuple([1,2,3], [1,2])";
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "Found 3 args");
    }

    #[test]
    fn index_access() {
        let input = r#"
a = [1,2,3]
b = a[0]
c = [1,2,3][1]
a[0] = 10

d = (1,2,3)
e = d[0]
f = (1,2,3)[1]
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), list![int!(10), int!(2), int!(3),]);
        assert_eq!(read(interpreter, "b"), int!(1));
        assert_eq!(read(interpreter, "c"), int!(2));
        assert_eq!(read(interpreter, "e"), int!(1));
        assert_eq!(read(interpreter, "f"), int!(2));

        let input = r#"
d = (1,2,3)
d[0] = 10
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "'tuple' object does not support item assignment");

        let input = r#"
d = (1,2,3)
del d[0]
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "'tuple' object does not support item deletion");

        let input = r#"
4[1]
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "'int' object is not subscriptable");
    }

    #[test]
    fn for_in_loop() {
        let input = r#"
a = [2,4,6,8]
b = 0
c = True
for i in a:
    b = b + i
    c = False
    print(b)
print(b)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(20));
        assert_eq!(read(interpreter, "i"), int!(8));
        assert_eq!(read(interpreter, "c"), bool!(false));

        let input = r#"
a = {2,4,6,8}
b = 0
c = True
for i in a:
    b = b + i
    c = False
    print(b)
print(b)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(20));
        assert_eq!(read(interpreter, "i"), int!(8));
        assert_eq!(read(interpreter, "c"), bool!(false));

        let input = r#"
a = (2,4,6,8)
b = 0
c = True
for i in a:
    b = b + i
    c = False
    print(b)
print(b)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(20));
        assert_eq!(read(interpreter, "i"), int!(8));
        assert_eq!(read(interpreter, "c"), bool!(false));

        let input = r#"
b = 0
for i in range(5):
    b = b + i
print(b)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(10));

        let input = r#"
a = {"a": 1,"b": 2}
b = 0
for k, v in a.items():
    b = b + v
print(b)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(3));
    }

    #[test]
    fn ranges() {
        let input = r#"
a = iter(range(0))
b = type(iter(range(0)))
c = type(range(0))
d = 0
for i in range(3):
    d += i

e = 0
r = range(3)
for i in r:
    e += i
for i in r:
    e += i
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", RangeIterator);
        assert_type_class(interpreter, "b", Type::RangeIterator);
        assert_type_class(interpreter, "c", Type::Range);
        assert_eq!(read(interpreter, "d"), int!(3));
        assert_eq!(read(interpreter, "e"), int!(6));
    }

    #[test]
    fn type_builtin() {
        let input = r#"
class Foo:
    def __init__(self):
        self.x = 0

a = Foo()
b = type(a)
c = b()

d = [1,2,3]
e = type(d)
f = e([1,2,3])
g = e({4,5})

h = {1,2}
i = type(h)
j = i({6,7})

k = (1,2)
l = type(k)
m = l((8,9))

n = {"a": 1,"b": 2}
o = type(n)
p = o({"c": 3,"d": 4})

q = type(None)
r = type(Ellipsis)
s = type(NotImplemented)

t = type(slice)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(
            interpreter
                .state
                .read("b")
                .unwrap()
                .as_class()
                .unwrap()
                .borrow()
                .name(),
            "Foo"
        );
        assert_eq!(
            interpreter
                .state
                .read("c")
                .unwrap()
                .as_object()
                .unwrap()
                .borrow()
                .class
                .borrow()
                .name(),
            "Foo"
        );
        assert_eq!(read(interpreter, "f"), list![int!(1), int!(2), int!(3),]);
        assert_eq!(read(interpreter, "g"), list![int!(4), int!(5),]);
        assert_eq!(read(interpreter, "j"), set![int!(6), int!(7),]);
        assert_eq!(read(interpreter, "m"), tuple![int!(8), int!(9),]);
        assert_eq!(
            read(interpreter, "p"),
            dict!(interpreter, { str!("c") => int!(3), str!("d") => int!(4) })
        );
        assert_type_class(interpreter, "q", Type::None);
        assert_type_class(interpreter, "r", Type::Ellipsis);
        assert_type_class(interpreter, "s", Type::NotImplemented);
        assert_type_class(interpreter, "t", Type::Type);
    }

    #[test]
    fn list_comprehension() {
        let input = r#"
a = [1,2,3]
b = [ i * 2 for i in a ]
c = [ i * 2 for i in a if False ]
d = [ j * 2 for j in a if j > 2 ]
e = [x * y for x in range(1,3) for y in range(1,3)]
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), list![int!(2), int!(4), int!(6),]);
        assert_eq!(read(interpreter, "c"), list![]);
        assert_eq!(read(interpreter, "d"), list![int!(6),]);
        assert_eq!(
            read(interpreter, "e"),
            list![int!(1), int!(2), int!(2), int!(4),]
        );
    }

    #[test]
    fn set_comprehension() {
        let input = r#"
a = [1,2,3]
b = { i * 2 for i in a }
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), set![int!(2), int!(4), int!(6),]);
    }

    #[test]
    fn object_comparison() {
        let input = r#"
a = [8,9,10]
b = a == [8,9,10]
c = a == [8,9]
d = a == [8,10,9]
e = [8,9,10] == a
f = a != [8,9]
g = a != [8,10,9]
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), bool!(true));
        assert_eq!(read(interpreter, "c"), bool!(false));
        assert_eq!(read(interpreter, "d"), bool!(false));
        assert_eq!(read(interpreter, "e"), bool!(true));
        assert_eq!(read(interpreter, "f"), bool!(true));
        assert_eq!(read(interpreter, "g"), bool!(true));
    }

    #[test]
    fn generator_basics() {
        let input = r#"
def countdown(n):
    yield n
    yield n - 1
    yield n - 2

a = countdown(5)
b = next(a)
c = next(a)
d = next(a)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(5));
        assert_eq!(read(interpreter, "c"), int!(4));
        assert_eq!(read(interpreter, "d"), int!(3));

        let input = r#"
def countdown(n):
    yield n

a = countdown(5)
b = next(a)
c = next(a)
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(&e, ExecutionErrorKind::StopIteration);
    }

    #[test]
    fn generator_as_iterator() {
        let input = r#"
def countdown(n):
    yield n
    yield n - 1
    yield n - 2

z = 0
for i in countdown(5):
    z = z + i
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), int!(12));

        let input = r#"
def countdown(n):
    yield n
    yield n - 1
    yield n - 2

z = [ i for i in countdown(5) ]
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), list![int!(5), int!(4), int!(3)]);
    }

    #[test]
    fn generator_with_nested_yield() {
        let input = r#"
def countdown(n):
    while n > 0:
        yield n
        n = n - 1

z = 0
for i in countdown(5):
    z = z + i
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), int!(15));

        let input = r#"
def countdown(n):
    for i in range(n):
        yield i

z = 0
for i in countdown(5):
    z = z + i
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "z"), int!(10));

        let input = r#"
def countdown():
    for i in [1,2]:
        yield i * 2

a = list(countdown())
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), list![int!(2), int!(4),]);

        let input = r#"
def countdown(n):
    if n > 5:
        while n < 10:
            yield n
            n += 1
    elif n > 3:
        while n < 8:
            yield n
            n += 2
    else:
        while n > 0:
            yield n
            n -= 1

a = [ i for i in countdown(4) ]
b = [ i for i in countdown(3) ]
c = [ i for i in countdown(7) ]
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), list![int!(4), int!(6),]);
        assert_eq!(read(interpreter, "b"), list![int!(3), int!(2), int!(1)]);
        assert_eq!(read(interpreter, "c"), list![int!(7), int!(8), int!(9)]);
    }

    #[test]
    fn basic_inheritance() {
        let input = r#"
class Parent:
    def baz(self):
        return 4

class Foo(Parent):
    def __init__(self):
        self.x = 12

f = Foo()
a = f.baz()
b = f.x
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert!(interpreter.state.is_class("Foo"));
        assert!(interpreter.state.is_class("Parent"));
        assert_eq!(read(interpreter, "a"), int!(4));
        assert_eq!(read(interpreter, "b"), int!(12));

        let input = r#"
class Parent:
    def baz(self):
        self.y = 11
        return 4

class Foo(Parent):
    def __init__(self):
        self.x = 0

    def bar(self):
        return self.y

f = Foo()
a = f.baz()
b = f.bar()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(4));
        assert_eq!(read(interpreter, "b"), int!(11));

        let input = r#"
class abstractclassmethod(classmethod):
    pass
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        // This used to throw an error based on classmethod not yet being a class. This is
        // found in abc.py in the Python standard lib.
        assert_type_is!(interpreter, "abstractclassmethod", Class);
        assert_eq!(
            interpreter
                .state
                .read("abstractclassmethod")
                .unwrap()
                .as_class()
                .unwrap()
                .super_mro()
                .first()
                .unwrap()
                .borrow()
                .name(),
            "classmethod"
        );
    }

    #[test]
    fn more_inheritance() {
        // Test that a parent constructor is not called when a child constructor is defined.
        let input = r#"
class Parent:
    def __init__(self):
        self.x = 3

    def one(self):
        return 1

    def bar(self):
        return 2

class ChildTwo(Parent):
    def __init__(self):
        pass

    def three(self):
        return self.x

d = ChildTwo().three()
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_attribute_error(&e, "ChildTwo", "x");

        // Test that multiple levels of a hierarchy can be traversed.
        let input = r#"
class Parent:
    def __init__(self):
        self.x = 3

    def one(self):
        return 1

    def bar(self):
        return 2

class ChildTwo(Parent):
    pass

class ChildThree(ChildTwo):
    def three(self):
        return self.x

child_three = ChildThree()
d = child_three.three()
e = child_three.one()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "d"), int!(3));
        assert_eq!(read(interpreter, "e"), int!(1));

        // Test that an attribute defined in a parent's constructor is stored in the child.
        let input = r#"
class Parent:
    def __init__(self):
        self.x = 3

class Child(Parent):
    def three(self):
        return self.x

child = Child()
c = child.three()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "c"), int!(3));

        // Check that calls to `super()` return a `Type::Super`.
        let input = r#"
class Parent:
    pass

class Child(Parent):
    def one(self):
        return super()

    @classmethod
    def two(cls):
        return super()

child = Child()
a = child.one()
b = Child.two()
c = child.two()
d = type(a)
e = type(b)
f = type(c)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", Super);
        assert_type_is!(interpreter, "b", Super);
        assert_type_is!(interpreter, "c", Super);
        assert_type_class(interpreter, "d", Type::Super);
        assert_type_class(interpreter, "e", Type::Super);
        assert_type_class(interpreter, "f", Type::Super);

        // Check that calls to `super()` works in an instance method.
        let input = r#"
class Parent:
    def one(self):
        return 1

class Child(Parent):
    def one(self):
        return super().one()

child = Child()
a = child.one()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(1));

        // Check that calls to `super()` works in an instance method including references to `self`.
        let input = r#"
class Parent:
    def __init__(self):
        self.val = 5

    def one(self):
        return self.val

class Child(Parent):
    def __init__(self):
        super().__init__()

    def one(self):
        return super().one()

    def two(self):
        return self.val

child = Child()
a = child.one()
b = child.two()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));
        assert_eq!(read(interpreter, "b"), int!(5));

        // Check that calls to `super()` works in a class method.
        let input = r#"
class Parent:
    @classmethod
    def two(cls):
        return 2

class Child(Parent):
    @classmethod
    def two(cls):
        return super().two()

child = Child()
b = child.two()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(2));

        // Check that calls to `super()` works in a class method included references to `cls`.
        let input = r#"
class Parent:
    val = 12

    @classmethod
    def two(cls):
        return cls.val

class Child(Parent):
    @classmethod
    def two(cls):
        return super().two()

child = Child()
b = child.two()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(12));
    }

    #[test]
    fn multiple_inheritance() {
        let input = r#"
class Bar: pass
class Baz: pass
class Foo(Bar, Baz): pass

a = Foo.__mro__
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        let mro = read(interpreter, "a")
            .as_tuple()
            .expect("Expected a tuple")
            .into_iter()
            .map(|i| i.as_class().unwrap().borrow().name().to_string())
            .collect::<Vec<String>>();
        assert_eq!(
            mro,
            vec![
                "Foo".to_string(),
                "Bar".into(),
                "Baz".into(),
                "object".into()
            ]
        );
    }

    #[test]
    fn dictionaries() {
        let input = r#"
a = { "b": 4, 'c': 5 }
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(
            read(interpreter, "a"),
            dict!(interpreter, { str!("b") => int!(4), str!("c") => int!(5) })
        );

        let input = r#"
a = { "b": 4, 'c': 5 }
b = a.items()
c = { key: value * 2  for key, value in a.items() }
d = dict({ "b": 4, 'c': 5 })
e = dict([('b', 4), ('c', 5)])
f = a["b"]
g = {}
h = {}.items()

i = {}.keys()
j = {"b": 4, 'c': 5}.keys()
k = iter({}.keys())
l = type(iter({}.keys()))

m = {}.values()
n = {"b": 4, 'c': 5}.values()
o = iter({}.values())
p = type(iter({}.values()))

q = iter({}.items())
r = type(iter({}.items()))

s = type({}.keys())
t = type({}.values())
u = type({}.items())
v = [ val for val in a ]
w = { key for key, value in a.items() }
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(
            read(interpreter, "a"),
            dict!(interpreter, { str!("b") => int!(4), str!("c") => int!(5) })
        );
        assert_eq!(
            read(interpreter, "b"),
            ExprResult::DictItems(DictItems::new(
                interpreter.clone(),
                vec![(str!("b"), int!(4)), (str!("c"), int!(5)),]
            ))
        );
        assert_eq!(
            read(interpreter, "c"),
            dict!(interpreter, { str!("b") => int!(8), str!("c") => int!(10) })
        );
        assert_eq!(
            read(interpreter, "d"),
            dict!(interpreter, { str!("b") => int!(4), str!("c") => int!(5) })
        );
        assert_eq!(
            read(interpreter, "e"),
            dict!(interpreter, { str!("b") => int!(4), str!("c") => int!(5) })
        );
        assert_eq!(read(interpreter, "f"), int!(4));
        assert_eq!(read(interpreter, "g"), dict!(interpreter, {}));
        assert_eq!(
            read(interpreter, "h"),
            ExprResult::DictItems(DictItems::default())
        );
        assert_type_is!(interpreter, "q", DictItemsIterator);
        assert_type_class(interpreter, "r", Type::DictItemIterator);
        assert_eq!(
            read(interpreter, "i"),
            ExprResult::DictKeys(DictKeys::new(vec![]))
        );
        assert_eq!(
            read(interpreter, "j"),
            ExprResult::DictKeys(DictKeys::new(vec![str!("b"), str!("c"),]))
        );
        assert_type_is!(interpreter, "k", DictKeysIterator);
        assert_type_class(interpreter, "l", Type::DictKeyIterator);
        assert_eq!(
            read(interpreter, "m"),
            ExprResult::DictValues(DictValues::new(vec![]))
        );
        assert_eq!(
            read(interpreter, "n"),
            ExprResult::DictValues(DictValues::new(vec![int!(4), int!(5),]))
        );
        assert_type_is!(interpreter, "o", DictValuesIterator);
        assert_type_class(interpreter, "p", Type::DictValueIterator);
        assert_type_class(interpreter, "s", Type::DictKeys);
        assert_type_class(interpreter, "t", Type::DictValues);
        assert_type_class(interpreter, "u", Type::DictItems);
        assert_eq!(read(interpreter, "v"), list![str!("b"), str!("c"),]);
        assert_eq!(read(interpreter, "w"), set![str!("b"), str!("c"),]);

        let input = r#"
a = { "b": 4, 'c': 5 }
b = a.get("b")
c = a.get("d")
d = a.get("d", 99)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(4));
        assert_eq!(read(interpreter, "c"), ExprResult::None);
        assert_eq!(read(interpreter, "d"), int!(99));

        let input = r#"
a = { "b": 4, 'c': 5 }
b = { **a }
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(
            read(interpreter, "b"),
            dict!(interpreter, { str!("b") => int!(4), str!("c") => int!(5) })
        );

        let input = r#"
inner = { 'key': 'inner' }
b = { 'key': 'outer', **inner }
c = { **inner, 'key': 'outer' }
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(
            read(interpreter, "b"),
            dict!(interpreter, { str!("key") => str!("inner") })
        );
        assert_eq!(
            read(interpreter, "c"),
            dict!(interpreter, { str!("key") => str!("outer") })
        );
    }

    #[test]
    fn assert() {
        let input = r#"
assert True
"#;
        let mut context = init(input);
        let _ = run(&mut context);

        let input = r#"
assert False
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(&e, ExecutionErrorKind::AssertionError);
    }

    #[test]
    fn try_except_finally() {
        let input = r#"
try:
    a = 4 / 0
except:
    a = 2
finally:
    a = 3
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(3));

        let input = r#"
try:
    a = 4 / 0
except:
    a = 2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(2));

        let input = r#"
try:
    a = 4 / 1
except:
    a = 2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(4));

        let input = r#"
try:
    a = 4 / 1
except:
    a = 2
finally:
    a = 3
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(3));

        // TODO should this move to the parser?
        //
        //         let input = r#"
        // try:
        //     a = 4 / 1
        // "#;
        //         let mut context = init(input);
        //
        //         match context.run_and_return_interpreter() {
        //             Err(e) => assert!(matches!(e, MemphisError::Parser(ParserError::SyntaxError))),
        //             Ok(_) => panic!("Expected an error!"),
        //         }

        let input = r#"
try:
    a = 4 / 0
except ZeroDivisionError:
    a = 2
except Exception:
    a = 3
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(2));

        let input = r#"
try:
    b = b + 1
    a = 4 / 0
except ZeroDivisionError:
    a = 2
except Exception:
    a = 3
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(3));

        let input = r#"
try:
    b = b + 1
    a = 4 / 0
except ZeroDivisionError:
    a = 2
except:
    a = 3
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(3));

        let input = r#"
try:
    b = b + 1
    a = 4 / 0
except ZeroDivisionError:
    a = 2
except Exception as e:
    a = 3
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(3));
        let e = extract!(interpreter, "e", Exception);
        test_utils::assert_name_error(&e, "b");

        let input = r#"
try:
    b = b + 1
    a = 4 / 0
except (ZeroDivisionError, Exception):
    a = 2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(2));

        let input = r#"
try:
    b = b + 1
    a = 4 / 0
except (Exception, ZeroDivisionError):
    a = 2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(2));

        let input = r#"
try:
    a = 8 / 1
except ZeroDivisionError:
    a = 2
except Exception as e:
    a = 3
else:
    a = 4
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(4));

        let input = r#"
try:
    a = 8 / 1
except ZeroDivisionError:
    a = 2
except Exception as e:
    a = 3
else:
    a = 4
finally:
    a = 5
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));

        // Uncaught exception
        let input = r#"
try:
    a = 8 / 0
except ValueError:
    a = 2
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(
            &e,
            ExecutionErrorKind::DivisionByZero("integer division or modulo by zero".into()),
        );

        let input = r#"
try:
    a = 8 / 0
except ZeroDivisionError:
    raise
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(
            &e,
            ExecutionErrorKind::DivisionByZero("integer division or modulo by zero".into()),
        );
    }

    #[test]
    fn args_and_kwargs() {
        let input = r#"
def test_kwargs(**kwargs):
    print(kwargs['a'])
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        let expected_args = Params {
            args: vec![],
            args_var: None,
            kwargs_var: Some("kwargs".into()),
        };
        let f = extract!(interpreter, "test_kwargs", Function);
        assert_eq!(f.borrow().args, expected_args);

        let input = r#"
def test_kwargs(**kwargs):
    return kwargs['a']

a = test_kwargs(a=5, b=2)
# A second test to ensure the value is not being set using b=2
b = test_kwargs(a=5, b=2)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));
        assert_eq!(read(interpreter, "b"), int!(5));

        let input = r#"
def test_kwargs(**kwargs):
    return kwargs['a']

a = test_kwargs(**{'a': 5, 'b': 2})
c = {'a': 4, 'b': 3}
b = test_kwargs(**c)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));
        assert_eq!(read(interpreter, "b"), int!(4));

        let input = r#"
def test_kwargs(**kwargs):
    return kwargs['a'], kwargs['b']

first = {'a': 44 }
second = {'b': 55 }
b = test_kwargs(**first, **second)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), tuple![int!(44), int!(55),]);

        let input = r#"
def test_args(*args):
    return args[1]

c = [0, 1]
b = test_args(*c)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(1));

        let input = r#"
def test_args(*args):
    return args[1]

c = [2, 3]
b = test_args(0, 1, *c)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(1));

        let input = r#"
def test_args(one, two, *args):
    return args[1]

c = [2, 3]
b = test_args(0, 1, *c)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(3));

        let input = r#"
def test_args(one, two):
    return one

b = test_args(0)
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(
            &e,
            "test_args() missing 1 required positional argument: 'two'",
        );

        let input = r#"
def test_args(one, two, three):
    return one

b = test_args(0)
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(
            &e,
            "test_args() missing 2 required positional arguments: 'two' and 'three'",
        );

        let input = r#"
def test_args(one, two):
    return one

b = test_args(1, 2, 3)
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "Found 3 args");

        let input = r#"
def test_args(one, two, *args):
    return args

b = test_args(1, 2)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), tuple![]);

        let input = r#"
class Foo:
    def __init__(self, **kwargs):
        self.a = kwargs['a']

foo = Foo(a=5)
a = foo.a
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));
    }

    #[test]
    fn closures() {
        let input = r#"
def _cell_factory():
    a = 1
    def f():
        nonlocal a
    return f.__closure__

a = type(_cell_factory()[0])
b = _cell_factory()[0].cell_contents
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::Cell);
        assert_eq!(read(interpreter, "b"), int!(1));

        let input = r#"
def _cell_factory():
    a = 1
    b = 2
    def f():
        nonlocal a
    return f.__closure__

a = type(_cell_factory()[0])
b = _cell_factory()[0].cell_contents
c = len(_cell_factory())
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::Cell);
        assert_eq!(read(interpreter, "b"), int!(1));
        assert_eq!(read(interpreter, "c"), int!(1));

        let input = r#"
def _cell_factory():
    a = 1
    b = 2
    def f():
        print(a)
        print(b)
    return f.__closure__

a = _cell_factory()[0].cell_contents
b = _cell_factory()[1].cell_contents
c = len(_cell_factory())
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(1));
        assert_eq!(read(interpreter, "b"), int!(2));
        assert_eq!(read(interpreter, "c"), int!(2));

        let input = r#"
def _cell_factory():
    a = 1
    b = 2
    def f():
        b = 3
        print(a)
        print(b)
    return f.__closure__

c = len(_cell_factory())
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "c"), int!(1));

        let input = r#"
def _cell_factory():
    a = 1
    b = 2
    def f():
        b = 3
    return f.__closure__

c = _cell_factory()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "c"), ExprResult::None);
    }

    #[test]
    fn decorators() {
        let input = r#"
def test_decorator(func):
    def wrapper():
        return func() * 2
    return wrapper

def get_val_undecorated():
    return 2

@test_decorator
def get_val_decorated():
    return 2

@test_decorator
@test_decorator
def twice_decorated():
    return 2

a = test_decorator(get_val_undecorated)()
b = get_val_decorated()
c = twice_decorated()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(4));
        assert_eq!(read(interpreter, "b"), int!(4));
        assert_eq!(read(interpreter, "c"), int!(8));

        let input = r#"
def multiply(factor):
    def decorator(func):
        def wrapper(*args, **kwargs):
            return func(*args, **kwargs) * factor
        return wrapper
    return decorator

@multiply(3)
def get_val():
    return 2

@multiply(4)
def get_larger_val():
    return 2

a = get_val()
b = get_larger_val()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(6));
        assert_eq!(read(interpreter, "b"), int!(8));

        let input = r#"
def normalize(func):
    def wrapper(self, val):
        return func(self, val) * self.factor
    return wrapper

class Foo:
    def __init__(self, factor):
        self.factor = factor

    @normalize
    def calculate(self, val):
        return val

f = Foo(7)
a = f.calculate(2)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(14));

        let input = r#"
def normalize(base=3):
    def decorating_function(func):
        def wrapper(self, val):
            return func(self, val) * self.factor * base
        return wrapper
    return decorating_function

class Foo:
    def __init__(self, factor):
        self.factor = factor

    @normalize()
    def calculate(self, val):
        return val

f = Foo(7)
a = f.calculate(2)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(42));
    }

    #[test]
    fn nested_function() {
        let input = r#"
class Foo:
    def calculate(self, val):
        def secret_sauce(x):
            return x * 9
        return secret_sauce(val)

f = Foo()
a = f.calculate(2)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(18));
    }

    #[test]
    fn raise() {
        let input = r#"
raise TypeError
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error_optional_message(&e, None);

        let input = r#"
raise TypeError('type is no good')
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "type is no good");
    }

    #[cfg(feature = "c_stdlib")]
    #[test]
    fn c_stdlib() {
        let input = r#"
import sys
a = sys.maxsize

import time
b = time.time()
c = time.ctime()
d = time.strftime("%a, %d %b %Y %H:%M:%S +0000")

from _weakref import ref

e = type(sys.implementation)
f = type(sys)

import itertools
g = type(itertools)

import _thread
h = type(_thread)

i = type(sys.builtin_module_names)

import posix
j = type(posix)

import builtins
k = type(builtins)

import errno
l = type(errno)

sys.modules['os.path'] = "os_path"
m = sys.modules['os.path']
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", Integer);
        assert_type_is!(interpreter, "b", FloatingPoint);
        assert_type_is!(interpreter, "c", String);
        assert_type_is!(interpreter, "d", String);
        let a = extract!(interpreter, "a", Integer);
        assert!(a > 0);
        let b = extract!(interpreter, "b", FloatingPoint);
        assert!(b > 1701281981.0);
        let c = extract!(interpreter, "c", String);
        assert!(c.len() > 10);
        let d = extract!(interpreter, "d", String);
        assert!(d.len() > 10);
        assert_type_is!(interpreter, "ref", CPythonObject);
        assert_type_is!(interpreter, "e", CPythonClass);
        assert_type_class(interpreter, "f", Type::Module);
        assert_type_class(interpreter, "g", Type::Module);
        assert_type_class(interpreter, "h", Type::Module);
        assert_type_class(interpreter, "i", Type::Tuple);
        assert_type_class(interpreter, "j", Type::Module);
        assert_type_class(interpreter, "k", Type::Module);
        assert_type_class(interpreter, "l", Type::Module);
        assert_eq!(read(interpreter, "m"), str!("os_path"));
    }

    #[test]
    fn context_manager() {
        let input = r#"
class MyContextManager:
    def __init__(self):
        self.a = 0

    def __enter__(self):
        self.a += 1
        return self

    def call(self):
        self.a += 1

    def __exit__(self, exc_type, exc_value, traceback):
        self.a += 1
        self

with MyContextManager() as cm:
    cm.call()

a = cm.a
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(3));

        let input = r#"
class MyContextManager:
    def __init__(self):
        self.a = 0

    def call(self):
        self.a += 1

    def __exit__(self, exc_type, exc_value, traceback):
        self.a += 1
        self

with MyContextManager() as cm:
    cm.call()
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(&e, ExecutionErrorKind::MissingContextManagerProtocol);

        let input = r#"
class MyContextManager:
    def __init__(self):
        self.a = 0

    def __enter__(self):
        self.a += 1
        return self

    def call(self):
        self.a += 1

with MyContextManager() as cm:
    cm.call()
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(&e, ExecutionErrorKind::MissingContextManagerProtocol);
    }

    #[test]
    fn delete() {
        let input = r#"
a = 4
del a
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read_optional(interpreter, "a"), None);

        let input = r#"
a = {'b': 1, 'c': 2}
del a['b']
c = a['b']
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_key_error(&e, "b");

        let input = r#"
a = [0,1,2]
del a[1]
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), list![int!(0), int!(2)]);

        let input = r#"
class Foo:
    def __init__(self):
        self.x = 1

f = Foo()
del f.x
a = f.x
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_attribute_error(&e, "Foo", "x");

        let input = r#"
class Foo:
    def bar(self):
        return 1

f = Foo()
del f.bar
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_attribute_error(&e, "Foo", "bar");

        let input = r#"
a = 4
b = 5
c = 6
del a, c
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read_optional(interpreter, "a"), None);
        assert_eq!(read(interpreter, "b"), int!(5));
        assert_eq!(read_optional(interpreter, "c"), None);
    }

    #[test]
    fn byte_string() {
        let input = r#"
a = b'hello'
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), ExprResult::Bytes("hello".into()));

        let input = r#"
a = iter(b'hello')
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", BytesIterator);

        let input = r#"
a = type(iter(b'hello'))
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::BytesIterator);
    }

    #[test]
    fn byte_array() {
        let input = r#"
a = bytearray()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(
            read(interpreter, "a"),
            ExprResult::ByteArray(Container::new(ByteArray::new("".into())))
        );

        let input = r#"
a = bytearray(b'hello')
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(
            read(interpreter, "a"),
            ExprResult::ByteArray(Container::new(ByteArray::new("hello".into())))
        );

        let input = r#"
a = bytearray('hello')
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "string argument without an encoding");

        let input = r#"
a = iter(bytearray())
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", ByteArrayIterator);

        let input = r#"
a = type(iter(bytearray()))
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::ByteArrayIterator);
    }

    #[test]
    fn bytes_builtin() {
        let input = r#"
a = bytes()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), ExprResult::Bytes("".into()));

        let input = r#"
a = bytes(b'hello')
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), ExprResult::Bytes("hello".into()));

        let input = r#"
a = bytes('hello')
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "string argument without an encoding");

        let input = r#"
a = iter(bytes())
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", BytesIterator);

        let input = r#"
a = type(iter(bytes()))
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::BytesIterator);
    }

    #[test]
    fn compound_operator() {
        let input = r#"
a = 5
a += 1
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(6));

        let input = r#"
a = 5
a -= 1
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(4));

        let input = r#"
a = 5
a *= 2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(10));

        let input = r#"
a = 5
a /= 2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(2));

        let input = r#"
a = 0b0101
a &= 0b0100
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(4));

        let input = r#"
a = 0b0101
a |= 0b1000
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(13));

        let input = r#"
a = 0b0101
a ^= 0b0100
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(1));

        let input = r#"
a = 5
a //= 2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(2));

        let input = r#"
a = 0b0101
a <<= 1
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(10));

        let input = r#"
a = 0b0101
a >>= 1
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(2));

        let input = r#"
a = 11
a %= 2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(1));

        let input = r#"
a = 2
a **= 3
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(8));
    }

    #[test]
    fn iter_builtin() {
        let input = r#"
a = iter([1,2,3])
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", ListIterator);

        let input = r#"
b = 0
for i in iter([1,2,3]):
    b += i
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(6));
    }

    #[test]
    fn f_strings() {
        let input = r#"
name = "John"
a = f"Hello {name}"
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), str!("Hello John"));
    }

    #[test]
    fn reversed() {
        let input = r#"
a = reversed([])
b = iter(reversed([]))
c = type(reversed([]))
d = type(iter(reversed([])))

e = [ i for i in reversed([1,2,3]) ]
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", ReversedIterator);
        assert_type_is!(interpreter, "b", ReversedIterator);
        assert_type_class(interpreter, "c", Type::ReversedIterator);
        assert_type_class(interpreter, "d", Type::ReversedIterator);
        assert_eq!(read(interpreter, "e"), list![int!(3), int!(2), int!(1),]);
    }

    #[test]
    fn binary_operators() {
        let input = "a = 0x1010 & 0x0011";
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(0x0010));

        let input = "a = 0o1010 | 0o0011";
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(0o1011));

        let input = "a = 0b1010 ^ 0b0011";
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(0b1001));

        let input = "a = 23 % 5";
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(3));

        let input = "a = 0b0010 << 1";
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(0b0100));

        let input = "a = 2 * 3 << 2 + 4 & 205";
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(128));

        let input = "a = ~0b1010";
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(-11));

        let input = "a = ~5.5";
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "bad operand type for unary ~: 'float'");

        // This tests the right-associativity of exponentiation.
        // right-associativity gives 2 ** (3 ** 2) == 512
        // NOT
        // left-associativity which gives (2 ** 3) ** 2 == 64
        let input = "a = 2 ** 3 ** 2";
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(512));
    }

    #[test]
    fn control_flow() {
        let input = r#"
a = 0
for i in [1,2,3,4,5,6]:
    if i == 4:
        break
    a += i
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(6));

        let input = r#"
a = 0
for i in [1,2,3,4,5,6]:
    if i == 4:
        continue
    a += i
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(17));

        let input = r#"
a = 0
i = 0
b = [1,2,3,4,5,6]
while i < 6:
    i += 1
    if b[i-1] == 4:
        break
    a += b[i-1]
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(6));

        let input = r#"
a = 0
i = 0
b = [1,2,3,4,5,6]
while i < 6:
    i += 1
    if b[i-1] == 4:
        continue
    a += b[i-1]
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(17));

        let input = r#"
a = 0
for i in [1,2,3,4,5,6]:
    if i == 4:
        break
    a += i
else:
    a = 1024
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(6));

        let input = r#"
a = 0
for i in [1,2,3,4,5,6]:
    a += i
else:
    a = 1024
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(1024));
    }

    #[test]
    fn zip() {
        let input = r#"
a = zip()
b = iter(zip())
c = type(zip())

d = [ i for i in zip([1,2,3], [4,5,6]) ]
e = [ i for i in iter(zip([1,2,3], [4,5,6])) ]

f = [ i for i in zip(range(5), range(4)) ]
g = [ i for i in zip(range(5), range(4), range(3)) ]

h = [ i for i in zip([1,2,3], [4,5,6], strict=False) ]
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", Zip);
        assert_type_is!(interpreter, "b", Zip);
        assert_type_class(interpreter, "c", Type::Zip);
        assert_eq!(
            read(interpreter, "d"),
            list![
                tuple![int!(1), int!(4),],
                tuple![int!(2), int!(5),],
                tuple![int!(3), int!(6),]
            ]
        );
        assert_eq!(
            read(interpreter, "e"),
            list![
                tuple![int!(1), int!(4),],
                tuple![int!(2), int!(5),],
                tuple![int!(3), int!(6),]
            ]
        );
        assert_eq!(
            read(interpreter, "f"),
            list![
                tuple![int!(0), int!(0),],
                tuple![int!(1), int!(1),],
                tuple![int!(2), int!(2),],
                tuple![int!(3), int!(3),]
            ]
        );
        assert_eq!(
            read(interpreter, "g"),
            list![
                tuple![int!(0), int!(0), int!(0),],
                tuple![int!(1), int!(1), int!(1),],
                tuple![int!(2), int!(2), int!(2),]
            ]
        );
        assert_eq!(
            read(interpreter, "h"),
            list![
                tuple![int!(1), int!(4),],
                tuple![int!(2), int!(5),],
                tuple![int!(3), int!(6),]
            ]
        );

        let input = r#"
f = [ i for i in zip(range(5), range(4), strict=True) ]
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(&e, ExecutionErrorKind::RuntimeError);
    }

    #[test]
    fn type_class() {
        let input = r#"
a = type
b = type.__dict__
c = type(type.__dict__)
d = type(dict.__dict__['fromkeys'])
# TODO this should fail
e = type(object().__dict__)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::Type);
        assert_type_is!(interpreter, "b", MappingProxy);
        assert_type_class(interpreter, "c", Type::MappingProxy);
        assert_type_class(interpreter, "d", Type::BuiltinMethod);
        assert_type_class(interpreter, "e", Type::Dict);
    }

    #[test]
    fn type_alias() {
        let input = r#"
a = type(list[int])
b = type(a)
c = type(int | str)
d = list[int]
e = int | str
f = int

# This used to fail inside classmethod::__new__
class MyClass:
    __class_getitem__ = classmethod(a)
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", Class);
        assert_type_class(interpreter, "b", Type::Type);
        assert_type_is!(interpreter, "c", Class);
        assert_type_is!(interpreter, "d", TypeNode);
        assert_type_is!(interpreter, "e", TypeNode);
        assert_type_is!(interpreter, "f", Class);
    }

    #[test]
    fn class_variable() {
        let input = r#"
class Foo:
    a = 6

b = Foo.a
Foo.a = 5
c = Foo.a
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(6));
        assert_eq!(read(interpreter, "c"), int!(5));

        let input = r#"
class Foo:
    a = 6

    def __init__(self):
        self.a = 5

b = Foo.a
c = Foo().a
d = Foo.a
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(6));
        assert_eq!(read(interpreter, "c"), int!(5));
        assert_eq!(read(interpreter, "d"), int!(6));
    }

    #[test]
    fn class_method() {
        let input = r#"
class Foo:
    def make(cls):
        return 5

    make = classmethod(make)

b = Foo.make()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        let method = extract_member!(interpreter, "Foo", "make", Class);
        assert!(matches!(method, ExprResult::Method(_)));

        assert_eq!(read(interpreter, "b"), int!(5));

        let input = r#"
class Foo:
    @classmethod
    def make(cls):
        return 5

b = Foo.make()
c = Foo().make()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(5));
        assert_eq!(read(interpreter, "c"), int!(5));

        let input = r#"
class Foo:
    val = 10

    def __init__(self):
        self.val = 9

    @classmethod
    def make(cls):
        return cls.val

b = Foo.make()
c = Foo.val
d = Foo().val
e = Foo().make()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(10));
        assert_eq!(read(interpreter, "c"), int!(10));
        assert_eq!(read(interpreter, "d"), int!(9));
        assert_eq!(read(interpreter, "e"), int!(10));

        let input = r#"
class Foo:
    def __init__(self):
        self.val = 9

    @classmethod
    def make(cls):
        return cls.val

b = Foo.make()
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_attribute_error(&e, "Foo", "val");

        let input = r#"
class Foo:
    def __init__(self):
        self.val = 9

    @classmethod
    def make(cls):
        return cls.val

b = Foo().make()
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_attribute_error(&e, "Foo", "val");
    }

    #[test]
    fn static_method() {
        let input = r#"
class Foo:
    @staticmethod
    def make():
        return 5

b = Foo.make()
c = Foo().make()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(5));
        assert_eq!(read(interpreter, "c"), int!(5));

        // Before we explicitly supported static methods, this case used to work. Let's test it to
        // ensure we keep getting an error now.
        let input = r#"
class Foo:
    def make():
        return 5

c = Foo().make()
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "Found 0 args");
    }

    #[test]
    fn new_method() {
        let input = r#"
class SingletonA:
    _instance = None
    _initialized = False

    def __new__(cls, *args, **kwargs):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def __init__(self, data):
        if not self._initialized:
            self.data = data
            self._initialized = True

singleton1 = SingletonA("First")
singleton2 = SingletonA("Second")

a = singleton1.data
b = singleton2.data
c = singleton1 is singleton2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), str!("First"));
        assert_eq!(read(interpreter, "b"), str!("First"));
        assert_eq!(read(interpreter, "c"), bool!(true));

        let input = r#"
class SingletonB:
    _instance = None

    def __new__(cls, *args, **kwargs):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def __init__(self, data):
        self.data = data

singleton1 = SingletonB("First")
singleton2 = SingletonB("Second")

a = singleton1.data
b = singleton2.data
c = singleton1 is singleton2
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), str!("Second"));
        assert_eq!(read(interpreter, "b"), str!("Second"));
        assert_eq!(read(interpreter, "c"), bool!(true));

        let input = r#"
class Foo:
    def __new__(cls):
        pass

a = Foo()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), ExprResult::None);
    }

    #[test]
    fn metaclasses() {
        let input = r#"
class InterfaceMeta(type):
    def __new__(mcls, name, bases, namespace, **kwargs):
        return super().__new__(mcls, name, bases, namespace)

    def run(cls):
        return 5

class BaseInterface(metaclass=InterfaceMeta):
    @classmethod
    def run_base(cls):
        return 5

class ConcreteImplementation(BaseInterface):
    pass

a = ConcreteImplementation.run()
b = ConcreteImplementation.run_base()
try:
    c = ConcreteImplementation().run()
except Exception as e:
    d = e
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));
        assert_eq!(read(interpreter, "b"), int!(5));
        let e = extract!(interpreter, "d", Exception);
        test_utils::assert_attribute_error(&e, "ConcreteImplementation", "run");

        let input = r#"
class InterfaceMeta(type):
    def __new__(mcls, name, bases, namespace, **kwargs):
        return super().__new__(mcls, name, bases, namespace)

    def run(cls):
        return 5

class BaseInterface(metaclass=InterfaceMeta):
    pass

class ConcreteImplementation(BaseInterface):
    pass

# This should use the metaclass implementation.
a = ConcreteImplementation.run()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));

        let input = r#"
class ABCMeta(type):
    def __new__(mcls, name, bases, namespace, **kwargs):
        cls = super().__new__(mcls, name, bases, namespace)
        cls.val = 33
        return cls

    def register(cls):
        return cls.val

class Coroutine(metaclass=ABCMeta):
    def sub_func(self):
        pass

a = Coroutine.register()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(33));

        // The test used to fail when we mistakenly were binding a metalcass call to __new__.
        let input = r#"
class ABCMeta(type):
    def __new__(mcls, name, bases, namespace, **kwargs):
        cls = super().__new__(mcls, name, bases, namespace)
        cls.val = 33
        return cls

    def register(cls):
        return cls.val

class ChildMeta(ABCMeta):
    def __new__(cls, name, bases, namespace, **kwargs):
        return super().__new__(cls, name, bases, namespace, **kwargs)

class Coroutine(metaclass=ChildMeta):
    def sub_func(self):
        pass

a = Coroutine.register()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(33));
    }

    #[test]
    fn scope_modifiers() {
        let input = r#"
global_var_one = 10
global_var_two = 10

def global_shadow():
   global_var_one = 9
global_shadow()

def global_modified():
    global global_var_two
    global_var_two = 9
global_modified()

a = global_var_one
b = global_var_two
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(10));
        assert_eq!(read(interpreter, "b"), int!(9));

        let input = r#"
def nonlocal_shadow():
    var = 5
    def f():
        var = 4
    f()
    return var

def nonlocal_modified():
    var = 5
    def f():
        nonlocal var
        var = 4
    f()
    return var

a = nonlocal_shadow()
b = nonlocal_modified()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));
        assert_eq!(read(interpreter, "b"), int!(4));

        let input = r#"
def outer():
    x = 10

    def middle():
        def inner():
            # Refers to x in the outer scope, not middle, as it isn't defined in middle
            nonlocal x
            x = 20

        inner()

    middle()
    return x

a = outer()
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(20));

        let input = r#"
def foo():
    def inner():
        nonlocal a
    inner()
foo()
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(&e, ExecutionErrorKind::SyntaxError);

        let input = r#"
def foo():
    nonlocal a
foo()
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(&e, ExecutionErrorKind::SyntaxError);

        let input = r#"
nonlocal a
"#;
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_error_kind(&e, ExecutionErrorKind::SyntaxError);
    }

    #[test]
    fn object_builtin() {
        let input = r#"
a = object
b = object()
c = object().__str__
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::Object);
        assert_type_is!(interpreter, "b", Object);
        assert_type_is!(interpreter, "c", Method);
    }

    #[test]
    fn int_builtin() {
        let input = r#"
a = int
b = int()
c = int(5)
d = int('6')
"#;
        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::Int);
        assert_eq!(read(interpreter, "b"), int!(0));
        assert_eq!(read(interpreter, "c"), int!(5));
        assert_eq!(read(interpreter, "d"), int!(6));
    }

    #[test]
    fn bound_function() {
        let input = r#"
class Child:
    def one(self):
        return 1

    @classmethod
    def two(cls):
        return 2

    @staticmethod
    def three():
        return 3

child = Child()
a = child.one
b = Child.one
c = child.two
d = Child.two
e = child.three
f = Child.three

g = type(a)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", Method);
        //assert_type_is!(interpreter, "b", Function);
        assert_type_is!(interpreter, "c", Method);
        assert_type_is!(interpreter, "d", Method);
        assert_type_is!(interpreter, "e", Function);
        assert_type_is!(interpreter, "f", Function);
        assert_type_class(interpreter, "g", Type::Method);

        let input = r#"
class Child:
    def one(self):
        return 1

    @classmethod
    def two(cls):
        return 2

    @staticmethod
    def three():
        return 3

child = Child()
a = child.one()
#b = Child.one # This one causes an error, test this separately
c = child.two()
d = Child.two()
e = child.three()
f = Child.three()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(1));
        assert_eq!(read(interpreter, "c"), int!(2));
        assert_eq!(read(interpreter, "d"), int!(2));
        assert_eq!(read(interpreter, "e"), int!(3));
        assert_eq!(read(interpreter, "f"), int!(3));

        let input = r#"
class Child:
    def one(self):
        return 1

b = Child.one()
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "one() missing 1 required positional argument: 'self'");

        let input = r#"
class Child:
    class_var = 22

    def __init__(self):
        self.instance_var = 11

    def one(self):
        return self.instance_var

    @classmethod
    def two(cls):
        return cls.class_var

child = Child()
a = child.one()
#b = Child.one # This one causes an error, test this separately
c = child.two()
d = Child.two()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(11));
        assert_eq!(read(interpreter, "c"), int!(22));
        assert_eq!(read(interpreter, "d"), int!(22));
    }

    #[test]
    fn unpacking() {
        let input = r#"
def foo():
    return 2, 3

a = foo()
b, c = foo()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), tuple![int!(2), int!(3)]);
        assert_eq!(read(interpreter, "b"), int!(2));
        assert_eq!(read(interpreter, "c"), int!(3));

        let input = r#"
b, c = [1, 2, 3]
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_value_error(&e, "too many values to unpack (expected 2)");

        let input = r#"
a, b, c = [2, 3]
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_value_error(&e, "not enough values to unpack (expected 3, got 2)");

        let input = r#"
b, c = (1, 2)
d, e = [1, 2]
f, g = {1, 2}
h, i, j = range(1, 4)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(1));
        assert_eq!(read(interpreter, "c"), int!(2));
        assert_eq!(read(interpreter, "d"), int!(1));
        assert_eq!(read(interpreter, "e"), int!(2));
        assert_eq!(read(interpreter, "f"), int!(1));
        assert_eq!(read(interpreter, "g"), int!(2));
        assert_eq!(read(interpreter, "h"), int!(1));
        assert_eq!(read(interpreter, "i"), int!(2));
        assert_eq!(read(interpreter, "j"), int!(3));

        let input = r#"
l = [1,2]
a = (*l,)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), tuple![int!(1), int!(2)]);

        let input = r#"
a = (*5)
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "Value after * must be an iterable, not int");

        // TODO not sure where to detect this, probably in semantic analysis
        //         let input = r#"
        // l = [1,2]
        // a = (*l)
        // "#;
        //
        //         let mut context = init(input);
        //
        //         match context.run() {
        //             Err(e) => assert_eq!(
        //                 e,
        //                 MemphisError::Interpreter(InterpreterError::ValueError(
        //                     "not enough values to unpack (expected 3, got 2)".into(),
        //                     interpreter.state.call_stack()
        //                 ))
        //             ),
        //             Ok(interpreter) => panic!("Expected an error!"),
        //         }
    }

    #[test]
    fn ternary_operation() {
        let input = r#"
a = 5 if True else 6
b = 7 if False else 8
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));
        assert_eq!(read(interpreter, "b"), int!(8));
    }

    #[test]
    fn slices() {
        let input = r#"
a = list(range(1,11))
b = a[:2]
c = a[7:]
d = a[::2]
e = a[::-2]
f = a[2:4]
g = a[-1:]
h = a[:-9]
i = a[4:2]

j = slice(5)
k = slice(5,10)
l = slice(5,10,2)
m = type(slice(5))

word = "hello"
n = word[0]
o = word[:1]
p = word[:2]
#q = word[-1]

r = [2,4,6][:]
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), list![int!(1), int!(2),]);
        assert_eq!(read(interpreter, "c"), list![int!(8), int!(9), int!(10),]);
        assert_eq!(
            read(interpreter, "d"),
            list![int!(1), int!(3), int!(5), int!(7), int!(9),]
        );
        assert_eq!(
            read(interpreter, "e"),
            list![int!(10), int!(8), int!(6), int!(4), int!(2),]
        );
        assert_eq!(read(interpreter, "f"), list![int!(3), int!(4),]);
        assert_eq!(read(interpreter, "g"), list![int!(10),]);
        assert_eq!(read(interpreter, "h"), list![int!(1),]);
        assert_eq!(read(interpreter, "i"), list![]);
        assert_type_is!(interpreter, "j", Slice);
        assert_type_is!(interpreter, "k", Slice);
        assert_type_is!(interpreter, "l", Slice);
        assert_type_class(interpreter, "m", Type::Slice);
        assert_eq!(read(interpreter, "n"), str!("h"));
        assert_eq!(read(interpreter, "o"), str!("h"));
        assert_eq!(read(interpreter, "p"), str!("he"));
        //assert_eq!(read(interpreter, "q"), str!("he"));
        assert_eq!(read(interpreter, "r"), list![int!(2), int!(4), int!(6),]);
    }

    #[test]
    fn property_decorator() {
        let input = r#"
class Foo:
    def __init__(self):
        self.val = 3

    @property
    def run(self):
        return 2 * self.val

a = Foo().run
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(6));
    }

    #[test]
    fn slash_args() {
        let input = r#"
def foo(cls, /):
    pass
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "foo", Function);
    }

    #[test]
    fn globals_builtin() {
        let input = r#"
a = 4
b = globals()
c = b['a']
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "c"), int!(4));
    }

    #[test]
    fn generator_comprehension() {
        let input = r#"
a = (i * 2 for i in [1,2])
b = next(a)
c = next(a)

d = (x * y for x in [2,4] for y in [3,5])
e = type(d)
f = list(d)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "b"), int!(2));
        assert_eq!(read(interpreter, "c"), int!(4));
        assert_type_is!(interpreter, "d", Generator);
        assert_type_class(interpreter, "e", Type::Generator);
        assert_eq!(
            read(interpreter, "f"),
            list![int!(6), int!(10), int!(12), int!(20),]
        );
    }

    #[test]
    fn frozenset() {
        let input = r#"
a = frozenset([1,2,2])
b = frozenset()
c = type(b)
d = [ i for i in a ]

e = frozenset().__contains__
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(
            read(interpreter, "a"),
            ExprResult::FrozenSet(FrozenSet::new(HashSet::from([int!(1), int!(2),])))
        );
        assert_eq!(
            read(interpreter, "b"),
            ExprResult::FrozenSet(FrozenSet::default())
        );
        assert_type_class(interpreter, "c", Type::FrozenSet);
        assert_eq!(read(interpreter, "d"), list![int!(1), int!(2),]);
        assert_eq!(read_type(interpreter, "e"), Type::Method);

        let input = "frozenset([1,2,3], [1,2])";
        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "Found 3 args");
    }

    #[test]
    fn default_args() {
        let input = r#"
def foo(data=None):
    return data if data is not None else 99

a = foo(88)
b = foo()
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(88));
        assert_eq!(read(interpreter, "b"), int!(99));

        let input = r#"
def foo(data_one, data_two=None):
    pass

b = foo()
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(
            &e,
            "foo() missing 1 required positional argument: 'data_one'",
        );
    }

    #[test]
    fn getattr_builtin() {
        let input = r#"
class Foo:
    def __init__(self):
        self.val = 44

f = Foo()
a = getattr(f, 'val')
b = getattr(f, 'val_two', 33)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(44));
        assert_eq!(read(interpreter, "b"), int!(33));

        let input = r#"
class Foo:
    pass

f = Foo()
b = getattr(f, 'val_two')
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_attribute_error(&e, "Foo", "val_two");
    }

    #[test]
    fn isinstance_builtin() {
        let input = r#"
class Foo: pass

class Bar(Foo): pass

class Baz: pass

a = isinstance([], list)
b = isinstance([], int)
c = isinstance(int, Foo)
d = isinstance(type, Foo)
e = isinstance(Foo(), Foo)
f = isinstance(Foo, Foo)
g = isinstance(Bar(), Foo)
h = isinstance(Baz(), Foo)
i = isinstance(Foo, type)
j = isinstance(Foo(), type)
k = isinstance([], (int, list))
l = isinstance([], (int, Foo))
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), bool!(true));
        assert_eq!(read(interpreter, "b"), bool!(false));
        assert_eq!(read(interpreter, "c"), bool!(false));
        assert_eq!(read(interpreter, "d"), bool!(false));
        assert_eq!(read(interpreter, "e"), bool!(true));
        assert_eq!(read(interpreter, "f"), bool!(false));
        assert_eq!(read(interpreter, "g"), bool!(true));
        assert_eq!(read(interpreter, "h"), bool!(false));
        assert_eq!(read(interpreter, "i"), bool!(true));
        assert_eq!(read(interpreter, "j"), bool!(false));
        assert_eq!(read(interpreter, "k"), bool!(true));
        assert_eq!(read(interpreter, "l"), bool!(false));

        let input = r#"
isinstance([], (int, 5))
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(
            &e,
            "isinstance() arg 2 must be a type, a tuple of types, or a union",
        );
    }

    #[test]
    fn issubclass_builtin() {
        let input = r#"
class Foo: pass

class Bar(Foo): pass

class Baz: pass

a = issubclass(int, Foo)
b = issubclass(type, Foo)
c = issubclass(Foo, Foo)
d = issubclass(Bar, Foo)
e = issubclass(Foo, type)
f = issubclass(Baz, Foo)
g = issubclass(Foo, object)
h = issubclass(Foo, Bar)
i = issubclass(object, object)
j = issubclass(type, type)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), bool!(false));
        assert_eq!(read(interpreter, "b"), bool!(false));
        assert_eq!(read(interpreter, "c"), bool!(true));
        assert_eq!(read(interpreter, "d"), bool!(true));
        assert_eq!(read(interpreter, "e"), bool!(false));
        assert_eq!(read(interpreter, "f"), bool!(false));
        assert_eq!(read(interpreter, "g"), bool!(true));
        assert_eq!(read(interpreter, "h"), bool!(false));
        assert_eq!(read(interpreter, "i"), bool!(true));
        assert_eq!(read(interpreter, "j"), bool!(true));

        let input = r#"
issubclass([], type)
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(&e, "issubclass() arg 1 must be a class");

        let input = r#"
issubclass(object, [])
"#;

        let mut context = init(input);
        let e = run_expect_error(&mut context);

        test_utils::assert_type_error(
            &e,
            "issubclass() arg 2 must be a type, a tuple of types, or a union",
        );
    }

    #[test]
    fn bool_builtin() {
        let input = r#"
a = bool()
b = bool(True)
c = bool(False)
d = bool([])
e = bool([1])
f = bool('')
g = bool('hello')
h = bool(0)
i = bool(5)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), bool!(false));
        assert_eq!(read(interpreter, "b"), bool!(true));
        assert_eq!(read(interpreter, "c"), bool!(false));
        assert_eq!(read(interpreter, "d"), bool!(false));
        assert_eq!(read(interpreter, "e"), bool!(true));
        assert_eq!(read(interpreter, "f"), bool!(false));
        assert_eq!(read(interpreter, "g"), bool!(true));
        assert_eq!(read(interpreter, "h"), bool!(false));
        assert_eq!(read(interpreter, "i"), bool!(true));
    }

    #[test]
    fn memoryview_builtin() {
        let input = r#"
a = memoryview
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_is!(interpreter, "a", Class);
    }

    #[test]
    fn yield_from() {
        let input = r#"
def countdown(n):
    while n > 0:
        yield n
        n -= 1

# def countdown_from(x, y):
#     yield from countdown(x)
#     yield from countdown(y)

def countdown_from(x, y):
    for number in countdown(x):
        yield number
    for number in countdown(y):
        yield number

sum = 0
for number in countdown_from(3, 2):
    print(number)
    sum += number
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "sum"), int!(9));
    }

    #[test]
    fn traceback_and_frame() {
        let input = r#"
try:
    raise TypeError
except TypeError as exc:
    a = type(exc.__traceback__)
    b = type(exc.__traceback__.tb_frame)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_type_class(interpreter, "a", Type::Traceback);
        assert_type_class(interpreter, "b", Type::Frame);
    }

    #[test]
    fn asyncio() {
        let input = r#"
a = asyncio.run
b = asyncio.sleep
c = asyncio.create_task
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        // these should probably just return Function, not BuiltinFunction
        // testing here to confirm they do not get bound to their module
        assert_eq!(read_type(interpreter, "a"), Type::BuiltinFunction);
        assert_eq!(read_type(interpreter, "b"), Type::BuiltinFunction);
        assert_eq!(read_type(interpreter, "c"), Type::BuiltinFunction);
    }

    #[test]
    fn multiple_assignment() {
        let input = r#"
a = b = True
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), bool!(true));
        assert_eq!(read(interpreter, "b"), bool!(true));
    }

    #[test]
    fn object_equality() {
        let input = r#"
class Foo: pass

f = Foo()
g = Foo()
a = f == g
b = f != g
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), bool!(false));
        assert_eq!(read(interpreter, "b"), bool!(true));

        let input = r#"
class Foo:
    def __init__(self, x):
        self.x = x

f = Foo(4)
g = Foo(4)
a = f == g
b = f != g
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), bool!(false));
        assert_eq!(read(interpreter, "b"), bool!(true));

        let input = r#"
class Foo:
    def __init__(self, x):
        self.x = x

    def __eq__(self, other):
        self.x == other.x

f = Foo(4)
g = Foo(4)
a = f == g
b = f != g
c = f.__ne__
d = c(g)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), bool!(true));
        assert_eq!(read(interpreter, "b"), bool!(false));
        let ExprResult::Method(method) = read(interpreter, "c") else {
            panic!("Expected a method!");
        };
        assert!(matches!(method.receiver(), Some(ExprResult::Object(_))));
        assert_eq!(read(interpreter, "d"), bool!(false));
    }

    #[test]
    fn descriptor_protocol() {
        let input = r#"
class MyDescriptor:
    def __get__(self, instance, owner):
        return 4 * instance.val

class MyClass:
    attribute = MyDescriptor()

    def __init__(self):
        self.val = 11

obj = MyClass()
a = obj.attribute
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(44));

        let input = r#"
class Descriptor:
    def __init__(self, default):
        self.default = default
        self.data = {}

    def __get__(self, instance, _owner):
        if instance is None:
            return self
        return self.data.get(instance, self.default)

    def __set__(self, instance, value):
        self.data[instance] = value

    def __delete__(self, instance):
        if instance in self.data:
            del self.data[instance]

class MyClass:
    my_attr = Descriptor('default value')

    def __init__(self, value=None):
        if value:
            self.my_attr = value

obj = MyClass()
a = obj.my_attr

obj.my_attr = 'new value'
b = obj.my_attr

del obj.my_attr
c = obj.my_attr

d = MyClass.my_attr

obj2 = MyClass('custom value')
e = obj2.my_attr

del obj.my_attr
f = obj.my_attr
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), str!("default value"));
        assert_eq!(read(interpreter, "b"), str!("new value"));
        assert_eq!(read(interpreter, "c"), str!("default value"));
        assert_eq!(
            read(interpreter, "d")
                .get_class(interpreter)
                .borrow()
                .name(),
            "Descriptor"
        );
        assert_eq!(read(interpreter, "e"), str!("custom value"));
        assert_eq!(read(interpreter, "f"), str!("default value"));
    }

    #[test]
    fn complex() {
        let input = r#"
a = complex(4, 5)
b = type(a)
c = complex()
d = complex(1)
e = complex("2+3j")
f = complex("2.1+3.1j")
g = complex(4.1, 5.1)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), complex!(4.0, 5.0));
        assert_type_class(interpreter, "b", Type::Complex);
        assert_eq!(read(interpreter, "c"), complex!(0.0, 0.0));
        assert_eq!(read(interpreter, "d"), complex!(1.0, 0.0));
        assert_eq!(read(interpreter, "e"), complex!(2.0, 3.0));
        assert_eq!(read(interpreter, "f"), complex!(2.1, 3.1));
        assert_eq!(read(interpreter, "g"), complex!(4.1, 5.1));
    }

    #[test]
    fn callable_builtin() {
        let input = r#"
def foo(): pass

class MyClass: pass

a = callable(4)
b = callable(foo)
c = callable(MyClass)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), bool!(false));
        assert_eq!(read(interpreter, "b"), bool!(true));
        assert_eq!(read(interpreter, "c"), bool!(true));
    }

    #[test]
    fn dir_builtin() {
        let input = r#"
class MyClass:
    def __init__(self):
        self.a = 0
        self.b = 1

my = MyClass()
a = dir(my)
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), list![str!("a"), str!("b"),]);
    }

    #[test]
    fn getitem_builtin() {
        let input = r#"
class MyDict:
    def __init__(self):
        self.inner = {}

    def __getitem__(self, key):
        return self.inner[key]

    def __setitem__(self, key, value):
        self.inner[key] = value

    def __delitem__(self, key):
        del self.inner[key]

my = MyDict()
my['one'] = 1
a = my['one']
del my['one']
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(1));
        assert_member_eq!(interpreter, "my", "inner", dict!(interpreter, {}));
    }

    #[test]
    fn hash_builtin() {
        let input = r#"
a = hash(5)
b = hash(complex)

class Foo:
    def __hash__(self):
        return 4.5

c = hash(Foo)

the_dict = {}
the_dict[complex] = True
d = the_dict[complex]

try:
    hash(Foo())
except Exception as e:
    the_exp = e
"#;

        let mut context = init(input);
        let interpreter = run(&mut context);

        assert_eq!(read(interpreter, "a"), int!(5));

        let b = extract!(interpreter, "c", Integer);
        assert!(b != 0);

        let c = extract!(interpreter, "c", Integer);
        assert!(c != 0);

        assert_eq!(read(interpreter, "d"), bool!(true));

        let e = extract!(interpreter, "the_exp", Exception);
        test_utils::assert_type_error(&e, "__hash__ method should return an integer");
    }
}
