use std::collections::{HashMap, HashSet};
use std::fmt::Write;

#[cfg(feature = "c_stdlib")]
use super::types::cpython::import_from_cpython;
use super::{evaluators, Scope, StackFrame, State};
use crate::{
    core::{log, Container, InterpreterEntrypoint, LogLevel},
    parser::{
        types::{
            Ast, BinOp, CompoundOperator, ConditionalBlock, DictOperation, ExceptClause,
            ExceptionInstance, ExceptionLiteral, Expr, FStringPart, ForClause, ImportPath,
            ImportedItem, LogicalOp, LoopIndex, ParsedArgDefinitions, ParsedArguments,
            ParsedSliceParams, RegularImport, Statement, TypeNode, UnaryOp, Variable,
        },
        Parser,
    },
    resolved_args,
    treewalk::types::{
        domain::traits::{Callable, MemberReader},
        function::FunctionType,
        iterators::GeneratorIterator,
        utils::{Dunder, ResolvedArguments},
        Class, Coroutine, Dict, ExprResult, Function, Generator, List, Module, Set, Slice, Str,
        Tuple,
    },
    types::errors::{InterpreterError, MemphisError},
};

#[derive(Clone)]
pub struct Interpreter {
    pub state: Container<State>,
}

impl Interpreter {
    pub fn new(state: Container<State>) -> Self {
        Interpreter { state }
    }

    pub fn call(
        &self,
        callable: Container<Box<dyn Callable>>,
        arguments: &ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        let mut bound_args = arguments.clone();
        if let Some(receiver) = callable.borrow().receiver() {
            bound_args.bind(receiver);
        }

        match callable.borrow().call(self, bound_args) {
            Err(InterpreterError::EncounteredReturn(result)) => Ok(result),
            Err(e) => Err(e),
            Ok(result) => Ok(result),
        }
    }

    pub fn call_function(
        &self,
        name: &str,
        arguments: &ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        let function = self.read_callable(name)?;
        self.call_function_inner(function, arguments)
    }

    pub fn invoke_function(
        &self,
        function: Container<Function>,
        scope: Container<Scope>,
    ) -> Result<ExprResult, InterpreterError> {
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
        self.state
            .push_context(StackFrame::from_function(function.borrow().clone()));
        self.state.push_function(function.clone());

        // We do not propagate errors here because we still must restore the scopes and things
        // before returning.
        let result = self.evaluate_ast(&function.borrow().body);

        // If an error is thrown, we should return that immediately without restoring any state.
        if matches!(result, Ok(_) | Err(InterpreterError::EncounteredReturn(_))) {
            self.state.pop_context();
            self.state.pop_function();
            self.state.pop_local();
            self.state.pop_captured_env();
            if cross_module {
                self.state.pop_module();
            }
        }

        result
    }

    pub fn resolve_method(
        &self,
        receiver: ExprResult,
        name: &str,
    ) -> Result<Container<Box<dyn Callable>>, InterpreterError> {
        self.evaluate_member_access_inner(&receiver, name)?
            .as_callable()
            .ok_or(InterpreterError::MethodNotFound(
                name.to_string(),
                self.state.call_stack(),
            ))
    }

    pub fn invoke_method(
        &self,
        receiver: ExprResult,
        name: &str,
        arguments: &ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        log(LogLevel::Debug, || {
            format!("Calling method {}.{}", receiver, name)
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
    ) -> Result<ExprResult, InterpreterError> {
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
                    .ok_or(InterpreterError::ExpectedFunction(self.state.call_stack()))?;
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
                    .ok_or(InterpreterError::ExpectedFunction(self.state.call_stack()))?;
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

    fn read_callable(&self, name: &str) -> Result<Container<Box<dyn Callable>>, InterpreterError> {
        self.state
            .read(name)
            .and_then(|val| val.as_callable())
            .ok_or(InterpreterError::FunctionNotFound(
                name.to_string(),
                self.state.call_stack(),
            ))
    }

    pub fn read_index(
        &self,
        object: &ExprResult,
        index: &ExprResult,
    ) -> Result<ExprResult, InterpreterError> {
        object
            .as_index_read(self)
            .ok_or(InterpreterError::TypeError(
                Some(format!(
                    "'{}' object is not subscriptable",
                    object.get_type()
                )),
                self.state.call_stack(),
            ))?
            .getitem(self, index.clone())?
            .ok_or(InterpreterError::KeyError(
                index.to_string(),
                self.state.call_stack(),
            ))
    }

    fn evaluate_binary_operation_inner(
        &self,
        left: ExprResult,
        op: &BinOp,
        right: ExprResult,
    ) -> Result<ExprResult, InterpreterError> {
        if matches!(op, BinOp::In) {
            if let Some(mut iterable) = right.try_into_iter() {
                return Ok(ExprResult::Boolean(iterable.contains(left)));
            }
            return Err(InterpreterError::ExpectedIterable(self.state.call_stack()));
        }

        if matches!(op, BinOp::NotIn) {
            if let Some(mut iterable) = right.try_into_iter() {
                return Ok(ExprResult::Boolean(!iterable.contains(left)));
            }
            return Err(InterpreterError::ExpectedIterable(self.state.call_stack()));
        }

        if left.is_integer() && right.is_integer() {
            left.as_integer()
                .and_then(|left| {
                    right.as_integer().map(|right| {
                        evaluators::evaluate_integer_operation(
                            left,
                            op,
                            right,
                            self.state.call_stack(),
                        )
                    })
                })
                .ok_or(InterpreterError::ExpectedInteger(self.state.call_stack()))?
        } else if left.is_fp() && right.is_fp() {
            left.as_fp()
                .and_then(|left| {
                    right.as_fp().map(|right| {
                        evaluators::evaluate_floating_point_operation(
                            left,
                            op,
                            right,
                            self.state.call_stack(),
                        )
                    })
                })
                .ok_or(InterpreterError::ExpectedFloatingPoint(
                    self.state.call_stack(),
                ))?
        } else if left.as_list().is_some() && right.as_list().is_some() {
            left.as_list()
                .and_then(|left| {
                    right
                        .as_list()
                        .map(|right| evaluators::evaluate_list_operation(left, op, right))
                })
                .ok_or(InterpreterError::ExpectedIterable(self.state.call_stack()))?
        } else if left.as_set().is_some() && right.as_set().is_some() {
            left.as_set()
                .and_then(|left| {
                    right
                        .as_set()
                        .map(|right| evaluators::evaluate_set_operation(left, op, right))
                })
                .ok_or(InterpreterError::ExpectedSet(self.state.call_stack()))?
        } else if left.as_object().is_some()
            && right.as_object().is_some()
            && Dunder::try_from(op).is_ok()
        {
            let dunder = Dunder::try_from(op).unwrap_or_else(|_| unreachable!());
            self.invoke_method(left, &dunder, &resolved_args!(right))
        } else {
            evaluators::evaluate_object_comparison(left, op, right)
        }
    }

    fn evaluate_member_access_inner(
        &self,
        result: &ExprResult,
        field: &str,
    ) -> Result<ExprResult, InterpreterError> {
        log(LogLevel::Debug, || {
            format!("Member access {}.{}", result, field)
        });
        result
            .as_member_reader(self)
            .get_member(self, field)?
            .ok_or(InterpreterError::AttributeError(
                result.get_class(self).borrow().name().to_string(),
                field.to_string(),
                self.state.call_stack(),
            ))
    }

    // -----------------------------
    // End of medium-order functions
    // -----------------------------

    fn evaluate_module_import(
        &self,
        import_path: &ImportPath,
    ) -> Result<ExprResult, InterpreterError> {
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

    fn evaluate_variable(&self, name: &str) -> Result<ExprResult, InterpreterError> {
        self.state.read(name).ok_or(InterpreterError::NameError(
            name.to_owned(),
            self.state.call_stack(),
        ))
    }

    fn evaluate_unary_operation(
        &self,
        op: &UnaryOp,
        right: &Expr,
    ) -> Result<ExprResult, InterpreterError> {
        let right = self.evaluate_expr(right)?;
        evaluators::evaluate_unary_operation(op, right, self.state.call_stack())
    }

    fn evaluate_ternary_operation(
        &self,
        condition: &Expr,
        if_value: &Expr,
        else_value: &Expr,
    ) -> Result<ExprResult, InterpreterError> {
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
    ) -> Result<ExprResult, InterpreterError> {
        let left = self.evaluate_expr(left)?.as_boolean();
        let right = self.evaluate_expr(right)?.as_boolean();
        evaluators::evaluate_logical_op(left, op, right)
    }

    fn evaluate_binary_operation(
        &self,
        left: &Expr,
        op: &BinOp,
        right: &Expr,
    ) -> Result<ExprResult, InterpreterError> {
        let left = self.evaluate_expr(left)?;
        let right = self.evaluate_expr(right)?;

        self.evaluate_binary_operation_inner(left, op, right)
    }

    fn evaluate_member_access(
        &self,
        object: &Expr,
        field: &str,
    ) -> Result<ExprResult, InterpreterError> {
        let result = self.evaluate_expr(object)?;
        self.evaluate_member_access_inner(&result, field)
    }

    fn evaluate_slice_operation(
        &self,
        object: &Expr,
        params: &ParsedSliceParams,
    ) -> Result<ExprResult, InterpreterError> {
        let object_result = self.evaluate_expr(object)?;
        let slice = Slice::resolve(self, params)?;

        self.read_index(&object_result, &ExprResult::Slice(slice))
    }

    fn evaluate_index_access(
        &self,
        object: &Expr,
        index: &Expr,
    ) -> Result<ExprResult, InterpreterError> {
        let object_result = self.evaluate_expr(object)?;
        let index_result = self.evaluate_expr(index)?;

        self.read_index(&object_result, &index_result)
    }

    fn evaluate_list(&self, items: &[Expr]) -> Result<ExprResult, InterpreterError> {
        items
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<Vec<_>, _>>()
            .map(|l| ExprResult::List(Container::new(List::new(l))))
    }

    fn evaluate_tuple(&self, items: &[Expr]) -> Result<ExprResult, InterpreterError> {
        let mut results = vec![];
        for item in items {
            let evaluated = self.evaluate_expr(item)?;
            match item {
                Expr::UnaryOperation {
                    op: UnaryOp::Unpack,
                    ..
                } => {
                    let list: Container<List> = evaluated.try_into()?;
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

    fn evaluate_set(&self, items: &HashSet<Expr>) -> Result<ExprResult, InterpreterError> {
        items
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<HashSet<_>, _>>()
            .map(Set::new)
            .map(Container::new)
            .map(ExprResult::Set)
    }

    fn evaluate_dict(&self, dict_ops: &[DictOperation]) -> Result<ExprResult, InterpreterError> {
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

    fn evaluate_await(&self, expr: &Expr) -> Result<ExprResult, InterpreterError> {
        let coroutine_to_await = self
            .evaluate_expr(expr)?
            .as_coroutine()
            .ok_or(InterpreterError::ExpectedCoroutine(self.state.call_stack()))?;

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
            Err(InterpreterError::EncounteredAwait)
        } else {
            Err(InterpreterError::ExpectedCoroutine(self.state.call_stack()))
        }
    }

    fn evaluate_delete(&self, exprs: &[Expr]) -> Result<(), InterpreterError> {
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
                        .ok_or(InterpreterError::TypeError(
                            Some(format!(
                                "'{}' object does not support item deletion",
                                object_result.get_type()
                            )),
                            self.state.call_stack(),
                        ))?
                        .delitem(self, index_result)?;
                }
                Expr::MemberAccess { object, field } => {
                    let result = self.evaluate_expr(object)?;
                    result
                        .as_member_writer()
                        .ok_or(InterpreterError::AttributeError(
                            result.get_type().to_string(),
                            field.to_string(),
                            self.state.call_stack(),
                        ))?
                        .delete_member(self, field)?;
                }
                _ => return Err(InterpreterError::ExpectedVariable(self.state.call_stack())),
            }
        }

        Ok(())
    }

    fn evaluate_return(&self, exprs: &[Expr]) -> Result<ExprResult, InterpreterError> {
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

        Err(InterpreterError::EncounteredReturn(return_val))
    }

    fn evaluate_assert(&self, expr: &Expr) -> Result<(), InterpreterError> {
        if self.evaluate_expr(expr)?.as_boolean() {
            Ok(())
        } else {
            Err(InterpreterError::AssertionError(self.state.call_stack()))
        }
    }

    fn evaluate_f_string(&self, parts: &[FStringPart]) -> Result<ExprResult, InterpreterError> {
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

    fn evaluate_ast(&self, ast: &Ast) -> Result<ExprResult, InterpreterError> {
        let mut result = ExprResult::None;
        for statement in ast.iter() {
            result = self.evaluate_statement(statement)?;
        }
        Ok(result)
    }

    fn evaluate_function_call(
        &self,
        name: &str,
        arguments: &ParsedArguments,
        callee: &Option<Box<Expr>>,
    ) -> Result<ExprResult, InterpreterError> {
        let arguments = ResolvedArguments::from(self, arguments)?;

        let function =
            if let Some(callee) = callee {
                self.evaluate_expr(callee)?.as_callable().ok_or(
                    InterpreterError::FunctionNotFound("<callee>".into(), self.state.call_stack()),
                )?
            } else {
                self.read_callable(name)?
            };

        self.call_function_inner(function, &arguments)
    }

    fn evaluate_method_call(
        &self,
        obj: &Expr,
        name: &str,
        arguments: &ParsedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        let arguments = ResolvedArguments::from(self, arguments)?;
        let result = self.evaluate_expr(obj)?;

        self.invoke_method(result, name, &arguments)
    }

    fn evaluate_class_instantiation(
        &self,
        name: &str,
        arguments: &ParsedArguments,
    ) -> Result<ExprResult, InterpreterError> {
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

        let result = self
            .state
            .read(name)
            .ok_or(InterpreterError::ClassNotFound(
                name.to_string(),
                self.state.call_stack(),
            ))?;
        let class = result.as_callable().ok_or(InterpreterError::ClassNotFound(
            name.to_string(),
            self.state.call_stack(),
        ))?;

        let arguments = ResolvedArguments::from(self, arguments)?;

        self.call(class, &arguments)
    }

    fn evaluate_compound_assignment(
        &self,
        operator: &CompoundOperator,
        target: &Expr,
        value: &Expr,
    ) -> Result<(), InterpreterError> {
        let bin_op = BinOp::from(operator);
        let result = self.evaluate_binary_operation(target, &bin_op, value)?;
        self.evaluate_assignment_inner(target, result)
    }

    /// Assignment functionality shared by traditional assignment such as `a = 1` and compound
    /// assignment such as `a += 1`.
    fn evaluate_assignment_inner(
        &self,
        name: &Expr,
        value: ExprResult,
    ) -> Result<(), InterpreterError> {
        match name {
            Expr::Variable(name) => {
                self.state.write(name, value.clone());
            }
            Expr::IndexAccess { object, index } => {
                let index_result = self.evaluate_expr(index)?;
                let object_result = self.evaluate_expr(object)?;
                object_result
                    .as_index_write(self)
                    .ok_or(InterpreterError::TypeError(
                        Some(format!(
                            "'{}' object does not support item assignment",
                            object_result.get_type()
                        )),
                        self.state.call_stack(),
                    ))?
                    .setitem(self, index_result, value)?;
            }
            Expr::MemberAccess { object, field } => {
                let result = self.evaluate_expr(object)?;
                result
                    .as_member_writer()
                    .ok_or(InterpreterError::AttributeError(
                        result.get_type().to_string(),
                        field.to_string(),
                        self.state.call_stack(),
                    ))?
                    .set_member(self, field, value)?;
            }
            _ => return Err(InterpreterError::ExpectedVariable(self.state.call_stack())),
        }

        Ok(())
    }

    fn evaluate_assignment(&self, name: &Expr, expr: &Expr) -> Result<(), InterpreterError> {
        let result = self.evaluate_expr(expr)?;
        self.evaluate_assignment_inner(name, result)
    }

    fn evaluate_multiple_assignment(
        &self,
        left: &[Expr],
        expr: &Expr,
    ) -> Result<(), InterpreterError> {
        let value = self.evaluate_expr(expr)?;
        for name in left {
            self.evaluate_assignment_inner(name, value.clone())?;
        }

        Ok(())
    }

    /// Python can unpack any iterables, not any index reads.
    fn evaluate_unpacking_assignment(
        &self,
        left: &[Expr],
        expr: &Expr,
    ) -> Result<(), InterpreterError> {
        let results = self.evaluate_expr(expr)?.into_iter();
        let right_len = results.clone().count();
        let left_len = left.len();

        if left_len < right_len {
            return Err(InterpreterError::ValueError(
                "too many values to unpack (expected ".to_string() + &left_len.to_string() + ")",
                self.state.call_stack(),
            ));
        }

        if left.len() > right_len {
            return Err(InterpreterError::ValueError(
                "not enough values to unpack (expected ".to_string()
                    + &left_len.to_string()
                    + ", got "
                    + &right_len.to_string()
                    + ")",
                self.state.call_stack(),
            ));
        }

        for (key, value) in left.iter().zip(results) {
            self.evaluate_assignment_inner(key, value)?;
        }

        Ok(())
    }

    fn evaluate_lambda(
        &self,
        arguments: &ParsedArgDefinitions,
        expr: &Expr,
    ) -> Result<ExprResult, InterpreterError> {
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
        arguments: &ParsedArgDefinitions,
        body: &Ast,
        decorators: &[Expr],
        is_async: &bool,
    ) -> Result<(), InterpreterError> {
        let function = Container::new(Function::new(
            self.state.clone(),
            name.to_string(),
            arguments.clone(),
            body.clone(),
            decorators.to_vec(),
            *is_async,
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
    ) -> Result<(), InterpreterError> {
        log(LogLevel::Debug, || format!("Defining class: {}", name));
        let parent_classes = parents
            .iter()
            .map(|p| self.evaluate_expr(p))
            .collect::<Result<Vec<_>, _>>()?
            .iter()
            .map(|f| {
                f.as_class()
                    .ok_or(InterpreterError::ExpectedClass(self.state.call_stack()))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let metaclass = metaclass
            .clone()
            .and_then(|p| self.state.read(p.as_str()))
            .map(|d| {
                d.as_class()
                    .ok_or(InterpreterError::ExpectedClass(self.state.call_stack()))
            })
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
            .ok_or(InterpreterError::RuntimeError)?
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
    ) -> Result<(), InterpreterError> {
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

    fn evaluate_while_loop(&self, condition: &Expr, body: &Ast) -> Result<(), InterpreterError> {
        while self.evaluate_expr(condition)?.as_boolean() {
            match self.evaluate_ast(body) {
                Err(InterpreterError::EncounteredBreak) => {
                    break;
                }
                Err(InterpreterError::EncounteredContinue) => {}
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
    ) -> Result<ExprResult, InterpreterError> {
        let generator = Generator::new_from_comprehension(self.state.clone(), body, clauses);
        let iterator = GeneratorIterator::new(generator, self.clone());
        Ok(ExprResult::Generator(Container::new(iterator)))
    }

    fn evaluate_list_comprehension(
        &self,
        body: &Expr,
        clauses: &[ForClause],
    ) -> Result<ExprResult, InterpreterError> {
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
    ) -> Result<ExprResult, InterpreterError> {
        self.evaluate_list_comprehension(body, clauses)?
            .try_into()
            .map_err(|_| InterpreterError::ExpectedSet(self.state.call_stack()))
            .map(ExprResult::Set)
    }

    fn evaluate_dict_comprehension(
        &self,
        clauses: &[ForClause],
        key_body: &Expr,
        value_body: &Expr,
    ) -> Result<ExprResult, InterpreterError> {
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
    ) -> Result<(), InterpreterError> {
        let range_expr = self.evaluate_expr(range)?;
        let mut encountered_break = false;

        for val_for_iteration in range_expr {
            self.state.write_loop_index(index, val_for_iteration);

            match self.evaluate_ast(body) {
                Err(InterpreterError::EncounteredBreak) => {
                    encountered_break = true;
                    break;
                }
                Err(InterpreterError::EncounteredContinue) => {}
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

    fn evaluate_regular_import(&self, items: &[RegularImport]) -> Result<(), InterpreterError> {
        for item in items.iter() {
            self.evaluate_regular_import_inner(&item.import_path, &item.alias)?;
        }

        Ok(())
    }

    fn evaluate_regular_import_inner(
        &self,
        import_path: &ImportPath,
        alias: &Option<String>,
    ) -> Result<(), InterpreterError> {
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
    ) -> Result<(), InterpreterError> {
        let module = self
            .evaluate_module_import(import_path)?
            .as_module()
            .ok_or(InterpreterError::ModuleNotFound(
                import_path.as_str().to_string(),
                self.state.call_stack(),
            ))?;

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
                return Err(InterpreterError::FunctionNotFound(
                    aliased_symbol.to_string(),
                    self.state.call_stack(),
                ));
            }
        }

        Ok(())
    }

    fn evaluate_context_manager(
        &self,
        expr: &Expr,
        variable: &Option<String>,
        block: &Ast,
    ) -> Result<(), InterpreterError> {
        let expr_result = self.evaluate_expr(expr)?;

        let object = expr_result
            .as_object()
            .ok_or(InterpreterError::ExpectedObject(self.state.call_stack()))?;

        if object.get_member(self, &Dunder::Enter)?.is_none()
            || object.get_member(self, &Dunder::Exit)?.is_none()
        {
            return Err(InterpreterError::MissingContextManagerProtocol(
                self.state.call_stack(),
            ));
        }

        let result = self.invoke_method(expr_result.clone(), &Dunder::Enter, &resolved_args!())?;

        if let Some(variable) = variable {
            self.state.write(variable, result);
        }
        let block_result = self.evaluate_ast(block);

        self.invoke_method(
            expr_result.clone(),
            &Dunder::Exit,
            &resolved_args!(ExprResult::None, ExprResult::None, ExprResult::None),
        )?;

        // Return the exception if one is called.
        block_result?;

        Ok(())
    }

    fn evaluate_raise(&self, instance: &Option<ExceptionInstance>) -> Result<(), InterpreterError> {
        // TODO we should throw a 'RuntimeError: No active exception to reraise'
        if instance.is_none() {
            return Err(InterpreterError::EncounteredRaise);
        }

        let instance = instance.as_ref().unwrap();
        let args = ResolvedArguments::from(self, &instance.args)?;
        let error = match instance.literal {
            ExceptionLiteral::TypeError => {
                let message = if args.len() == 1 {
                    Some(
                        args.get_arg(0)
                            .as_string()
                            .ok_or(InterpreterError::ExpectedString(self.state.call_stack()))?,
                    )
                } else {
                    None
                };

                InterpreterError::TypeError(message, self.state.call_stack())
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
    ) -> Result<(), InterpreterError> {
        if let Err(error) = self.evaluate_ast(try_block) {
            // Only the first matching clause should be evaluated. They will still be in order
            // here from the parsed code.
            if let Some(except_clause) = except_clauses
                .iter()
                .find(|clause| error.matches_except_clause(&clause.exception_types))
            {
                if let Some(alias) = &except_clause.alias {
                    self.state
                        .write(alias, ExprResult::Exception(Box::new(error.clone())));
                }

                match self.evaluate_ast(&except_clause.block) {
                    Err(InterpreterError::EncounteredRaise) => return Err(error),
                    Err(second_error) => return Err(second_error),
                    Ok(_) => {}
                }
            } else {
                // Uncaught errors should be raised
                return Err(error);
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
    fn validate_nonlocal_context(&self, name: &str) -> Result<(), InterpreterError> {
        // We could not find the variable `name` in an enclosing context.
        if let Some(env) = self.state.read_captured_env() {
            if env.borrow().read(name).is_none() {
                return Err(InterpreterError::SyntaxError(self.state.call_stack()));
            }
        }

        // `nonlocal` cannot be used at the module-level (outside of a function,
        // i.e. captured environment).
        if self.state.read_captured_env().is_none() {
            return Err(InterpreterError::SyntaxError(self.state.call_stack()));
        }

        Ok(())
    }

    fn evaluate_nonlocal(&self, names: &[Variable]) -> Result<(), InterpreterError> {
        for name in names {
            self.validate_nonlocal_context(name)?;
            self.state.mark_nonlocal(name);
        }

        Ok(())
    }

    fn evaluate_global(&self, names: &[Variable]) -> Result<(), InterpreterError> {
        for name in names {
            self.state.mark_global(name);
        }

        Ok(())
    }

    fn evaluate_type_node(&self, type_node: &TypeNode) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::TypeNode(type_node.into()))
    }

    pub fn evaluate_expr(&self, expr: &Expr) -> Result<ExprResult, InterpreterError> {
        match expr {
            Expr::None => Ok(ExprResult::None),
            Expr::Ellipsis => Ok(ExprResult::Ellipsis),
            Expr::NotImplemented => Ok(ExprResult::NotImplemented),
            Expr::Integer(value) => Ok(ExprResult::Integer(*value)),
            Expr::FloatingPoint(value) => Ok(ExprResult::FloatingPoint(*value)),
            Expr::Boolean(value) => Ok(ExprResult::Boolean(*value)),
            Expr::StringLiteral(value) => Ok(ExprResult::String(Str::new(value.clone()))),
            Expr::ByteStringLiteral(value) => Ok(ExprResult::Bytes(value.clone())),
            Expr::Variable(name) => self.evaluate_variable(name),
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
            Expr::Await { right } => self.evaluate_await(right),
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

    pub fn evaluate_statement(&self, stmt: &Statement) -> Result<ExprResult, InterpreterError> {
        // These are the only types of statements that will return a value.
        match stmt {
            Statement::Expression(expr) => return self.evaluate_expr(expr),
            Statement::Return(expr) => return self.evaluate_return(expr),
            _ => {}
        };

        let result = match stmt {
            // These are handled above
            Statement::Expression(_) | Statement::Return(_) => unreachable!(),
            Statement::Pass => Ok(()),
            Statement::Break => Err(InterpreterError::EncounteredBreak),
            Statement::Continue => Err(InterpreterError::EncounteredContinue),
            Statement::Assert(expr) => self.evaluate_assert(expr),
            Statement::Delete(expr) => self.evaluate_delete(expr),
            Statement::Nonlocal(names) => self.evaluate_nonlocal(names),
            Statement::Global(names) => self.evaluate_global(names),
            Statement::Assignment { left, right } => self.evaluate_assignment(left, right),
            Statement::MultipleAssignment { left, right } => {
                self.evaluate_multiple_assignment(left, right)
            }
            Statement::UnpackingAssignment { left, right } => {
                self.evaluate_unpacking_assignment(left, right)
            }
            Statement::CompoundAssignment {
                operator,
                target,
                value,
            } => self.evaluate_compound_assignment(operator, target, value),
            Statement::FunctionDef {
                name,
                args,
                body,
                decorators,
                is_async,
            } => self.evaluate_function_def(name, args, body, decorators, is_async),
            Statement::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => self.evaluate_if_else(if_part, elif_parts, else_part),
            Statement::WhileLoop { condition, body } => self.evaluate_while_loop(condition, body),
            Statement::ForInLoop {
                index,
                iterable: range,
                body,
                else_block,
            } => self.evaluate_for_in_loop(index, range, body, else_block),
            Statement::ClassDef {
                name,
                parents,
                metaclass,
                body,
            } => self.evaluate_class_definition(name, parents, metaclass, body),
            Statement::RegularImport(items) => self.evaluate_regular_import(items),
            Statement::SelectiveImport {
                import_path,
                items,
                wildcard,
            } => self.evaluate_selective_import(import_path, items, wildcard),
            Statement::TryExcept {
                try_block,
                except_clauses,
                else_block,
                finally_block,
            } => self.evaluate_try_except(try_block, except_clauses, else_block, finally_block),
            Statement::Raise(exception) => self.evaluate_raise(exception),
            Statement::ContextManager {
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
            result = self
                .evaluate_statement(&stmt)
                .map_err(MemphisError::Interpreter)?;
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        init::MemphisContext,
        treewalk::types::{
            domain::Type, ByteArray, Complex, DictItems, DictKeys, DictValues, FrozenSet,
        },
        types::errors::ParserError,
    };

    fn init_path(path: &str) -> MemphisContext {
        MemphisContext::from_path(path)
    }

    fn init(text: &str) -> MemphisContext {
        MemphisContext::from_text(text)
    }

    fn evaluate(text: &str) -> Result<ExprResult, MemphisError> {
        init(text).evaluate_oneshot()
    }

    fn evaluate_and_expect(text: &str) -> ExprResult {
        evaluate(text).expect("Failed to evaluate test string!")
    }

    #[test]
    fn undefined_variable() {
        let input = "x + 1";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                let interpreter = context.ensure_treewalk();
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::NameError(
                        "x".to_string(),
                        interpreter.state.call_stack(),
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }
    }

    #[test]
    fn division_by_zero() {
        let input = "1 / 0";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::DivisionByZero(
                        "division by zero".into(),
                        context.ensure_treewalk().state.call_stack(),
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }
    }

    #[test]
    fn expression() {
        let input = "2 + 3 * (4 - 1)";
        assert_eq!(evaluate_and_expect(input), ExprResult::Integer(11));
    }

    #[test]
    fn integer_division() {
        let input = "2 // 3";
        assert_eq!(evaluate_and_expect(input), ExprResult::Integer(0));

        let input = "5 // 3";
        assert_eq!(evaluate_and_expect(input), ExprResult::Integer(1));

        let input = "5 // 0";
        let result = evaluate(input);
        let Err(MemphisError::Interpreter(InterpreterError::DivisionByZero(msg, _))) = result
        else {
            panic!("Expected DivisionByZero error");
        };
        assert_eq!(msg, "integer division or modulo by zero",);
    }

    #[test]
    fn integer_assignment() {
        let input = r#"
a = 2 + 3 * 4
b = a + 5
c = None
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(14)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(19)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::None));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::String(Str::new("foo".to_string())))
                );
                assert_eq!(
                    interpreter.state.read("b").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::BuiltinMethod)
                );
                assert_eq!(
                    interpreter.state.read("c").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Method)
                );
                assert_eq!(
                    interpreter.state.read("d").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::BuiltinMethod)
                );
            }
        }
    }

    #[test]
    fn boolean_operators() {
        let input = "True and False";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(false));

        let input = "True or False";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "not (True or False)";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(false));

        let input = "True and not False";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "not False";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "not True";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(false));
    }

    // Confirm that the interpreter can evaluate boolean expressions.
    #[test]
    fn comparison_operators() {
        let input = "2 == 1";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(false));

        let input = "2 == 2";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "2 != 1";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "2 != 2";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(false));

        let input = "2 > 1";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "2 < 1";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(false));

        let input = "1 <= 1";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "1 >= 1";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "4 in range(5)";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "4 in range(3)";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(false));

        let input = "4 not in range(5)";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(false));

        let input = "4 not in range(3)";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));

        let input = "4 is None";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(false));

        let input = "4 is not None";
        assert_eq!(evaluate_and_expect(input), ExprResult::Boolean(true));
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::BuiltinFunction)
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::StringIterator(_))
                ));
                assert_eq!(
                    interpreter.state.read("b").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::StringIterator)
                );
            }
        }
    }

    #[test]
    fn function_definition() {
        let input = r#"
def foo(a, b):
    return a + b

foo(2, 3)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                let expected_name = "foo".to_string();
                assert!(matches!(
                    interpreter.state.read("foo").unwrap().as_function().unwrap().borrow().clone(),
                    Function {
                        name,
                        ..
                    } if name == expected_name));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
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
                    &Ast::new(vec![Statement::Expression(Expr::Integer(4))])
                );
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(4)));
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
                    &Ast::new(vec![Statement::Expression(Expr::Yield(None))])
                );
                assert!(matches!(
                    interpreter.state.read("e").unwrap(),
                    ExprResult::Generator(_)
                ));
                assert_eq!(
                    interpreter.state.read("f").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Generator)
                );
                assert!(matches!(
                    interpreter.state.read("h").unwrap(),
                    ExprResult::Coroutine(_)
                ));
                // I commented this out when we removed Clone from Class.
                //assert!(matches!(
                //    interpreter.state.read("i").unwrap().as_class().unwrap().borrow(),
                //    Class { name, .. } if name == "coroutine"
                //));
                // TODO add support for async generators, which will change the next two assertions
                assert!(matches!(
                    interpreter.state.read("k").unwrap(),
                    ExprResult::Coroutine(_)
                ));
                //assert!(matches!(
                //    interpreter.state.read("l").unwrap().as_class().unwrap().borrow().clone(),
                //    Class { name, .. } if name == "coroutine"
                //));
                assert!(matches!(
                    interpreter.state.read("m").unwrap(),
                    ExprResult::Code(_)
                ));
                assert_eq!(
                    interpreter.state.read("n").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Code)
                );
                assert_eq!(
                    interpreter.state.read("o").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::GetSetDescriptor)
                );
                assert_eq!(
                    interpreter.state.read("p").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Dict)
                );
                assert_eq!(
                    interpreter.state.read("q").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::MemberDescriptor)
                );
                assert_eq!(
                    interpreter.state.read("r").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::None)
                );
                assert_eq!(
                    interpreter.state.read("s").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::MemberDescriptor)
                );
                assert_eq!(
                    interpreter.state.read("t").unwrap(),
                    ExprResult::String(Str::new("__main__".into()))
                );
                assert_eq!(
                    interpreter.state.read("u").unwrap(),
                    ExprResult::String(Str::new("".into()))
                );
                assert_eq!(
                    interpreter.state.read("v").unwrap(),
                    ExprResult::String(Str::new("_f".into()))
                );
                assert_eq!(
                    interpreter.state.read("w").unwrap(),
                    ExprResult::String(Str::new("_f".into()))
                );
                assert_eq!(
                    interpreter.state.read("x").unwrap(),
                    ExprResult::Dict(Container::new(Dict::default()))
                );
                assert_eq!(
                    interpreter.state.read("y").unwrap(),
                    ExprResult::Tuple(Tuple::default())
                );
            }
        }

        // Test early return
        let input = r#"
def foo():
    return 4
    return 5

a = foo()
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(4)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("z"),
                    Some(ExprResult::String(Str::new("Greater than 0".to_string())))
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("z"),
                    Some(ExprResult::String(Str::new("Greater than -10".to_string())))
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("z"),
                    Some(ExprResult::String(Str::new("Greater than -20".to_string())))
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("z"),
                    Some(ExprResult::String(Str::new("Empty".to_string())))
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("z"),
                    Some(ExprResult::String(Str::new("Else".to_string())))
                );
            }
        }

        let input = r#"
z = 0
if 4 in range(5):
    z = 1
else:
    z = 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("z"), Some(ExprResult::Integer(1)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("z"), Some(ExprResult::Integer(10)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(interpreter.state.is_class("Foo"));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(interpreter.state.is_class("Foo"));
                assert!(!interpreter.state.is_class("foo"));

                let foo = match interpreter.state.read("foo") {
                    Some(ExprResult::Object(o)) => o,
                    _ => panic!("Expected an object."),
                };
                assert_eq!(
                    foo.get_member(&interpreter, "y").unwrap(),
                    Some(ExprResult::Integer(3))
                );
                assert_eq!(
                    foo.get_member(&interpreter, "x").unwrap(),
                    Some(ExprResult::Integer(0))
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(interpreter.state.is_class("Foo"));
                assert!(!interpreter.state.is_class("foo"));

                // This should be an object with foo.y == 3 and foo.x == 0 even
                // when the last line of the constructor did not touch self.
                let foo = match interpreter.state.read("foo") {
                    Some(ExprResult::Object(o)) => o,
                    _ => panic!("Expected an object."),
                };
                assert_eq!(
                    foo.get_member(&interpreter, "y").unwrap(),
                    Some(ExprResult::Integer(3))
                );
                assert_eq!(
                    foo.get_member(&interpreter, "x").unwrap(),
                    Some(ExprResult::Integer(0))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("x"), Some(ExprResult::Integer(3)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(interpreter.state.is_class("Foo"));
                assert!(!interpreter.state.is_class("foo"));

                let foo = match interpreter.state.read("foo") {
                    Some(ExprResult::Object(o)) => o,
                    _ => panic!("Expected an object."),
                };

                // These should be set even when it's not a constructor
                assert_eq!(
                    foo.get_member(&interpreter, "y").unwrap(),
                    Some(ExprResult::Integer(3))
                );
                assert_eq!(
                    foo.get_member(&interpreter, "x").unwrap(),
                    Some(ExprResult::Integer(0))
                );
            }
        }
    }

    #[test]
    fn regular_import() {
        let mut context = init_path("src/fixtures/imports/regular_import.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("x"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("y"), Some(ExprResult::Integer(6)));
                // This previously returned [`Type::Method`], which was an issue with binding
                // classes (as callables) to their module.
                assert_eq!(interpreter.state.read("z").unwrap().get_type(), Type::Type);
            }
        }

        let mut context = init_path("src/fixtures/imports/regular_import_b.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("y"), Some(ExprResult::Integer(7)));
            }
        }

        let mut context = init_path("src/fixtures/imports/relative/main_b.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("x"), Some(ExprResult::Integer(2)));
            }
        }

        let mut context = init_path("src/fixtures/imports/relative/main_c.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("x"), Some(ExprResult::Integer(2)));
            }
        }
    }

    #[test]
    fn selective_import() {
        let mut context = init_path("src/fixtures/imports/selective_import_a.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("x"), Some(ExprResult::Integer(5)));
            }
        }

        let mut context = init_path("src/fixtures/imports/selective_import_b.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("y"), Some(ExprResult::Integer(6)));
                assert_eq!(interpreter.state.read("z"), Some(ExprResult::Integer(6)));
            }
        }

        let mut context = init_path("src/fixtures/imports/selective_import_c.py");

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::FunctionNotFound(
                        "something_third".to_string(),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let mut context = init_path("src/fixtures/imports/selective_import_d.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("z"), Some(ExprResult::Integer(8)));
            }
        }

        let mut context = init_path("src/fixtures/imports/selective_import_e.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("x"), Some(ExprResult::Integer(8)));
            }
        }

        let mut context = init_path("src/fixtures/imports/selective_import_f.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("y"), Some(ExprResult::Integer(6)));
                assert_eq!(interpreter.state.read("z"), Some(ExprResult::Integer(6)));
            }
        }

        let mut context = init_path("src/fixtures/imports/relative/main_a.py");

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("x"), Some(ExprResult::Integer(2)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::FloatingPoint(3.14))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::FloatingPoint(3.1425))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::FloatingPoint(6.1))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::FloatingPoint(5.9))
                );
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::Boolean(false))
                );
            }
        }

        let input = r#"
def add(x, y):
    return x + y

z = add(2.1, 3)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("z"),
                    Some(ExprResult::FloatingPoint(5.1))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::FloatingPoint(-3.14))
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(-3)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(-1)));
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::FloatingPoint(-2e-3))
                );
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Integer(-1)));
                assert_eq!(interpreter.state.read("f"), Some(ExprResult::Integer(-1)));
                assert_eq!(interpreter.state.read("g"), Some(ExprResult::Integer(-3)));
                assert_eq!(interpreter.state.read("h"), Some(ExprResult::Integer(-5)));
                assert_eq!(interpreter.state.read("i"), Some(ExprResult::Integer(3)));
                assert_eq!(interpreter.state.read("j"), Some(ExprResult::Integer(-3)));
            }
        }
    }

    #[test]
    fn call_stack() {
        let mut context = init_path("src/fixtures/call_stack/call_stack.py");

        match context.run_and_return_interpreter() {
            Err(e) => {
                let call_stack = context.ensure_treewalk().state.call_stack();
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::FunctionNotFound(
                        "unknown".to_string(),
                        call_stack.clone(),
                    ))
                );

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

                assert_eq!(call_stack.get(0).function_name(), "<module>");
                assert_eq!(call_stack.get(1).function_name(), "middle_call");
                assert_eq!(call_stack.get(2).function_name(), "last_call");
                assert_eq!(call_stack.get(0).line_number(), 2);
                assert_eq!(call_stack.get(1).line_number(), 2);
                assert_eq!(call_stack.get(2).line_number(), 5);
            }
            Ok(_) => panic!("Expected an error!"),
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => {
                let call_stack = context.ensure_treewalk().state.call_stack();
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::FunctionNotFound(
                        "foo".to_string(),
                        call_stack.clone(),
                    ))
                );

                assert_eq!(call_stack.len(), 1);
                assert_eq!(call_stack.get(0).file_path_str(), "<stdin>");
                assert_eq!(call_stack.get(0).function_name(), "__main__");
                assert_eq!(call_stack.get(0).line_number(), 11);
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                        ExprResult::Integer(3),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::FloatingPoint(2.1)
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2)
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2)
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2)
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(0),
                        ExprResult::Integer(1)
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("g"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2)
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("h"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                        ExprResult::Integer(1),
                        ExprResult::Integer(2)
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("i"),
                    Some(ExprResult::List(Container::new(List::new(vec![]))))
                );
                assert!(matches!(
                    interpreter.state.read("j"),
                    Some(ExprResult::ListIterator(_))
                ));
                assert_eq!(
                    interpreter.state.read("k").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::ListIterator)
                );
                assert_eq!(interpreter.state.read("l"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("m"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("n"), Some(ExprResult::Integer(0)));
                assert!(matches!(
                    interpreter.state.read("o"),
                    Some(ExprResult::Method(_))
                ));
                assert_eq!(
                    interpreter.state.read("p").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Method)
                );
                assert_eq!(
                    interpreter.state.read("q"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                        ExprResult::Integer(3),
                    ]))))
                );
                assert!(matches!(
                    interpreter.state.read("s"),
                    Some(ExprResult::Method(_))
                ));
                assert_eq!(
                    interpreter.state.read("r"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(3),
                        ExprResult::Integer(4),
                        ExprResult::Integer(5),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("t"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                        ExprResult::Integer(3),
                        ExprResult::Integer(4)
                    ]))))
                );
            }
        }

        let input = "list([1,2,3], [1,2])";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::WrongNumberOfArguments(
                        1,
                        3,
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                        ExprResult::Integer(3),
                    ])))))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::FloatingPoint(2.1),
                        ExprResult::Integer(1),
                    ])))))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                    ])))))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                    ])))))
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                    ])))))
                );
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                    ])))))
                );
                assert_eq!(
                    interpreter.state.read("g"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::Integer(0),
                        ExprResult::Integer(1),
                    ])))))
                );
                assert_eq!(
                    interpreter.state.read("h"),
                    Some(ExprResult::Set(Container::new(Set::default())))
                );
                assert!(matches!(
                    interpreter.state.read("i"),
                    Some(ExprResult::SetIterator(_))
                ));
                assert_eq!(
                    interpreter.state.read("j").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::SetIterator)
                );
                assert_eq!(
                    interpreter.state.read("new_set"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::String(Str::new("five".into()))
                    ])))))
                );
                assert_eq!(interpreter.state.read("k"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("l"),
                    Some(ExprResult::Boolean(false))
                );
            }
        }

        let input = "set({1,2,3}, {1,2})";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::WrongNumberOfArguments(
                        1,
                        2,
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                        ExprResult::Integer(3),
                    ])))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::FloatingPoint(2.1)
                    ])))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2)
                    ])))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2)
                    ])))
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2)
                    ])))
                );
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(0),
                        ExprResult::Integer(1)
                    ])))
                );
                assert!(matches!(
                    interpreter.state.read("g"),
                    Some(ExprResult::TupleIterator(_))
                ));
                assert_eq!(
                    interpreter.state.read("h").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::TupleIterator)
                );
                assert_eq!(
                    interpreter.state.read("i"),
                    Some(ExprResult::Tuple(Tuple::new(vec![ExprResult::Integer(4),])))
                );
                assert_eq!(
                    interpreter.state.read("j"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(9),
                        ExprResult::Integer(10),
                    ])))
                );
            }
        }

        let input = "tuple([1,2,3], [1,2])";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    // TODO these numbers are wrong because it comes out of Dunder::new
                    MemphisError::Interpreter(InterpreterError::WrongNumberOfArguments(
                        2,
                        3,
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(10),
                        ExprResult::Integer(2),
                        ExprResult::Integer(3),
                    ]))))
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("f"), Some(ExprResult::Integer(2)));
            }
        }

        let input = r#"
d = (1,2,3)
d[0] = 10
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("'tuple' object does not support item assignment".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
d = (1,2,3)
del d[0]
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("'tuple' object does not support item deletion".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
4[1]
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("'int' object is not subscriptable".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(20)));
                assert_eq!(interpreter.state.read("i"), Some(ExprResult::Integer(8)));
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Boolean(false))
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(20)));
                assert_eq!(interpreter.state.read("i"), Some(ExprResult::Integer(8)));
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Boolean(false))
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(20)));
                assert_eq!(interpreter.state.read("i"), Some(ExprResult::Integer(8)));
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Boolean(false))
                );
            }
        }

        let input = r#"
b = 0
for i in range(5):
    b = b + i
print(b)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(10)));
            }
        }

        let input = r#"
a = {"a": 1,"b": 2}
b = 0
for k, v in a.items():
    b = b + v
print(b)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(3)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::RangeIterator(_))
                ));
                assert_eq!(
                    interpreter.state.read("b").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::RangeIterator)
                );
                assert_eq!(
                    interpreter.state.read("c").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Range)
                );
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(3)));
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Integer(6)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
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
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                        ExprResult::Integer(3),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("g"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(4),
                        ExprResult::Integer(5),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("j"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::Integer(6),
                        ExprResult::Integer(7),
                    ])))))
                );
                assert_eq!(
                    interpreter.state.read("m"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(8),
                        ExprResult::Integer(9),
                    ])))
                );
                assert_eq!(
                    interpreter.state.read("p"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::from([
                            (
                                ExprResult::String(Str::new("c".to_string())),
                                ExprResult::Integer(3)
                            ),
                            (
                                ExprResult::String(Str::new("d".to_string())),
                                ExprResult::Integer(4)
                            ),
                        ])
                    ))))
                );
                assert_eq!(
                    interpreter.state.read("q").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::None)
                );
                assert_eq!(
                    interpreter.state.read("r").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Ellipsis)
                );
                assert_eq!(
                    interpreter.state.read("s").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::NotImplemented)
                );
                assert_eq!(
                    interpreter.state.read("t").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Type)
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(2),
                        ExprResult::Integer(4),
                        ExprResult::Integer(6),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::List(Container::new(List::default())))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(6),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                        ExprResult::Integer(2),
                        ExprResult::Integer(4),
                    ]))))
                );
            }
        }
    }

    #[test]
    fn set_comprehension() {
        let input = r#"
a = [1,2,3]
b = { i * 2 for i in a }
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::Integer(2),
                        ExprResult::Integer(4),
                        ExprResult::Integer(6),
                    ])))))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Boolean(true)));
                assert_eq!(interpreter.state.read("f"), Some(ExprResult::Boolean(true)));
                assert_eq!(interpreter.state.read("g"), Some(ExprResult::Boolean(true)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(4)));
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(3)));
            }
        }

        let input = r#"
def countdown(n):
    yield n

a = countdown(5)
b = next(a)
c = next(a)
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::StopIteration(
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("z"), Some(ExprResult::Integer(12)));
            }
        }

        let input = r#"
def countdown(n):
    yield n
    yield n - 1
    yield n - 2

z = [ i for i in countdown(5) ]
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("z"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(5),
                        ExprResult::Integer(4),
                        ExprResult::Integer(3)
                    ]))))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("z"), Some(ExprResult::Integer(15)));
            }
        }

        let input = r#"
def countdown(n):
    for i in range(n):
        yield i

z = 0
for i in countdown(5):
    z = z + i
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("z"), Some(ExprResult::Integer(10)));
            }
        }

        let input = r#"
def countdown():
    for i in [1,2]:
        yield i * 2

a = list(countdown())
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(2),
                        ExprResult::Integer(4),
                    ]))))
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(4),
                        ExprResult::Integer(6),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(3),
                        ExprResult::Integer(2),
                        ExprResult::Integer(1)
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(7),
                        ExprResult::Integer(8),
                        ExprResult::Integer(9)
                    ]))))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(interpreter.state.is_class("Foo"));
                assert!(interpreter.state.is_class("Parent"));
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(4)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(12)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(4)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(11)));
            }
        }

        let input = r#"
class abstractclassmethod(classmethod):
    pass
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                // This used to throw an error based on classmethod not yet being a class. This is
                // found in abc.py in the Python standard lib.
                assert!(matches!(
                    interpreter.state.read("abstractclassmethod"),
                    Some(ExprResult::Class(_))
                ));
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
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::AttributeError(
                        "ChildTwo".into(),
                        "x".to_string(),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(3)));
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Integer(1)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(3)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Super(_))
                ));
                assert!(matches!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Super(_))
                ));
                assert!(matches!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Super(_))
                ));
                assert_eq!(
                    interpreter.state.read("d").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Super)
                );
                assert_eq!(
                    interpreter.state.read("e").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Super)
                );
                assert_eq!(
                    interpreter.state.read("f").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Super)
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(1)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(5)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(2)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(12)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                let mro = interpreter.state.read("a").map(|a| {
                    a.as_tuple()
                        .unwrap()
                        .into_iter()
                        .map(|i| i.as_class().unwrap().borrow().name().to_string())
                        .collect::<Vec<String>>()
                });
                assert_eq!(
                    mro,
                    Some(vec![
                        "Foo".into(),
                        "Bar".into(),
                        "Baz".into(),
                        "object".into()
                    ])
                );
            }
        }
    }

    #[test]
    fn dictionaries() {
        let input = r#"
a = { "b": 4, 'c': 5 }
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::from([
                            (
                                ExprResult::String(Str::new("b".to_string())),
                                ExprResult::Integer(4)
                            ),
                            (
                                ExprResult::String(Str::new("c".to_string())),
                                ExprResult::Integer(5)
                            ),
                        ])
                    ))))
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::from([
                            (
                                ExprResult::String(Str::new("b".to_string())),
                                ExprResult::Integer(4)
                            ),
                            (
                                ExprResult::String(Str::new("c".to_string())),
                                ExprResult::Integer(5)
                            ),
                        ])
                    ))))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::DictItems(DictItems::new(
                        interpreter.clone(),
                        vec![
                            (
                                ExprResult::String(Str::new("b".to_string())),
                                ExprResult::Integer(4)
                            ),
                            (
                                ExprResult::String(Str::new("c".to_string())),
                                ExprResult::Integer(5)
                            ),
                        ]
                    )))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::from([
                            (
                                ExprResult::String(Str::new("b".to_string())),
                                ExprResult::Integer(8)
                            ),
                            (
                                ExprResult::String(Str::new("c".to_string())),
                                ExprResult::Integer(10)
                            ),
                        ])
                    ))))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::from([
                            (
                                ExprResult::String(Str::new("b".to_string())),
                                ExprResult::Integer(4)
                            ),
                            (
                                ExprResult::String(Str::new("c".to_string())),
                                ExprResult::Integer(5)
                            ),
                        ])
                    ))))
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::from([
                            (
                                ExprResult::String(Str::new("b".to_string())),
                                ExprResult::Integer(4)
                            ),
                            (
                                ExprResult::String(Str::new("c".to_string())),
                                ExprResult::Integer(5)
                            ),
                        ])
                    ))))
                );
                assert_eq!(interpreter.state.read("f"), Some(ExprResult::Integer(4)));
                assert_eq!(
                    interpreter.state.read("g"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::new()
                    ))))
                );
                assert_eq!(
                    interpreter.state.read("h"),
                    Some(ExprResult::DictItems(DictItems::default()))
                );
                assert!(matches!(
                    interpreter.state.read("q"),
                    Some(ExprResult::DictItemsIterator(_))
                ));
                assert_eq!(
                    interpreter.state.read("r").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::DictItemIterator)
                );
                assert_eq!(
                    interpreter.state.read("i"),
                    Some(ExprResult::DictKeys(DictKeys::new(vec![])))
                );
                assert_eq!(
                    interpreter.state.read("j"),
                    Some(ExprResult::DictKeys(DictKeys::new(vec![
                        ExprResult::String(Str::new("b".into())),
                        ExprResult::String(Str::new("c".into())),
                    ])))
                );
                assert!(matches!(
                    interpreter.state.read("k"),
                    Some(ExprResult::DictKeysIterator(_))
                ));
                assert_eq!(
                    interpreter.state.read("l").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::DictKeyIterator)
                );
                assert_eq!(
                    interpreter.state.read("m"),
                    Some(ExprResult::DictValues(DictValues::new(vec![])))
                );
                assert_eq!(
                    interpreter.state.read("n"),
                    Some(ExprResult::DictValues(DictValues::new(vec![
                        ExprResult::Integer(4),
                        ExprResult::Integer(5),
                    ])))
                );
                assert!(matches!(
                    interpreter.state.read("o"),
                    Some(ExprResult::DictValuesIterator(_))
                ));
                assert_eq!(
                    interpreter.state.read("p").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::DictValueIterator)
                );
                assert_eq!(
                    interpreter.state.read("s").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::DictKeys)
                );
                assert_eq!(
                    interpreter.state.read("t").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::DictValues)
                );
                assert_eq!(
                    interpreter.state.read("u").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::DictItems)
                );
                assert_eq!(
                    interpreter.state.read("v"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::String(Str::new("b".into())),
                        ExprResult::String(Str::new("c".into())),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("w"),
                    Some(ExprResult::Set(Container::new(Set::new(HashSet::from([
                        ExprResult::String(Str::new("b".into())),
                        ExprResult::String(Str::new("c".into())),
                    ])))))
                );
            }
        }

        let input = r#"
a = { "b": 4, 'c': 5 }
b = a.get("b")
c = a.get("d")
d = a.get("d", 99)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(4)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::None));
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(99)));
            }
        }

        let input = r#"
a = { "b": 4, 'c': 5 }
b = { **a }
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::from([
                            (
                                ExprResult::String(Str::new("b".to_string())),
                                ExprResult::Integer(4)
                            ),
                            (
                                ExprResult::String(Str::new("c".to_string())),
                                ExprResult::Integer(5)
                            ),
                        ])
                    ))))
                );
            }
        }

        let input = r#"
inner = { 'key': 'inner' }
b = { 'key': 'outer', **inner }
c = { **inner, 'key': 'outer' }
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::from([(
                            ExprResult::String(Str::new("key".to_string())),
                            ExprResult::String(Str::new("inner".to_string()))
                        ),])
                    ))))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Dict(Container::new(Dict::new(
                        &interpreter,
                        HashMap::from([(
                            ExprResult::String(Str::new("key".to_string())),
                            ExprResult::String(Str::new("outer".to_string()))
                        ),])
                    ))))
                );
            }
        }
    }

    #[test]
    fn assert() {
        let input = r#"
assert True
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {}
        }

        let input = r#"
assert False
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::AssertionError(
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(3)));
            }
        }

        let input = r#"
try:
    a = 4 / 0
except:
    a = 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(2)));
            }
        }

        let input = r#"
try:
    a = 4 / 1
except:
    a = 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(4)));
            }
        }

        let input = r#"
try:
    a = 4 / 1
except:
    a = 2
finally:
    a = 3
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(3)));
            }
        }

        let input = r#"
try:
    a = 4 / 1
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => assert!(matches!(e, MemphisError::Parser(ParserError::SyntaxError))),
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
try:
    a = 4 / 0
except ZeroDivisionError:
    a = 2
except Exception:
    a = 3
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(2)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(3)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(3)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(3)));
                match interpreter.state.read("e") {
                    Some(ExprResult::Exception(e)) => assert_eq!(
                        e,
                        Box::new(InterpreterError::NameError(
                            "b".into(),
                            interpreter.state.call_stack()
                        ))
                    ),
                    _ => panic!(),
                }
            }
        }

        let input = r#"
try:
    b = b + 1
    a = 4 / 0
except (ZeroDivisionError, Exception):
    a = 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(2)));
            }
        }

        let input = r#"
try:
    b = b + 1
    a = 4 / 0
except (Exception, ZeroDivisionError):
    a = 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(2)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(4)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
            }
        }

        // Uncaught exception
        let input = r#"
try:
    a = 8 / 0
except ValueError:
    a = 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::DivisionByZero(
                        "division by zero".into(),
                        context.ensure_treewalk().state.call_stack(),
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
try:
    a = 8 / 0
except ZeroDivisionError:
    raise
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::DivisionByZero(
                        "division by zero".into(),
                        context.ensure_treewalk().state.call_stack(),
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }
    }

    #[test]
    fn args_and_kwargs() {
        let input = r#"
def test_kwargs(**kwargs):
    print(kwargs['a'])
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                let expected_name = "test_kwargs".to_string();
                let expected_args = ParsedArgDefinitions {
                    args: vec![],
                    args_var: None,
                    kwargs_var: Some("kwargs".into()),
                };
                assert!(matches!(
                    interpreter.state.read("test_kwargs").unwrap().as_function().unwrap().borrow().clone(),
                    Function {
                        name,
                        args,
                        ..
                    } if name == expected_name && args == expected_args
                ));
            }
        }

        let input = r#"
def test_kwargs(**kwargs):
    return kwargs['a']

a = test_kwargs(a=5, b=2)
# A second test to ensure the value is not being set using b=2
b = test_kwargs(a=5, b=2)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(5)));
            }
        }

        let input = r#"
def test_kwargs(**kwargs):
    return kwargs['a']

a = test_kwargs(**{'a': 5, 'b': 2})
c = {'a': 4, 'b': 3}
b = test_kwargs(**c)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(4)));
            }
        }

        let input = r#"
def test_kwargs(**kwargs):
    return kwargs['a'], kwargs['b']

first = {'a': 44 }
second = {'b': 55 }
b = test_kwargs(**first, **second)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(44),
                        ExprResult::Integer(55),
                    ])))
                );
            }
        }

        let input = r#"
def test_args(*args):
    return args[1]

c = [0, 1]
b = test_args(*c)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(1)));
            }
        }

        let input = r#"
def test_args(*args):
    return args[1]

c = [2, 3]
b = test_args(0, 1, *c)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(1)));
            }
        }

        let input = r#"
def test_args(one, two, *args):
    return args[1]

c = [2, 3]
b = test_args(0, 1, *c)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(3)));
            }
        }

        let input = r#"
def test_args(one, two):
    return one

b = test_args(0)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("test_args() missing 1 required positional argument: 'two'".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
def test_args(one, two, three):
    return one

b = test_args(0)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("test_args() missing 2 required positional arguments: 'two' and 'three'".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
def test_args(one, two):
    return one

b = test_args(1, 2, 3)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::WrongNumberOfArguments(
                        2,
                        3,
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
def test_args(one, two, *args):
    return args

b = test_args(1, 2)
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Tuple(Tuple::default()))
                );
            }
        }

        let input = r#"
class Foo:
    def __init__(self, **kwargs):
        self.a = kwargs['a']

foo = Foo(a=5)
a = foo.a
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Cell)
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(1)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Cell)
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(1)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(2)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(1)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::None));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(4)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(4)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(8)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(6)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(8)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(14)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(42)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(18)));
            }
        }
    }

    #[test]
    fn raise() {
        let input = r#"
raise TypeError
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        None,
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
raise TypeError('type is no good')
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("type is no good".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Integer(_))
                ));
                assert!(matches!(
                    interpreter.state.read("b"),
                    Some(ExprResult::FloatingPoint(_))
                ));
                assert!(matches!(
                    interpreter.state.read("c"),
                    Some(ExprResult::String(_))
                ));
                assert!(matches!(
                    interpreter.state.read("d"),
                    Some(ExprResult::String(_))
                ));
                match interpreter.state.read("a") {
                    Some(ExprResult::Integer(a)) => {
                        assert!(a > 0);
                    }
                    _ => panic!("Unexpected type!"),
                }
                match interpreter.state.read("b") {
                    Some(ExprResult::FloatingPoint(b)) => {
                        assert!(b > 1701281981.0);
                    }
                    _ => panic!("Unexpected type!"),
                }
                match interpreter.state.read("c") {
                    Some(ExprResult::String(c)) => {
                        assert!(c.len() > 10);
                    }
                    _ => panic!("Unexpected type!"),
                }
                match interpreter.state.read("d") {
                    Some(ExprResult::String(d)) => {
                        assert!(d.len() > 10);
                    }
                    _ => panic!("Unexpected type!"),
                }
                assert!(matches!(
                    interpreter.state.read("ref"),
                    Some(ExprResult::CPythonObject(_))
                ));
                assert!(matches!(
                    interpreter.state.read("e"),
                    Some(ExprResult::CPythonClass(_))
                ));
                assert_eq!(
                    interpreter.state.read("f").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Module)
                );
                assert_eq!(
                    interpreter.state.read("g").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Module)
                );
                assert_eq!(
                    interpreter.state.read("h").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Module)
                );
                assert_eq!(
                    interpreter.state.read("i").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Tuple)
                );
                assert_eq!(
                    interpreter.state.read("j").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Module)
                );
                assert_eq!(
                    interpreter.state.read("k").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Module)
                );
                assert_eq!(
                    interpreter.state.read("l").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Module)
                );
                assert_eq!(
                    interpreter.state.read("m"),
                    Some(ExprResult::String(Str::new("os_path".into())))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(3)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => assert_eq!(
                e,
                MemphisError::Interpreter(InterpreterError::MissingContextManagerProtocol(
                    context.ensure_treewalk().state.call_stack()
                ))
            ),
            Ok(_) => panic!("Expected an exception!"),
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => assert_eq!(
                e,
                MemphisError::Interpreter(InterpreterError::MissingContextManagerProtocol(
                    context.ensure_treewalk().state.call_stack()
                ))
            ),
            Ok(_) => panic!("Expected an exception!"),
        }
    }

    #[test]
    fn delete() {
        let input = r#"
a = 4
del a
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), None);
            }
        }

        let input = r#"
a = {'b': 1, 'c': 2}
del a['b']
c = a['b']
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::KeyError(
                        "b".into(),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error"),
        }

        let input = r#"
a = [0,1,2]
del a[1]
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(0),
                        ExprResult::Integer(2)
                    ]))))
                );
            }
        }

        let input = r#"
class Foo:
    def __init__(self):
        self.x = 1

f = Foo()
del f.x
a = f.x
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::AttributeError(
                        "Foo".into(),
                        "x".into(),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error"),
        }

        let input = r#"
class Foo:
    def bar(self):
        return 1

f = Foo()
del f.bar
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::AttributeError(
                        "Foo".into(),
                        "bar".into(),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error"),
        }

        let input = r#"
a = 4
b = 5
c = 6
del a, c
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), None);
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("c"), None);
            }
        }
    }

    #[test]
    fn byte_string() {
        let input = r#"
a = b'hello'
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Bytes("hello".into()))
                );
            }
        }

        let input = r#"
a = iter(b'hello')
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::BytesIterator(_))
                ))
            }
        }

        let input = r#"
a = type(iter(b'hello'))
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::BytesIterator)
                )
            }
        }
    }

    #[test]
    fn byte_array() {
        let input = r#"
a = bytearray()
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::ByteArray(Container::new(ByteArray::new(
                        "".into()
                    ))))
                );
            }
        }

        let input = r#"
a = bytearray(b'hello')
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::ByteArray(Container::new(ByteArray::new(
                        "hello".into()
                    ))))
                );
            }
        }

        let input = r#"
a = bytearray('hello')
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("string argument without an encoding".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
a = iter(bytearray())
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::ByteArrayIterator(_))
                ))
            }
        }

        let input = r#"
a = type(iter(bytearray()))
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::ByteArrayIterator)
                )
            }
        }
    }

    #[test]
    fn bytes_builtin() {
        let input = r#"
a = bytes()
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Bytes("".into()))
                );
            }
        }

        let input = r#"
a = bytes(b'hello')
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Bytes("hello".into()))
                );
            }
        }

        let input = r#"
a = bytes('hello')
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("string argument without an encoding".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
a = iter(bytes())
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::BytesIterator(_))
                ))
            }
        }

        let input = r#"
a = type(iter(bytes()))
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::BytesIterator)
                )
            }
        }
    }

    #[test]
    fn compound_operator() {
        let input = r#"
a = 5
a += 1
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(6)));
            }
        }

        let input = r#"
a = 5
a -= 1
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(4)));
            }
        }

        let input = r#"
a = 5
a *= 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(10)));
            }
        }

        let input = r#"
a = 5
a /= 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(2)));
            }
        }

        let input = r#"
a = 0b0101
a &= 0b0100
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(4)));
            }
        }

        let input = r#"
a = 0b0101
a |= 0b1000
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(13)));
            }
        }

        let input = r#"
a = 0b0101
a ^= 0b0100
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(1)));
            }
        }

        let input = r#"
a = 5
a //= 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(2)));
            }
        }

        let input = r#"
a = 0b0101
a <<= 1
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(10)));
            }
        }

        let input = r#"
a = 0b0101
a >>= 1
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(2)));
            }
        }

        let input = r#"
a = 11
a %= 2
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(1)));
            }
        }

        let input = r#"
a = 2
a **= 3
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(8)));
            }
        }
    }

    #[test]
    fn iter_builtin() {
        let input = r#"
a = iter([1,2,3])
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::ListIterator(_))
                ));
            }
        }

        let input = r#"
b = 0
for i in iter([1,2,3]):
    b += i
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(6)));
            }
        }
    }

    #[test]
    fn f_strings() {
        let input = r#"
name = "John"
a = f"Hello {name}"
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::String(Str::new("Hello John".into())))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::ReversedIterator(_))
                ));
                assert!(matches!(
                    interpreter.state.read("b"),
                    Some(ExprResult::ReversedIterator(_))
                ));
                assert_eq!(
                    interpreter.state.read("c").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::ReversedIterator)
                );
                assert_eq!(
                    interpreter.state.read("d").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::ReversedIterator)
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(3),
                        ExprResult::Integer(2),
                        ExprResult::Integer(1),
                    ]))))
                );
            }
        }
    }

    #[test]
    fn binary_operators() {
        let input = "a = 0x1010 & 0x0011";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Integer(0x0010))
                );
            }
        }

        let input = "a = 0o1010 | 0o0011";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Integer(0o1011))
                );
            }
        }

        let input = "a = 0b1010 ^ 0b0011";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Integer(0b1001))
                );
            }
        }

        let input = "a = 23 % 5";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(3)));
            }
        }

        let input = "a = 0b0010 << 1";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Integer(0b0100))
                );
            }
        }

        let input = "a = 2 * 3 << 2 + 4 & 205";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(128)));
            }
        }

        let input = "a = ~0b1010";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(-11)));
            }
        }

        let input = "a = ~5.5";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => assert_eq!(
                e,
                MemphisError::Interpreter(InterpreterError::TypeError(
                    Some("bad operand type for unary ~: 'float'".to_string()),
                    context.ensure_treewalk().state.call_stack()
                ))
            ),
            Ok(_) => panic!("Expected an error!"),
        }

        // This tests the right-associativity of exponentiation.
        // right-associativity gives 2 ** (3 ** 2) == 512
        // NOT
        // left-associativity which gives (2 ** 3) ** 2 == 64
        let input = "a = 2 ** 3 ** 2";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(512)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(6)));
            }
        }

        let input = r#"
a = 0
for i in [1,2,3,4,5,6]:
    if i == 4:
        continue
    a += i
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(17)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(6)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(17)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(6)));
            }
        }

        let input = r#"
a = 0
for i in [1,2,3,4,5,6]:
    a += i
else:
    a = 1024
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(1024)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Zip(_))
                ));
                assert!(matches!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Zip(_))
                ));
                assert_eq!(
                    interpreter.state.read("c").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Zip)
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(1),
                            ExprResult::Integer(4),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(2),
                            ExprResult::Integer(5),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(3),
                            ExprResult::Integer(6),
                        ])),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(1),
                            ExprResult::Integer(4),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(2),
                            ExprResult::Integer(5),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(3),
                            ExprResult::Integer(6),
                        ])),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(0),
                            ExprResult::Integer(0),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(1),
                            ExprResult::Integer(1),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(2),
                            ExprResult::Integer(2),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(3),
                            ExprResult::Integer(3),
                        ])),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("g"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(0),
                            ExprResult::Integer(0),
                            ExprResult::Integer(0),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(1),
                            ExprResult::Integer(1),
                            ExprResult::Integer(1),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(2),
                            ExprResult::Integer(2),
                            ExprResult::Integer(2),
                        ])),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("h"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(1),
                            ExprResult::Integer(4),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(2),
                            ExprResult::Integer(5),
                        ])),
                        ExprResult::Tuple(Tuple::new(vec![
                            ExprResult::Integer(3),
                            ExprResult::Integer(6),
                        ])),
                    ]))))
                );
            }
        }

        let input = r#"
f = [ i for i in zip(range(5), range(4), strict=True) ]
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(e, MemphisError::Interpreter(InterpreterError::RuntimeError));
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Type)
                );
                assert!(matches!(
                    interpreter.state.read("b"),
                    Some(ExprResult::MappingProxy(_))
                ));
                assert_eq!(
                    interpreter.state.read("c").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::MappingProxy)
                );
                assert_eq!(
                    interpreter.state.read("d").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::BuiltinMethod)
                );
                assert_eq!(
                    interpreter.state.read("e").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Dict)
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a").unwrap(),
                    ExprResult::Class(_)
                ));
                assert_eq!(
                    interpreter.state.read("b").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Type)
                );
                assert!(matches!(
                    interpreter.state.read("c").unwrap(),
                    ExprResult::Class(_)
                ));
                assert!(matches!(
                    interpreter.state.read("d").unwrap(),
                    ExprResult::TypeNode(_)
                ));
                assert!(matches!(
                    interpreter.state.read("e").unwrap(),
                    ExprResult::TypeNode(_)
                ));
                assert!(matches!(
                    interpreter.state.read("f").unwrap(),
                    ExprResult::Class(_)
                ));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(6)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(5)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(6)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(6)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                let foo = match interpreter.state.read("Foo") {
                    Some(ExprResult::Class(o)) => o,
                    _ => panic!("Expected an object."),
                };
                assert!(matches!(
                    foo.get_member(&interpreter, "make").unwrap().unwrap(),
                    ExprResult::Method(_)
                ));

                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(5)));
            }
        }

        let input = r#"
class Foo:
    @classmethod
    def make(cls):
        return 5

b = Foo.make()
c = Foo().make()
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(5)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(10)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(10)));
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(9)));
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Integer(10)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::AttributeError(
                        "Foo".into(),
                        "val".into(),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::AttributeError(
                        "Foo".into(),
                        "val".into(),
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(5)));
            }
        }

        // Before we explicitly supported static methods, this case used to work. Let's test it to
        // ensure we keep getting an error now.
        let input = r#"
class Foo:
    def make():
        return 5

c = Foo().make()
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::WrongNumberOfArguments(
                        0,
                        1,
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::String(Str::new("First".into())))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::String(Str::new("First".into())))
                );
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Boolean(true)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::String(Str::new("Second".into())))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::String(Str::new("Second".into())))
                );
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Boolean(true)));
            }
        }

        let input = r#"
class Foo:
    def __new__(cls):
        pass

a = Foo()
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::None));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(5)));
                match interpreter.state.read("d") {
                    Some(ExprResult::Exception(e)) => assert_eq!(
                        e,
                        Box::new(InterpreterError::AttributeError(
                            "ConcreteImplementation".into(),
                            "run".into(),
                            interpreter.state.call_stack()
                        ))
                    ),
                    _ => panic!("Expected error!"),
                }
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(33)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(33)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(10)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(9)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(4)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(20)));
            }
        }

        let input = r#"
def foo():
    def inner():
        nonlocal a
    inner()
foo()
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    // this should become a parser error but it will require adding scope
                    // context to the parser
                    MemphisError::Interpreter(InterpreterError::SyntaxError(
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
def foo():
    nonlocal a
foo()
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    // this should become a parser error but it will require adding scope
                    // context to the parser
                    MemphisError::Interpreter(InterpreterError::SyntaxError(
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
nonlocal a
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    // this should become a parser error but it will require adding scope
                    // context to the parser
                    MemphisError::Interpreter(InterpreterError::SyntaxError(
                        context.ensure_treewalk().state.call_stack()
                    ))
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }
    }

    #[test]
    fn object_builtin() {
        let input = r#"
a = object
b = object()
c = object().__str__
"#;
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Object)
                );
                assert!(matches!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Object(_))
                ));
                assert!(matches!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Method(_))
                ));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Int)
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(0)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(6)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Method(_))
                ));
                //assert!(matches!(
                //    interpreter.state.read("b"),
                //    Some(ExprResult::Function(_))
                //));
                assert!(matches!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Method(_))
                ));
                assert!(matches!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Method(_))
                ));
                assert!(matches!(
                    interpreter.state.read("e"),
                    Some(ExprResult::Function(_))
                ));
                assert!(matches!(
                    interpreter.state.read("f"),
                    Some(ExprResult::Function(_))
                ));
                assert_eq!(
                    interpreter.state.read("g").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Method)
                );
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Integer(3)));
                assert_eq!(interpreter.state.read("f"), Some(ExprResult::Integer(3)));
            }
        }

        let input = r#"
class Child:
    def one(self):
        return 1

b = Child.one()
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => assert_eq!(
                e,
                MemphisError::Interpreter(InterpreterError::TypeError(
                    Some("one() missing 1 required positional argument: 'self'".to_string()),
                    context.ensure_treewalk().state.call_stack()
                ))
            ),
            Ok(_) => panic!("Expected an error!"),
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(11)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(22)));
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(22)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(2),
                        ExprResult::Integer(3)
                    ])))
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(3)));
            }
        }

        let input = r#"
b, c = [1, 2, 3]
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => assert_eq!(
                e,
                MemphisError::Interpreter(InterpreterError::ValueError(
                    "too many values to unpack (expected 2)".into(),
                    context.ensure_treewalk().state.call_stack()
                ))
            ),
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
a, b, c = [2, 3]
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => assert_eq!(
                e,
                MemphisError::Interpreter(InterpreterError::ValueError(
                    "not enough values to unpack (expected 3, got 2)".into(),
                    context.ensure_treewalk().state.call_stack()
                ))
            ),
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
b, c = (1, 2)
d, e = [1, 2]
f, g = {1, 2}
h, i, j = range(1, 4)
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("f"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("g"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("h"), Some(ExprResult::Integer(1)));
                assert_eq!(interpreter.state.read("i"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("j"), Some(ExprResult::Integer(3)));
            }
        }

        let input = r#"
l = [1,2]
a = (*l,)
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Tuple(Tuple::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2)
                    ])))
                );
            }
        }

        let input = r#"
a = (*5)
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => assert_eq!(
                e,
                MemphisError::Interpreter(InterpreterError::TypeError(
                    Some("Value after * must be an iterable, not int".into()),
                    context.ensure_treewalk().state.call_stack()
                ))
            ),
            Ok(_) => panic!("Expected an error!"),
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(8)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(8),
                        ExprResult::Integer(9),
                        ExprResult::Integer(10),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(3),
                        ExprResult::Integer(5),
                        ExprResult::Integer(7),
                        ExprResult::Integer(9),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(10),
                        ExprResult::Integer(8),
                        ExprResult::Integer(6),
                        ExprResult::Integer(4),
                        ExprResult::Integer(2),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(3),
                        ExprResult::Integer(4),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("g"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(10),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("h"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("i"),
                    Some(ExprResult::List(Container::new(List::default())))
                );
                assert!(matches!(
                    interpreter.state.read("j"),
                    Some(ExprResult::Slice(_))
                ));
                assert!(matches!(
                    interpreter.state.read("k"),
                    Some(ExprResult::Slice(_))
                ));
                assert!(matches!(
                    interpreter.state.read("l"),
                    Some(ExprResult::Slice(_))
                ));
                assert_eq!(
                    interpreter.state.read("m").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Slice)
                );
                assert_eq!(
                    interpreter.state.read("n"),
                    Some(ExprResult::String(Str::new("h".into())))
                );
                assert_eq!(
                    interpreter.state.read("o"),
                    Some(ExprResult::String(Str::new("h".into())))
                );
                assert_eq!(
                    interpreter.state.read("p"),
                    Some(ExprResult::String(Str::new("he".into())))
                );
                //assert_eq!(
                //    interpreter.state.read("q"),
                //    Some(ExprResult::String(Str::new("he".into())))
                //);
                assert_eq!(
                    interpreter.state.read("r"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(2),
                        ExprResult::Integer(4),
                        ExprResult::Integer(6),
                    ]))))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(6)));
            }
        }
    }

    #[test]
    fn slash_args() {
        let input = r#"
def foo(cls, /):
    pass
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("foo"),
                    Some(ExprResult::Function(_))
                ));
            }
        }
    }

    #[test]
    fn globals_builtin() {
        let input = r#"
a = 4
b = globals()
c = b['a']
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(4)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(2)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Integer(4)));
                assert!(matches!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Generator(_))
                ));
                assert_eq!(
                    interpreter.state.read("e").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Generator)
                );
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(6),
                        ExprResult::Integer(10),
                        ExprResult::Integer(12),
                        ExprResult::Integer(20),
                    ]))))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::FrozenSet(FrozenSet::new(HashSet::from([
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::FrozenSet(FrozenSet::default()))
                );
                assert_eq!(
                    interpreter.state.read("c").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::FrozenSet)
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::Integer(1),
                        ExprResult::Integer(2),
                    ]))))
                );
                assert_eq!(
                    interpreter.state.read("e").unwrap().get_type(),
                    Type::Method
                );
            }
        }

        let input = "frozenset([1,2,3], [1,2])";
        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::WrongNumberOfArguments(
                        1,
                        3,
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(88)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(99)));
            }
        }

        let input = r#"
def foo(data_one, data_two=None):
    pass

b = foo()
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("foo() missing 1 required positional argument: 'data_one'".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(44)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Integer(33)));
            }
        }

        let input = r#"
class Foo:
    pass

f = Foo()
b = getattr(f, 'val_two')
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::AttributeError(
                        "Foo".into(),
                        "val_two".into(),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("g"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("h"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("i"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("j"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("k"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("l"),
                    Some(ExprResult::Boolean(false))
                );
            }
        }

        let input = r#"
isinstance([], (int, 5))
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some(
                            "isinstance() arg 2 must be a type, a tuple of types, or a union"
                                .into()
                        ),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Boolean(true)));
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("g"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("h"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("i"), Some(ExprResult::Boolean(true)));
                assert_eq!(interpreter.state.read("j"), Some(ExprResult::Boolean(true)));
            }
        }

        let input = r#"
issubclass([], type)
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some("issubclass() arg 1 must be a class".into()),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"
issubclass(object, [])
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => {
                assert_eq!(
                    e,
                    MemphisError::Interpreter(InterpreterError::TypeError(
                        Some(
                            "issubclass() arg 2 must be a type, a tuple of types, or a union"
                                .into()
                        ),
                        context.ensure_treewalk().state.call_stack()
                    ))
                )
            }
            Ok(_) => panic!("Expected an error!"),
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("e"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("g"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("h"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("i"), Some(ExprResult::Boolean(true)));
            }
        }
    }

    #[test]
    fn memoryview_builtin() {
        let input = r#"
a = memoryview
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert!(matches!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Class(_))
                ));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("sum"), Some(ExprResult::Integer(9)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Traceback)
                );
                assert_eq!(
                    interpreter.state.read("b").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Frame)
                );
            }
        }
    }

    #[test]
    fn asyncio() {
        let input = r#"
a = asyncio.run
b = asyncio.sleep
c = asyncio.create_task
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                // these should probably just return Function, not BuiltinFunction
                // testing here to confirm they do not get bound to their module
                assert_eq!(
                    interpreter.state.read("a").unwrap().get_type(),
                    Type::BuiltinFunction
                );
                assert_eq!(
                    interpreter.state.read("b").unwrap().get_type(),
                    Type::BuiltinFunction
                );
                assert_eq!(
                    interpreter.state.read("c").unwrap().get_type(),
                    Type::BuiltinFunction
                );
            }
        }
    }

    #[test]
    fn multiple_assignment() {
        let input = r#"
a = b = True
"#;

        let mut context = init(input);

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Boolean(true)));
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Boolean(true)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Boolean(true)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Boolean(true)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Boolean(true)));
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::Boolean(false))
                );
                let Some(ExprResult::Method(method)) = interpreter.state.read("c") else {
                    panic!("Expected a method!");
                };
                assert!(matches!(method.receiver(), Some(ExprResult::Object(_))));
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Boolean(false))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(44)));
            }
        }

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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::String(Str::new("default value".into())))
                );
                assert_eq!(
                    interpreter.state.read("b"),
                    Some(ExprResult::String(Str::new("new value".into())))
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::String(Str::new("default value".into())))
                );
                assert_eq!(
                    interpreter
                        .state
                        .read("d")
                        .unwrap()
                        .get_class(&interpreter)
                        .borrow()
                        .name(),
                    "Descriptor"
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::String(Str::new("custom value".into())))
                );
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::String(Str::new("default value".into())))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Complex(Complex::new(4.0, 5.0)))
                );
                assert_eq!(
                    interpreter.state.read("b").unwrap().as_class().unwrap(),
                    interpreter.state.get_type_class(Type::Complex)
                );
                assert_eq!(
                    interpreter.state.read("c"),
                    Some(ExprResult::Complex(Complex::new(0.0, 0.0)))
                );
                assert_eq!(
                    interpreter.state.read("d"),
                    Some(ExprResult::Complex(Complex::new(1.0, 0.0)))
                );
                assert_eq!(
                    interpreter.state.read("e"),
                    Some(ExprResult::Complex(Complex::new(2.0, 3.0)))
                );
                assert_eq!(
                    interpreter.state.read("f"),
                    Some(ExprResult::Complex(Complex::new(2.1, 3.1)))
                );
                assert_eq!(
                    interpreter.state.read("g"),
                    Some(ExprResult::Complex(Complex::new(4.1, 5.1)))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::Boolean(false))
                );
                assert_eq!(interpreter.state.read("b"), Some(ExprResult::Boolean(true)));
                assert_eq!(interpreter.state.read("c"), Some(ExprResult::Boolean(true)));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(
                    interpreter.state.read("a"),
                    Some(ExprResult::List(Container::new(List::new(vec![
                        ExprResult::String(Str::new("a".to_string())),
                        ExprResult::String(Str::new("b".to_string())),
                    ]))))
                );
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(1)));
                let my = interpreter.state.read("my").unwrap().as_object().unwrap();
                let inner = my.get_member(&interpreter, "inner").unwrap().unwrap();
                assert_eq!(inner, ExprResult::Dict(Container::new(Dict::default())));
            }
        }
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

        match context.run_and_return_interpreter() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(interpreter) => {
                assert_eq!(interpreter.state.read("a"), Some(ExprResult::Integer(5)));
                match interpreter.state.read("b") {
                    Some(ExprResult::Integer(a)) => {
                        assert!(a != 0);
                    }
                    _ => panic!("Unexpected type!"),
                }
                match interpreter.state.read("c") {
                    Some(ExprResult::Integer(a)) => {
                        assert!(a != 0);
                    }
                    _ => panic!("Unexpected type!"),
                }
                assert_eq!(interpreter.state.read("d"), Some(ExprResult::Boolean(true)));
                match interpreter.state.read("the_exp") {
                    Some(ExprResult::Exception(e)) => assert_eq!(
                        e,
                        Box::new(InterpreterError::TypeError(
                            Some("__hash__ method should return an integer".into()),
                            interpreter.state.call_stack()
                        ))
                    ),
                    _ => panic!("Expected error!"),
                }
            }
        }
    }
}
