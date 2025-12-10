use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

use crate::{
    core::{log, Container, LogLevel},
    domain::ExecutionError,
    parser::types::{
        Ast, BinOp, CallArgs, Callee, CompareOp, DictOperation, Expr, FStringPart, ForClause,
        LogicalOp, Params, SliceParams, TypeNode, UnaryOp,
    },
    treewalk::{
        protocols::TryEvalFrom,
        result::Raise,
        types::{
            iterators::GeneratorIter, Dict, Function, Generator, List, Set, Slice, Str, Tuple,
        },
        TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn evaluate_expr(&self, expr: &Expr) -> TreewalkResult<TreewalkValue> {
        match expr {
            Expr::None => Ok(TreewalkValue::None),
            Expr::Ellipsis => Ok(TreewalkValue::Ellipsis),
            Expr::NotImplemented => Ok(TreewalkValue::NotImplemented),
            Expr::Integer(value) => Ok(TreewalkValue::Int(*value)),
            Expr::Float(value) => Ok(TreewalkValue::Float(*value)),
            Expr::Boolean(value) => Ok(TreewalkValue::Bool(*value)),
            Expr::StringLiteral(value) => Ok(TreewalkValue::Str(Str::new(value))),
            Expr::BytesLiteral(value) => Ok(TreewalkValue::Bytes(value.clone())),
            Expr::Variable(name) => self.load_var(name),
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
            Expr::ComparisonChain { left, ops } => self.evaluate_comparison_chain(left, ops),
            Expr::Await(right) => self.evaluate_await(right),
            Expr::FunctionCall { callee, args } => self.evaluate_function_call(callee, args),
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
            Expr::MemberAccess { object, field } => self.evaluate_member_access(object, field),
            Expr::IndexAccess { object, index } => self.evaluate_index_access(object, index),
            Expr::SliceOperation { object, params } => {
                self.evaluate_slice_operation(object, params)
            }
            Expr::FString(parts) => self.evaluate_f_string(parts),
            Expr::Lambda { args, expr } => self.evaluate_lambda(args, expr),
            Expr::TypeNode(type_node) => self.evaluate_type_node(type_node),
            Expr::Yield(expr) => self.evaluate_yield(expr),
            Expr::YieldFrom(expr) => self.evaluate_yield_from(expr),
        }
    }

    fn evaluate_list(&self, items: &[Expr]) -> TreewalkResult<TreewalkValue> {
        items
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<Vec<_>, _>>()
            .map(|l| TreewalkValue::List(Container::new(List::new(l))))
    }

    fn evaluate_set(&self, items: &HashSet<Expr>) -> TreewalkResult<TreewalkValue> {
        items
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<HashSet<_>, _>>()
            .map(Set::new)
            .map(Container::new)
            .map(TreewalkValue::Set)
    }

    fn evaluate_dict(&self, dict_ops: &[DictOperation]) -> TreewalkResult<TreewalkValue> {
        // TODO we are suppressing this clippy error for now because `TreewalkValue` allows interior
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
                    for key in unpacked.clone().as_iterable().raise(self)? {
                        let value = self.load_index(&unpacked, &key)?;
                        result.insert(key, value); // later keys overwrite earlier ones
                    }
                }
            }
        }
        Ok(TreewalkValue::Dict(Container::new(Dict::new(self, result))))
    }

    fn evaluate_tuple(&self, items: &[Expr]) -> TreewalkResult<TreewalkValue> {
        let mut results = vec![];
        for item in items {
            let evaluated = self.evaluate_expr(item)?;
            match item {
                Expr::UnaryOperation {
                    op: UnaryOp::Unpack,
                    ..
                } => {
                    for elem in evaluated.as_iterable().raise(self)? {
                        results.push(elem);
                    }
                }
                _ => {
                    results.push(evaluated);
                }
            }
        }

        Ok(TreewalkValue::Tuple(Tuple::new(results)))
    }

    fn evaluate_generator_comprehension(
        &self,
        body: &Expr,
        clauses: &[ForClause],
    ) -> TreewalkResult<TreewalkValue> {
        let generator = Generator::new_from_comprehension(self.state.clone(), body, clauses);
        let iterator = GeneratorIter::new(generator, self.clone());
        Ok(TreewalkValue::Generator(iterator))
    }

    fn evaluate_list_comprehension(
        &self,
        body: &Expr,
        clauses: &[ForClause],
    ) -> TreewalkResult<TreewalkValue> {
        if let Some((first_clause, remaining_clauses)) = clauses.split_first() {
            // Recursive case: Process the first ForClause
            let mut output = vec![];
            for i in self
                .evaluate_expr(&first_clause.iterable)?
                .as_iterable()
                .raise(self)?
            {
                if first_clause.indices.len() == 1 {
                    self.state.write(&first_clause.indices[0], i);
                } else {
                    for (key, value) in first_clause
                        .indices
                        .iter()
                        .zip(i.as_iterable().raise(self)?)
                    {
                        self.state.write(key, value);
                    }
                }

                if let Some(condition) = first_clause.condition.as_ref() {
                    if !self.evaluate_expr(condition)?.coerce_to_boolean() {
                        continue;
                    }
                }

                // Recursively handle the rest of the clauses. If `remaining_clauses` is empty,
                // we'll hit the base case on the next call.
                match self.evaluate_list_comprehension(body, remaining_clauses)? {
                    TreewalkValue::List(list) => output.extend(list),
                    single => output.push(single),
                }
            }

            Ok(TreewalkValue::List(Container::new(List::new(output))))
        } else {
            // Base case: Evaluate the expression. We drop into this case when `clauses` is empty.
            self.evaluate_expr(body)
        }
    }

    fn evaluate_set_comprehension(
        &self,
        body: &Expr,
        clauses: &[ForClause],
    ) -> TreewalkResult<TreewalkValue> {
        let evaluated = self.evaluate_list_comprehension(body, clauses)?;
        let set = Set::try_eval_from(evaluated, self)?;
        Ok(TreewalkValue::Set(Container::new(set)))
    }

    fn evaluate_dict_comprehension(
        &self,
        clauses: &[ForClause],
        key_body: &Expr,
        value_body: &Expr,
    ) -> TreewalkResult<TreewalkValue> {
        let first_clause = match clauses {
            [first] => first,
            _ => unimplemented!(),
        };

        // TODO we are suppressing this clippy error for now because `TreewalkValue` allows interior
        // mutability which can lead to issues when used as a key for a `HashMap` or `HashSet`.
        #[allow(clippy::mutable_key_type)]
        let mut output = HashMap::new();
        for i in self
            .evaluate_expr(&first_clause.iterable)?
            .as_iterable()
            .raise(self)?
        {
            for (key, value) in first_clause
                .indices
                .iter()
                .zip(i.as_iterable().raise(self)?)
            {
                self.state.write(key, value);
            }
            let key_result = self.evaluate_expr(key_body)?;
            let value_result = self.evaluate_expr(value_body)?;
            output.insert(key_result, value_result);
        }
        Ok(TreewalkValue::Dict(Container::new(Dict::new(self, output))))
    }

    fn evaluate_unary_operation(
        &self,
        op: &UnaryOp,
        right: &Expr,
    ) -> TreewalkResult<TreewalkValue> {
        let right = self.evaluate_expr(right)?;
        self.invoke_unary_operation(op, right)
    }

    fn evaluate_binary_operation(
        &self,
        left: &Expr,
        op: &BinOp,
        right: &Expr,
    ) -> TreewalkResult<TreewalkValue> {
        let left = self.evaluate_expr(left)?;
        let right = self.evaluate_expr(right)?;
        self.invoke_binary_op(left, op, right)
    }

    fn evaluate_comparison_chain(
        &self,
        left: &Expr,
        ops: &[(CompareOp, Expr)],
    ) -> TreewalkResult<TreewalkValue> {
        let mut left = self.evaluate_expr(left)?;

        for (op, right) in ops {
            let right = self.evaluate_expr(right)?;
            // is cloning really necessary here?
            let result = self.invoke_compare_op(left, op, right.clone())?;
            if !result.coerce_to_boolean() {
                return Ok(TreewalkValue::Bool(false));
            }
            left = right;
        }

        Ok(TreewalkValue::Bool(true))
    }

    fn evaluate_await(&self, expr: &Expr) -> TreewalkResult<TreewalkValue> {
        let coroutine_to_await = self.evaluate_expr(expr)?.as_coroutine().raise(self)?;

        if let Some(result) = coroutine_to_await.clone().borrow().is_finished_with() {
            Ok(result)
        } else if let Some(ref current_coroutine) =
            self.with_executor(|exec| exec.current_coroutine().clone())
        {
            self.with_executor(|exec| {
                exec.set_wait_on(current_coroutine.clone(), coroutine_to_await)
            });
            Err(TreewalkDisruption::Signal(TreewalkSignal::Await))
        } else {
            ExecutionError::type_error("Expected a coroutine").raise(self)
        }
    }

    fn evaluate_function_call(
        &self,
        callee: &Callee,
        call_args: &CallArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let args = self.evaluate_args(call_args)?;

        let function = match callee {
            Callee::Expr(callee) => self.evaluate_expr(callee)?.as_callable().raise(self)?,
            Callee::Symbol(name) => self.load_callable(name)?,
        };

        self.call(function, args)
    }

    fn evaluate_class_instantiation(
        &self,
        name: &str,
        call_args: &CallArgs,
    ) -> TreewalkResult<TreewalkValue> {
        log(LogLevel::Debug, || format!("Instantiating: {name}"));
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

        let class = self.load_callable(name)?;
        let args = self.evaluate_args(call_args)?;
        self.call(class, args)
    }

    fn evaluate_logical_operation(
        &self,
        left: &Expr,
        op: &LogicalOp,
        right: &Expr,
    ) -> TreewalkResult<TreewalkValue> {
        let left = self.evaluate_expr(left)?;
        let right = self.evaluate_expr(right)?;
        self.invoke_logical_op(left, op, right)
    }

    fn evaluate_ternary_operation(
        &self,
        condition: &Expr,
        if_value: &Expr,
        else_value: &Expr,
    ) -> TreewalkResult<TreewalkValue> {
        if self.evaluate_expr(condition)?.coerce_to_boolean() {
            self.evaluate_expr(if_value)
        } else {
            self.evaluate_expr(else_value)
        }
    }

    fn evaluate_member_access(&self, object: &Expr, field: &str) -> TreewalkResult<TreewalkValue> {
        let result = self.evaluate_expr(object)?;
        self.load_member(&result, field)
    }

    fn evaluate_index_access(&self, object: &Expr, index: &Expr) -> TreewalkResult<TreewalkValue> {
        let object_result = self.evaluate_expr(object)?;
        let index_result = self.evaluate_expr(index)?;

        self.load_index(&object_result, &index_result)
    }

    fn evaluate_slice_operation(
        &self,
        object: &Expr,
        params: &SliceParams,
    ) -> TreewalkResult<TreewalkValue> {
        let object_result = self.evaluate_expr(object)?;
        let slice = Slice::resolve(self, params)?;

        self.load_index(&object_result, &TreewalkValue::Slice(slice))
    }

    fn evaluate_f_string(&self, parts: &[FStringPart]) -> TreewalkResult<TreewalkValue> {
        let mut result = String::new();
        for part in parts {
            match part {
                FStringPart::String(s) => {
                    result.push_str(s);
                }
                FStringPart::Expr(e) => {
                    let r = self.evaluate_expr(&e.expr)?;
                    write!(result, "{r}").unwrap();
                }
            }
        }

        Ok(TreewalkValue::Str(Str::from(result)))
    }

    fn evaluate_lambda(&self, params: &Params, expr: &Expr) -> TreewalkResult<TreewalkValue> {
        let block = Ast::from_expr(expr.clone());
        let runtime_params = self.evaluate_params(params)?;

        let function = Container::new(Function::new_lambda(
            self.state.clone(),
            runtime_params,
            block,
        ));

        Ok(TreewalkValue::Function(function))
    }

    fn evaluate_type_node(&self, type_node: &TypeNode) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::TypeNode(type_node.into()))
    }

    fn evaluate_yield(&self, expr: &Option<Box<Expr>>) -> TreewalkResult<TreewalkValue> {
        let value = match expr {
            None => TreewalkValue::None,
            Some(e) => self.evaluate_expr(e)?,
        };
        Err(TreewalkDisruption::Signal(TreewalkSignal::Yield(value)))
    }

    fn evaluate_yield_from(&self, expr: &Expr) -> TreewalkResult<TreewalkValue> {
        let gen = self.evaluate_expr(expr)?;
        Err(TreewalkDisruption::Signal(TreewalkSignal::YieldFrom(gen)))
    }
}
