use crate::{
    core::{log, Container, LogLevel},
    domain::{
        resolve_absolute_path, DomainResult, Dunder, ExceptionLiteral, ExecutionError,
        FromImportPath, Identifier,
    },
    parser::types::{
        Ast, BinOp, CompoundOperator, ConditionalAst, ExceptClause, ExceptionInstance, Expr,
        FromImportMode, LoopIndex, Params, RegularImport, Statement, StatementKind,
    },
    treewalk::{
        protocols::MemberRead,
        result::Raise,
        types::{Function, StopIteration, Tuple},
        utils::{args, Args},
        TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn evaluate_statement(&self, stmt: &Statement) -> TreewalkResult<TreewalkValue> {
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
            StatementKind::WhileLoop(cond_ast) => self.evaluate_while_loop(cond_ast),
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
            StatementKind::SelectiveImport { import_path, mode } => {
                self.evaluate_selective_import(import_path, mode)
            }
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

        Ok(TreewalkValue::None)
    }

    fn evaluate_return(&self, exprs: &[Expr]) -> TreewalkResult<TreewalkValue> {
        assert!(!exprs.is_empty());

        let results = exprs
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<Vec<_>, _>>()?;

        let return_val = if results.len() > 1 {
            TreewalkValue::Tuple(Tuple::new(results))
        } else {
            results[0].clone()
        };

        Err(TreewalkDisruption::Signal(TreewalkSignal::Return(
            return_val,
        )))
    }

    fn evaluate_assert(&self, expr: &Expr) -> TreewalkResult<()> {
        if self.evaluate_expr(expr)?.coerce_to_boolean() {
            Ok(())
        } else {
            ExecutionError::assertion_error().raise(self)
        }
    }

    fn evaluate_delete(&self, exprs: &[Expr]) -> TreewalkResult<()> {
        for expr in exprs {
            match expr {
                Expr::Variable(name) => {
                    self.state.delete(name.as_str());
                }
                Expr::IndexAccess { object, index } => {
                    let index_result = self.evaluate_expr(index)?;
                    let object_result = self.evaluate_expr(object)?;
                    object_result
                        .clone()
                        .into_index_write(self)?
                        .ok_or_else(|| {
                            ExecutionError::type_error(format!(
                                "'{}' object does not support item deletion",
                                object_result.get_type()
                            ))
                        })
                        .raise(self)?
                        .delitem(self, index_result)?;
                }
                Expr::MemberAccess { object, field } => {
                    let result = self.evaluate_expr(object)?;
                    result
                        .clone()
                        .into_member_writer()
                        .ok_or_else(|| {
                            ExecutionError::attribute_error(result.class_name(self), field.as_str())
                        })
                        .raise(self)?
                        .delete_member(self, field.as_str())?;
                }
                _ => return ExecutionError::type_error("cannot delete").raise(self),
            }
        }

        Ok(())
    }

    /// TODO This should be moved to the semantic analysis
    fn validate_nonlocal_context(&self, name: &Identifier) -> TreewalkResult<()> {
        // We could not find the variable `name` in an enclosing context.
        if let Some(env) = self.state.read_captured_env() {
            if env.borrow().read(name.as_str()).is_none() {
                return Err(self.raise(ExecutionError::SyntaxError));
            }
        }

        // `nonlocal` cannot be used at the module-level (outside of a function,
        // i.e. captured environment).
        if self.state.read_captured_env().is_none() {
            return Err(self.raise(ExecutionError::SyntaxError));
        }

        Ok(())
    }

    fn evaluate_nonlocal(&self, names: &[Identifier]) -> TreewalkResult<()> {
        for name in names {
            self.validate_nonlocal_context(name)?;
            self.state.mark_nonlocal(name.as_str());
        }

        Ok(())
    }

    fn evaluate_global(&self, names: &[Identifier]) -> TreewalkResult<()> {
        for name in names {
            self.state.mark_global(name.as_str());
        }

        Ok(())
    }

    fn evaluate_assignment(&self, name: &Expr, expr: &Expr) -> TreewalkResult<()> {
        let result = self.evaluate_expr(expr)?;
        self.execute_assignment(name, result)
    }

    fn evaluate_multiple_assignment(&self, left: &[Expr], expr: &Expr) -> TreewalkResult<()> {
        for name in left {
            let value = self.evaluate_expr(expr)?;
            self.execute_assignment(name, value)?;
        }

        Ok(())
    }

    /// Python can unpack any iterables, not any index reads.
    fn evaluate_unpacking_assignment(&self, left: &[Expr], right: &Expr) -> TreewalkResult<()> {
        let right_result = self
            .evaluate_expr(right)?
            .as_iterable()
            .raise(self)?
            .into_iter();

        // Collect the items once so that we can get a length without clearing our iterator, some
        // of which (`ListIter`, etc) use interior mutability to track iterator state.
        let right_items: Vec<_> = right_result.clone_box().collect();
        let right_len = right_items.len();

        if left.len() < right_len {
            ExecutionError::value_error(format!(
                "too many values to unpack (expected {})",
                left.len()
            ))
            .raise(self)
        } else if left.len() > right_len {
            ExecutionError::value_error(format!(
                "not enough values to unpack (expected {}, got {})",
                left.len(),
                right_len
            ))
            .raise(self)
        } else {
            for (key, value) in left.iter().zip(right_items) {
                self.execute_assignment(key, value)?;
            }
            Ok(())
        }
    }

    fn evaluate_compound_assignment(
        &self,
        operator: &CompoundOperator,
        target: &Expr,
        value: &Expr,
    ) -> TreewalkResult<()> {
        let bin_op = BinOp::from(operator);
        let expr = Expr::BinaryOperation {
            left: Box::new(target.clone()),
            op: bin_op,
            right: Box::new(value.clone()),
        };
        let result = self.evaluate_expr(&expr)?;
        self.execute_assignment(target, result)
    }

    fn evaluate_function_def(
        &self,
        name: &Identifier,
        params: &Params,
        body: &Ast,
        decorators: &[Expr],
        is_async: &bool,
        line_number: usize,
    ) -> TreewalkResult<()> {
        // We must evaluate each parameter's default once at function-definition time.
        let runtime_params = self.evaluate_params(params)?;

        let function = Container::new(Function::new(
            self.state.clone(),
            name.as_str(),
            runtime_params,
            body.clone(),
            *is_async,
            line_number,
        ));

        // Decorators are applied to a function when it is defined and then the decorated version
        // is written into the symbol table.
        let result = self.apply_decorators(function, decorators)?;

        // We should note that what we write here it not always a `Function` or even a `Callable`.
        // In the case of the `@property` decorator, what is written to the symbol table is a
        // `MemberDescriptor`.
        self.state.write(name.as_str(), result);
        Ok(())
    }

    /// At most one of the Blocks will be evaluated, once we know which one we can return the
    /// result early.
    fn evaluate_if_else(
        &self,
        if_part: &ConditionalAst,
        elif_parts: &[ConditionalAst],
        else_part: &Option<Ast>,
    ) -> TreewalkResult<()> {
        let if_condition_result = self.evaluate_expr(&if_part.condition)?;
        if if_condition_result.coerce_to_boolean() {
            self.execute_ast(&if_part.ast)?;
            return Ok(());
        }

        for elif_part in elif_parts {
            let elif_condition_result = self.evaluate_expr(&elif_part.condition)?;
            if elif_condition_result.coerce_to_boolean() {
                self.execute_ast(&elif_part.ast)?;
                return Ok(());
            }
        }

        if let Some(else_part) = else_part {
            self.execute_ast(else_part)?;
            return Ok(());
        }

        Ok(())
    }

    fn evaluate_while_loop(&self, cond_ast: &ConditionalAst) -> TreewalkResult<()> {
        while self.evaluate_expr(&cond_ast.condition)?.coerce_to_boolean() {
            match self.execute_ast(&cond_ast.ast) {
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

    fn evaluate_for_in_loop(
        &self,
        index: &LoopIndex,
        range: &Expr,
        body: &Ast,
        else_block: &Option<Ast>,
    ) -> TreewalkResult<()> {
        let mut encountered_break = false;

        for val_for_iteration in self.evaluate_expr(range)?.as_iterable().raise(self)? {
            self.execute_loop_index_assignment(index, val_for_iteration)?;

            match self.execute_ast(body) {
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
                self.execute_ast(else_block)?;
            }
        }

        Ok(())
    }

    fn evaluate_class_definition(
        &self,
        name: &Identifier,
        parents: &[Expr],
        metaclass: &Option<Identifier>,
        body: &Ast,
    ) -> TreewalkResult<()> {
        log(LogLevel::Debug, || format!("Defining class: {name}"));
        let parent_classes = parents
            .iter()
            .map(|p| self.evaluate_expr(p))
            .collect::<Result<Vec<_>, _>>()?
            .iter()
            .map(|f| f.as_class())
            .collect::<DomainResult<Vec<_>>>()
            .raise(self)?;

        let metaclass = metaclass
            .as_ref()
            .and_then(|p| self.state.read(p.as_str()))
            .map(|d| d.as_class())
            .transpose()
            .raise(self)?;

        // We will update the scope on this class before we write it to the symbol table, but we
        // must instantiate the class here so we can get a reference that can be associated with
        // each function defined inside it.
        let class = self.build_class(name.as_str(), parent_classes, metaclass)?;

        // We must use the class scope here in case it received any initialization from its
        // metaclass `Dunder::New` method.
        self.state
            .push_local(Container::new(class.borrow().scope.clone()));
        self.state.push_class(class.clone());
        self.execute_ast(body)?;
        self.state.pop_class();
        class.borrow_mut().scope = self
            .state
            .pop_local()
            .ok_or_else(ExecutionError::runtime_error)
            .raise(self)?
            .borrow()
            .clone();

        self.state.write(name.as_str(), TreewalkValue::Class(class));

        Ok(())
    }

    fn evaluate_regular_import(&self, items: &[RegularImport]) -> TreewalkResult<()> {
        for item in items.iter() {
            let module_name = resolve_absolute_path(&item.module_path);
            let module = self.load_module(&module_name)?;
            self.execute_import(&module_name, module, &item.alias)?;
        }

        Ok(())
    }

    fn evaluate_selective_import(
        &self,
        import_path: &FromImportPath,
        mode: &FromImportMode,
    ) -> TreewalkResult<()> {
        // Resolve the module name (handles relative imports correctly)
        let module_name = self.state.resolve_import_path(import_path).raise(self)?;
        let module = self.load_module(&module_name)?.as_module().raise(self)?;

        match mode {
            // --------------------------
            //   from x import *
            // --------------------------
            FromImportMode::All => {
                for module_symbol in module.dir() {
                    let symbol = module_symbol.as_str();

                    let value = module
                        .get_member(self, symbol)?
                        .ok_or_else(|| ExecutionError::name_error(symbol))
                        .raise(self)?;

                    self.state.write(symbol, value);
                }
            }

            // -----------------------------------------
            //   from x import a, b as c, d as e, ...
            // -----------------------------------------
            FromImportMode::List(items) => {
                for item in items {
                    let original = item.original();
                    let imported = item.imported();

                    let value = module
                        .get_member(self, original.as_str())?
                        .ok_or_else(|| ExecutionError::name_error(original.as_str()))
                        .raise(self)?;

                    self.state.write(imported.as_str(), value);
                }
            }
        }

        Ok(())
    }

    fn evaluate_try_except(
        &self,
        try_block: &Ast,
        except_clauses: &[ExceptClause],
        else_block: &Option<Ast>,
        finally_block: &Option<Ast>,
    ) -> TreewalkResult<()> {
        if let Err(TreewalkDisruption::Error(error)) = self.execute_ast(try_block) {
            // Only the first matching clause should be evaluated. They will still be in order
            // here from the parsed code.
            if let Some(except_clause) = except_clauses
                .iter()
                .find(|clause| error.matches_except_clause(&clause.exception_types))
            {
                if let Some(alias) = &except_clause.alias {
                    let exception = match &error.execution_error {
                        ExecutionError::StopIteration(v) => {
                            let stop_iter_payload = v.clone().unwrap_treewalk();
                            TreewalkValue::StopIteration(Box::new(StopIteration::new(
                                stop_iter_payload,
                            )))
                        }
                        _ => TreewalkValue::Exception(error.clone()),
                    };
                    self.state.write(alias.as_str(), exception);
                }

                match self.execute_ast(&except_clause.block) {
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
            self.execute_ast(else_block)?;
        }

        // Finally block is evaluated always if it exists
        if let Some(finally_block) = finally_block {
            self.execute_ast(finally_block)?;
        }

        Ok(())
    }

    fn evaluate_raise(&self, instance: &Option<ExceptionInstance>) -> TreewalkResult<()> {
        // TODO we should throw a 'RuntimeError: No active exception to reraise'
        if instance.is_none() {
            return Err(TreewalkDisruption::Signal(TreewalkSignal::Raise));
        }

        let instance = instance.as_ref().unwrap();
        let args = self.evaluate_args(&instance.args)?;
        let error = match instance.literal {
            ExceptionLiteral::TypeError => {
                if args.len() == 1 {
                    ExecutionError::type_error(args.get_arg(0).as_str().raise(self)?)
                } else {
                    ExecutionError::type_error_empty()
                }
            }
            _ => unimplemented!(),
        };

        error.raise(self)
    }

    fn evaluate_context_manager(
        &self,
        expr: &Expr,
        variable: &Option<Identifier>,
        block: &Ast,
    ) -> TreewalkResult<()> {
        let expr_result = self.evaluate_expr(expr)?;
        // TODO this probably doesn't need to be an object
        let object = expr_result.as_object().raise(self)?;

        if object.get_member(self, &Dunder::Enter)?.is_none()
            || object.get_member(self, &Dunder::Exit)?.is_none()
        {
            return Err(self.raise(ExecutionError::MissingContextManagerProtocol));
        }

        let result = self.call_method(&expr_result, Dunder::Enter, args![])?;

        if let Some(variable) = variable {
            self.state.write(variable.as_str(), result);
        }
        let block_result = self.execute_ast(block);

        self.call_method(
            &expr_result,
            Dunder::Exit,
            args![
                TreewalkValue::None,
                TreewalkValue::None,
                TreewalkValue::None
            ],
        )?;

        // Return the exception if one is called.
        block_result?;

        Ok(())
    }
}
