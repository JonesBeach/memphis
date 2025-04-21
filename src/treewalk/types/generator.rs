use crate::{
    core::Container,
    domain::ExecutionErrorKind,
    parser::types::{Ast, Expr, ForClause, LoopIndex, Statement, StatementKind},
    treewalk::{
        macros::*,
        pausable::{Frame, Pausable, PausableContext, PausableState, PausableStepResult},
        types::Function,
        Scope, TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkState,
        TreewalkValue,
    },
};

impl_iterable!(GeneratorIterator);

pub struct Generator {
    scope: Container<Scope>,
    context: PausableContext,
}

impl Generator {
    pub fn new(scope: Container<Scope>, function: Container<Function>) -> Self {
        let frame = Frame::new(function.borrow().clone().body);

        Self {
            scope,
            context: PausableContext::new(frame),
        }
    }

    pub fn new_from_comprehension(
        state: Container<TreewalkState>,
        body: &Expr,
        clauses: &[ForClause],
    ) -> Self {
        let generator_body = Self::build_nested_loops(body, clauses);
        let function = Container::new(Function::new_anonymous_generator(state, generator_body));
        Self::new(Container::new(Scope::default()), function)
    }

    /// By this point, all control flow statements have already been handled manually. Evaluate all
    /// other statements unless we encounter a yield.
    ///
    /// Only yield statements will cause a value to be returned, everything else will return
    /// `None`.
    fn execute_statement(
        &self,
        interpreter: &TreewalkInterpreter,
        stmt: Statement,
        control_flow: bool,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        if !control_flow {
            match &stmt.kind {
                StatementKind::Expression(Expr::Yield(None)) => Ok(None),
                StatementKind::Expression(Expr::Yield(Some(expr))) => {
                    Ok(Some(interpreter.evaluate_expr(expr)?))
                }
                StatementKind::Expression(Expr::YieldFrom(_)) => unimplemented!(),
                _ => {
                    // would we ever need to support a return statement here?
                    let _ = interpreter.evaluate_statement(&stmt)?;
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }

    // This is a utility which takes the parsed elements found in a generator comprehension and
    // recursively builds a generator function out of them. This will then become the body of the
    // function provided to a generator.
    fn build_nested_loops(body: &Expr, clauses: &[ForClause]) -> Ast {
        if let Some((first_clause, remaining_clauses)) = clauses.split_first() {
            let loop_body = if remaining_clauses.is_empty() {
                // Base case: Yield the body
                Ast::from_expr(Expr::Yield(Some(Box::new(body.clone()))))
            } else {
                // Recursive case: Build nested loop for the remaining clauses
                Self::build_nested_loops(body, remaining_clauses)
            };

            let index = if first_clause.indices.len() == 1 {
                &first_clause.indices[0]
            } else {
                // This is if we need to unpack multiple variables
                unimplemented!()
            };

            let for_in_loop = Statement::new(
                1,
                StatementKind::ForInLoop {
                    index: LoopIndex::Variable(index.to_string()),
                    iterable: first_clause.iterable.clone(),
                    body: loop_body,
                    else_block: None,
                },
            );

            Ast::new(vec![for_in_loop])
        } else {
            unreachable!()
        }
    }
}

impl Pausable for Generator {
    fn context(&self) -> &PausableContext {
        &self.context
    }

    fn context_mut(&mut self) -> &mut PausableContext {
        &mut self.context
    }

    fn scope(&self) -> Container<Scope> {
        self.scope.clone()
    }

    fn set_scope(&mut self, scope: Container<Scope>) {
        self.scope = scope;
    }

    fn finish(
        &mut self,
        interpreter: &TreewalkInterpreter,
        _result: TreewalkValue,
    ) -> TreewalkResult<TreewalkValue> {
        Err(interpreter.stop_iteration())
    }

    fn handle_step(
        &mut self,
        interpreter: &TreewalkInterpreter,
        stmt: Statement,
        control_flow: bool,
    ) -> TreewalkResult<PausableStepResult> {
        match self.execute_statement(interpreter, stmt, control_flow)? {
            Some(yielded) => {
                self.on_exit(interpreter);
                Ok(PausableStepResult::BreakAndReturn(yielded))
            }
            None => Ok(PausableStepResult::NoOp),
        }
    }
}

#[derive(Clone)]
pub struct GeneratorIterator {
    pub generator: Container<Generator>,
    pub interpreter: TreewalkInterpreter,
}

impl GeneratorIterator {
    pub fn new(generator: Generator, interpreter: TreewalkInterpreter) -> Self {
        Self {
            generator: Container::new(generator),
            interpreter,
        }
    }
}

impl Iterator for GeneratorIterator {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.generator.borrow().context().current_state() == PausableState::Finished {
            return None;
        }

        // we need a better way to surface error during a generator run
        match self
            .generator
            .borrow_mut()
            .run_until_pause(&self.interpreter)
        {
            Ok(result) => Some(result),
            Err(TreewalkDisruption::Error(e))
                if matches!(e.execution_error_kind, ExecutionErrorKind::StopIteration) =>
            {
                None
            }
            _ => panic!("Unexpected error during generator run."),
        }
    }
}
