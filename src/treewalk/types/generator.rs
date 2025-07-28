use crate::{
    core::Container,
    parser::types::{Ast, Expr, ForClause, LoopIndex, Statement, StatementKind},
    treewalk::{
        pausable::{Frame, Pausable, PausableContext, PausableStepResult},
        protocols::Iterable,
        type_system::CloneableIterable,
        types::Function,
        Scope, TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal,
        TreewalkState, TreewalkValue,
    },
};

pub struct Generator {
    scope: Container<Scope>,
    context: PausableContext,
    delegated: Option<Box<dyn CloneableIterable>>,
}

impl Generator {
    pub fn new(scope: Container<Scope>, function: Container<Function>) -> Self {
        let frame = Frame::new(function.borrow().clone().body);

        Self {
            scope,
            context: PausableContext::new(frame),
            delegated: None,
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
        &mut self,
        interpreter: &TreewalkInterpreter,
        stmt: Statement,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        match interpreter.evaluate_statement(&stmt) {
            Ok(_) => Ok(None),
            Err(TreewalkDisruption::Signal(TreewalkSignal::Return(val))) => {
                Err(interpreter.stop_iteration_with(val))
            }
            Err(TreewalkDisruption::Signal(TreewalkSignal::Yield(val))) => Ok(Some(val)),
            Err(TreewalkDisruption::Signal(TreewalkSignal::YieldFrom(val))) => {
                if self.delegated.is_none() {
                    let iter = val.expect_iterator(interpreter)?;
                    self.delegated = Some(iter);
                }

                // This is a bit leaky because we not only initializing the delegation here, we're
                // also kicking it off. Ideally, we'd do this step later.
                match self.delegated.as_mut().unwrap().try_next()? {
                    Some(val) => Ok(Some(val)),
                    // We can only hit this if the iterable we are calling yield from on is
                    // empty.
                    // This matches Python's behavior for: `yield from []`
                    None => Err(interpreter.stop_iteration()),
                }
            }
            Err(e) => Err(e),
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
    ) -> TreewalkResult<PausableStepResult> {
        match self.execute_statement(interpreter, stmt)? {
            Some(yielded) => {
                self.on_exit(interpreter);
                Ok(PausableStepResult::BreakAndReturn(yielded))
            }
            None => Ok(PausableStepResult::NoOp),
        }
    }

    fn clear_delegated(&mut self) {
        self.delegated = None;
    }

    fn delegated(&mut self) -> Option<&mut Box<dyn CloneableIterable>> {
        self.delegated.as_mut()
    }
}

#[derive(Clone)]
pub struct GeneratorIter {
    generator: Container<Generator>,
    interpreter: TreewalkInterpreter,
}

impl GeneratorIter {
    pub fn new(generator: Generator, interpreter: TreewalkInterpreter) -> Self {
        Self {
            generator: Container::new(generator),
            interpreter,
        }
    }

    pub fn run_until_pause(&mut self) -> TreewalkResult<TreewalkValue> {
        self.generator
            .borrow_mut()
            .run_until_pause(&self.interpreter)
    }
}

impl Iterable for GeneratorIter {
    // We cannot use the boilerplate impl_iterable! here because we want to surface any
    // StopIteration errors, not swallow them the way Iterator::next does.
    fn try_next(&mut self) -> TreewalkResult<Option<TreewalkValue>> {
        match self.run_until_pause() {
            // is this right?
            Ok(r) => Ok(Some(r)),
            Err(e) => Err(e),
        }
    }
}
