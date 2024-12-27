use crate::{
    core::Container,
    parser::types::{Ast, Expr, ForClause, LoopIndex, Statement},
    treewalk::{
        types::{
            pausable::{Frame, Pausable, PausableContext, PausableState, PausableStepResult},
            ExprResult, Function,
        },
        Interpreter, Scope, State,
    },
    types::errors::InterpreterError,
};

pub struct Generator {
    scope: Container<Scope>,
    function: Container<Function>,
    context: Container<PausableContext>,
}

impl Generator {
    pub fn new(scope: Container<Scope>, function: Container<Function>) -> Self {
        let frame = Frame::new(function.borrow().clone().body);

        Self {
            scope,
            function,
            context: PausableContext::new(frame),
        }
    }

    pub fn new_from_comprehension(
        state: Container<State>,
        body: &Expr,
        clauses: &[ForClause],
    ) -> Self {
        let generator_body = Self::build_nested_loops(body, clauses);
        let function = Container::new(Function::new_anonymous_generator(state, generator_body));
        Self::new(Container::new(Scope::default()), function)
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

            let for_in_loop = Statement::ForInLoop {
                index: LoopIndex::Variable(index.to_string()),
                iterable: first_clause.iterable.clone(),
                body: loop_body,
                else_block: None,
            };

            Ast::new(vec![for_in_loop])
        } else {
            unreachable!()
        }
    }
}

impl Pausable for Container<Generator> {
    fn context(&self) -> Container<PausableContext> {
        self.borrow().context.clone()
    }

    fn scope(&self) -> Container<Scope> {
        self.borrow().scope.clone()
    }

    fn function(&self) -> Container<Function> {
        self.borrow().function.clone()
    }

    fn set_scope(&self, scope: Container<Scope>) {
        self.borrow_mut().scope = scope;
    }

    fn finish(
        &self,
        interpreter: &Interpreter,
        _result: ExprResult,
    ) -> Result<ExprResult, InterpreterError> {
        Err(InterpreterError::StopIteration(
            interpreter.state.call_stack(),
        ))
    }

    fn handle_step(
        &self,
        interpreter: &Interpreter,
        statement: Statement,
        control_flow: bool,
    ) -> Result<PausableStepResult, InterpreterError> {
        match self.execute_statement(interpreter, statement, control_flow)? {
            Some(yielded) => {
                self.on_exit(interpreter);
                Ok(PausableStepResult::BreakAndReturn(yielded))
            }
            None => Ok(PausableStepResult::NoOp),
        }
    }
}

impl Container<Generator> {
    /// By this point, all control flow statements have already been handled manually. Evaluate all
    /// other statements unless we encounter a yield.
    ///
    /// Only yield statements will cause a value to be returned, everything else will return
    /// `None`.
    fn execute_statement(
        &self,
        interpreter: &Interpreter,
        statement: Statement,
        control_flow: bool,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        if !control_flow {
            match statement {
                Statement::Expression(Expr::Yield(None)) => Ok(None),
                Statement::Expression(Expr::Yield(Some(expr))) => {
                    Ok(Some(interpreter.evaluate_expr(&expr)?))
                }
                Statement::Expression(Expr::YieldFrom(_)) => unimplemented!(),
                _ => {
                    // would we ever need to support a return statement here?
                    let _ = interpreter.evaluate_statement(&statement)?;
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }
}

#[derive(Clone)]
pub struct GeneratorIterator {
    pub generator: Container<Generator>,
    pub interpreter: Interpreter,
}

impl GeneratorIterator {
    pub fn new(generator: Generator, interpreter: Interpreter) -> Self {
        Self {
            generator: Container::new(generator),
            interpreter,
        }
    }
}

impl Iterator for GeneratorIterator {
    type Item = ExprResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.generator.context().current_state() == PausableState::Finished {
            return None;
        }

        // we need a better way to surface error during a generator run
        match self.generator.run_until_pause(&self.interpreter) {
            Ok(result) => Some(result),
            Err(InterpreterError::StopIteration(_)) => None,
            _ => panic!("Unexpected error during generator run."),
        }
    }
}

impl IntoIterator for Container<GeneratorIterator> {
    type Item = ExprResult;
    type IntoIter = GeneratorIterator;

    fn into_iter(self) -> Self::IntoIter {
        self.borrow().clone()
    }
}
