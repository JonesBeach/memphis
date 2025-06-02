use std::collections::HashSet;

use crate::{
    core::{log, LogLevel},
    parser::types::{Ast, Expr, Statement, StatementKind, Variable},
};

use super::visitor::Visitor;

#[derive(Debug)]
pub struct FunctionAnalysisVisitor {
    local_vars: HashSet<Variable>,
    accessed_vars: Vec<Variable>,
}

impl Default for FunctionAnalysisVisitor {
    fn default() -> Self {
        Self::new()
    }
}

impl FunctionAnalysisVisitor {
    pub fn new() -> Self {
        FunctionAnalysisVisitor {
            local_vars: HashSet::new(),
            accessed_vars: vec![],
        }
    }

    /// We return a Vec<_> here because the order is defined as the order in which the variables
    /// are accessed.
    pub fn get_free_vars(&self) -> Vec<Variable> {
        self.accessed_vars
            .iter()
            .filter(|item| !self.local_vars.contains(*item))
            .cloned()
            .collect()
    }

    fn check_for_local_vars(&mut self, statement: &StatementKind) {
        match statement {
            StatementKind::UnpackingAssignment { left, .. } => {
                for var in left.iter() {
                    if let Some(name) = var.as_variable() {
                        self.local_vars.insert(name);
                    }
                }
            }
            StatementKind::Assignment { left, .. } => {
                if let Some(name) = left.as_variable() {
                    self.local_vars.insert(name);
                }
            }
            StatementKind::CompoundAssignment { target, .. } => {
                if let Some(name) = target.as_variable() {
                    self.local_vars.insert(name);
                }
            }
            _ => {}
        }
    }

    // When we still did this in the parser, we had this check and comment about not considering
    // modules as accessed. We probably want to add that back here.
    //
    // /// The check with `is_module` is a little bit weird but ultimately makes sense.
    // ///
    // /// A module does not need to be saved in a closure.
    // /// ```
    // /// asyncio.create_task(task1())
    // /// ```
    // ///
    // /// Objects or variables do.
    // /// ```
    // /// print(b)
    // /// print(c.attr)
    // /// ```
    // fn save_accessed_var(&mut self, name: &str) {
    //     if !self.state.is_module(name) {
    //         self.function_context_stack.with_top_mut(|context| {
    //             context.insert_accessed_var(name.to_string());
    //         });
    //     }
    // }
    // There are more cases where variables might be accessed we should add here, such as
    // variable reads, attribute accesses on objects.
    fn check_for_accessed_vars(&mut self, statement: &StatementKind) {
        match statement {
            StatementKind::Expression(Expr::FunctionCall { args, .. }) => {
                for arg in args.args.iter() {
                    if let Some(name) = arg.as_variable() {
                        self.accessed_vars.push(name);
                    }
                }
            }
            StatementKind::Global(names) | StatementKind::Nonlocal(names) => {
                for name in names {
                    self.accessed_vars.push(name.clone());
                }
            }
            _ => {}
        }
    }
}

impl Visitor for FunctionAnalysisVisitor {
    fn visit_ast(&mut self, _program: &Ast) {}

    fn visit_statement(&mut self, statement: &Statement) {
        log(LogLevel::Trace, || format!("Visiting {:?}", statement));

        self.check_for_local_vars(&statement.kind);
        self.check_for_accessed_vars(&statement.kind);
    }
}
