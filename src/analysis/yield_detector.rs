use crate::parser::types::{Ast, Expr, Statement, StatementKind};

use super::visitor::Visitor;

pub struct YieldDetector {
    pub found_yield: bool,
}

impl YieldDetector {
    pub fn new() -> Self {
        Self { found_yield: false }
    }
}

impl Visitor for YieldDetector {
    fn visit_ast(&mut self, _program: &Ast) {}

    fn visit_statement(&mut self, statement: &Statement) {
        if matches!(
            statement.kind,
            StatementKind::Expression(Expr::Yield(_) | Expr::YieldFrom(_))
        ) {
            self.found_yield = true;
        }
    }
}
