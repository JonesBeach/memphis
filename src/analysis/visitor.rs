use crate::parser::types::{Ast, Statement, StatementKind};

/// Core visitor trait used to traverse the AST.
pub trait Visitor {
    fn visit_ast(&mut self, program: &Ast);
    fn visit_statement(&mut self, statement: &Statement);
}

/// Trait for anything that can be visited by a `Visitor`.
pub trait AcceptsVisitor {
    fn accept<V: Visitor>(&self, visitor: &mut V);
}

impl AcceptsVisitor for Ast {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_ast(self);
        for statement in self.iter() {
            statement.accept(visitor);
        }
    }
}

impl AcceptsVisitor for Statement {
    /// Visit this statement, then walk the AST to any nested blocks.
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_statement(self);

        match &self.kind {
            StatementKind::FunctionDef { body, .. } => {
                body.accept(visitor);
            }
            StatementKind::WhileLoop(cond_ast) => {
                cond_ast.ast.accept(visitor);
            }
            StatementKind::ForInLoop { body, .. } => {
                body.accept(visitor);
            }
            StatementKind::ContextManager { block, .. } => {
                block.accept(visitor);
            }
            StatementKind::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => {
                if_part.ast.accept(visitor);
                for part in elif_parts {
                    part.ast.accept(visitor);
                }

                if let Some(else_part) = else_part {
                    else_part.accept(visitor);
                }
            }
            _ => {}
        }
    }
}
