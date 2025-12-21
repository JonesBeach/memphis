use crate::parser::types::{Statement, StatementKind};

macro_rules! assert_ast_eq {
    ($input:expr, $expected:expr) => {
        let ast = parse!($input, Statement);
        assert_stmt_eq!(ast, $expected);
    };
    ($input:expr, $expected:expr, $pattern:ident) => {
        let ast = parse!($input, $pattern);
        assert_eq!(ast, $expected);
    };
}

macro_rules! assert_stmt_eq {
    ($actual:expr, $expected:expr) => {
        assert_stmt_eq_inner(&$actual, &$expected)
    };
}

macro_rules! assert_cond_ast_eq {
    ($actual:expr, $expected:expr) => {
        assert_eq!($actual.condition, $expected.condition, "Condition mismatch");
        assert_eq!(
            $actual.ast.len(),
            $expected.ast.len(),
            "Ast length mismatch"
        );
        for (a, e) in $actual.ast.iter().zip($expected.ast.iter()) {
            assert_stmt_eq!(a, e);
        }
    };
}

pub fn assert_stmt_eq_inner(actual: &Statement, expected: &Statement) {
    match (&actual.kind, &expected.kind) {
        // Function definitions (compare nested body statements)
        (
            StatementKind::FunctionDef {
                name: actual_name,
                args: actual_args,
                body: actual_body,
                decorators: actual_decorators,
                is_async: actual_is_async,
            },
            StatementKind::FunctionDef {
                name: expected_name,
                args: expected_args,
                body: expected_body,
                decorators: expected_decorators,
                is_async: expected_is_async,
            },
        ) => {
            assert_eq!(
                actual_body.len(),
                expected_body.len(),
                "Function body length mismatch"
            );
            for (a, e) in actual_body.iter().zip(expected_body.iter()) {
                assert_stmt_eq_inner(a, e);
            }

            assert_eq!(actual_name, expected_name, "Function name mismatch");
            assert_eq!(actual_args, expected_args, "Function args mismatch");
            assert_eq!(
                actual_decorators, expected_decorators,
                "Function decorators mismatch"
            );
            assert_eq!(
                actual_is_async, expected_is_async,
                "Function is_async mismatch"
            );
        }

        (
            StatementKind::ClassDef {
                name: actual_name,
                parents: actual_parents,
                metaclass: actual_metaclass,
                body: actual_body,
            },
            StatementKind::ClassDef {
                name: expected_name,
                parents: expected_parents,
                metaclass: expected_metaclass,
                body: expected_body,
            },
        ) => {
            assert_eq!(actual_name, expected_name, "Class name mismatch");
            assert_eq!(actual_parents, expected_parents, "Class parents mismatch");
            assert_eq!(
                actual_metaclass, expected_metaclass,
                "Class metaclass mismatch"
            );

            assert_eq!(
                actual_body.len(),
                expected_body.len(),
                "Class body length mismatch"
            );
            for (a, e) in actual_body.iter().zip(expected_body.iter()) {
                assert_stmt_eq_inner(a, e);
            }
        }

        (
            StatementKind::ContextManager {
                expr: actual_expr,
                variable: actual_variable,
                block: actual_block,
            },
            StatementKind::ContextManager {
                expr: expected_expr,
                variable: expected_variable,
                block: expected_block,
            },
        ) => {
            assert_eq!(actual_expr, expected_expr, "Context manager expr mismatch");
            assert_eq!(
                actual_variable, expected_variable,
                "Context manager variable mismatch"
            );

            assert_eq!(
                actual_block.len(),
                expected_block.len(),
                "Context manager block length mismatch"
            );
            for (a, e) in actual_block.iter().zip(expected_block.iter()) {
                assert_stmt_eq_inner(a, e);
            }
        }

        (
            StatementKind::TryExcept {
                try_block: actual_try_block,
                handlers: actual_handlers,
                else_block: actual_else_block,
                finally_block: actual_finally_block,
            },
            StatementKind::TryExcept {
                try_block: expected_try_block,
                handlers: expected_handlers,
                else_block: expected_else_block,
                finally_block: expected_finally_block,
            },
        ) => {
            assert_eq!(
                actual_try_block.len(),
                expected_try_block.len(),
                "Try block length mismatch"
            );
            for (a, e) in actual_try_block.iter().zip(expected_try_block.iter()) {
                assert_stmt_eq_inner(a, e);
            }

            assert_eq!(
                actual_handlers.len(),
                expected_handlers.len(),
                "Except handlers length mismatch"
            );
            for (actual_handler, expected_handler) in
                actual_handlers.iter().zip(expected_handlers.iter())
            {
                assert_eq!(
                    actual_handler.kind, expected_handler.kind,
                    "Except handler kind mismatch"
                );
                assert_eq!(
                    actual_handler.block.len(),
                    expected_handler.block.len(),
                    "Except handler block length mismatch"
                );
                for (a, e) in actual_handler
                    .block
                    .iter()
                    .zip(expected_handler.block.iter())
                {
                    assert_stmt_eq_inner(a, e);
                }
            }

            match (actual_else_block, expected_else_block) {
                (Some(actual), Some(expected)) => {
                    assert_eq!(actual.len(), expected.len(), "Else block length mismatch");
                    for (a, e) in actual.iter().zip(expected.iter()) {
                        assert_stmt_eq_inner(a, e);
                    }
                }
                (None, None) => {} // Both bodies are None—nothing to compare
                (None, Some(_)) | (Some(_), None) => {
                    panic!("Else block mismatch: one body is None while the other is Some");
                }
            }

            match (actual_finally_block, expected_finally_block) {
                (Some(actual), Some(expected)) => {
                    assert_eq!(
                        actual.len(),
                        expected.len(),
                        "Finally block length mismatch"
                    );
                    for (a, e) in actual.iter().zip(expected.iter()) {
                        assert_stmt_eq_inner(a, e);
                    }
                }
                (None, None) => {} // Both bodies are None—nothing to compare
                (None, Some(_)) | (Some(_), None) => {
                    panic!("Finally block mismatch: one body is None while the other is Some");
                }
            }
        }

        (
            StatementKind::IfElse {
                if_part: actual_if_part,
                elif_parts: actual_elif_parts,
                else_part: actual_else_part,
            },
            StatementKind::IfElse {
                if_part: expected_if_part,
                elif_parts: expected_elif_parts,
                else_part: expected_else_part,
            },
        ) => {
            assert_cond_ast_eq!(actual_if_part, expected_if_part);

            assert_eq!(
                actual_elif_parts.len(),
                expected_elif_parts.len(),
                "Elif parts length mismatch"
            );
            for (actual_elif_part, expected_elif_part) in
                actual_elif_parts.iter().zip(expected_elif_parts.iter())
            {
                assert_cond_ast_eq!(actual_elif_part, expected_elif_part);
            }

            match (actual_else_part, expected_else_part) {
                (Some(actual), Some(expected)) => {
                    assert_eq!(actual.len(), expected.len(), "Else part length mismatch");
                    for (a, e) in actual.iter().zip(expected.iter()) {
                        assert_stmt_eq_inner(a, e);
                    }
                }
                (None, None) => {} // Both bodies are None—nothing to compare
                (None, Some(_)) | (Some(_), None) => {
                    panic!("Else part mismatch: one body is None while the other is Some");
                }
            }
        }

        (
            StatementKind::WhileLoop(actual_cond_ast),
            StatementKind::WhileLoop(expected_cond_ast),
        ) => {
            assert_cond_ast_eq!(actual_cond_ast, expected_cond_ast);
        }

        (
            StatementKind::ForInLoop {
                index: actual_index,
                iterable: actual_iterable,
                body: actual_body,
                else_block: actual_else_block,
            },
            StatementKind::ForInLoop {
                index: expected_index,
                iterable: expected_iterable,
                body: expected_body,
                else_block: expected_else_block,
            },
        ) => {
            assert_eq!(actual_index, expected_index, "Loop index mismatch");
            assert_eq!(actual_iterable, expected_iterable, "Loop iterable mismatch");

            assert_eq!(
                actual_body.len(),
                expected_body.len(),
                "Loop body length mismatch"
            );
            for (a, e) in actual_body.iter().zip(expected_body.iter()) {
                assert_stmt_eq_inner(a, e);
            }

            match (actual_else_block, expected_else_block) {
                (Some(actual), Some(expected)) => {
                    assert_eq!(actual.len(), expected.len(), "Loop body length mismatch");
                    for (a, e) in actual.iter().zip(expected.iter()) {
                        assert_stmt_eq_inner(a, e);
                    }
                }
                (None, None) => {} // Both bodies are None—nothing to compare
                (None, Some(_)) | (Some(_), None) => {
                    panic!("Loop body mismatch: one body is None while the other is Some");
                }
            }
        }

        // Default case (compare only kinds, ignoring start_line)
        _ => {
            assert_eq!(actual.kind, expected.kind, "AST nodes do not match");
        }
    }
}

pub(crate) use assert_ast_eq;
pub(crate) use assert_stmt_eq;
