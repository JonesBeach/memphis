use std::fmt::{Display, Error, Formatter};

use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Constant, Opcode},
        find_index,
        indices::{ConstantIndex, Index, LocalIndex, NonlocalIndex},
        CompilerResult,
    },
    core::{log, LogLevel},
    domain::{Context, Source},
    parser::types::{
        Ast, BinOp, CallArgs, ConditionalAst, Expr, Params, Statement, StatementKind, UnaryOp,
    },
};

type UnsignedOffset = usize;
type SignedOffset = isize;

#[derive(Clone, PartialEq, Debug)]
pub enum CompilerError {
    SyntaxError(String),
    Internal(String),
}

impl Display for CompilerError {
    fn fmt(&self, _f: &mut Formatter) -> Result<(), Error> {
        unimplemented!("Unsupported error type in bytecode VM")
    }
}

/// A Python bytecode compiler.
pub struct Compiler {
    /// This will likely need to become a stack once we support module imports.
    source: Source,

    /// Keep a reference to the code object being constructed so we can associate things with it,
    /// (variable names, constants, etc.).
    code_stack: Vec<CodeObject>,

    /// Whether we are in a Local or Global context.
    context_stack: Vec<Context>,

    /// The most recent line number seen from the Ast.
    line_number: usize,
}

impl Compiler {
    pub fn new(source: Source) -> Self {
        let code = CodeObject::new_root(source.clone());

        Self {
            source,
            code_stack: vec![code],
            context_stack: vec![Context::Global],
            line_number: 0,
        }
    }

    /// Compile the provided `Ast` and return a `CodeObject` which can be executed. This is not
    /// destructive, meaning multiple calls will build upon the same `CodeObject`.
    pub fn compile(&mut self, ast: &Ast) -> CompilerResult<CodeObject> {
        self.compile_ast(ast)?;
        self.finalize()
    }

    fn compile_ast(&mut self, ast: &Ast) -> CompilerResult<()> {
        ast.iter().try_fold((), |_, stmt| self.compile_stmt(stmt))
    }

    fn finalize(&self) -> CompilerResult<CodeObject> {
        let mut code = self.ensure_code_object()?.clone();
        code.bytecode.push(Opcode::Halt);
        Ok(code)
    }

    fn current_offset(&self) -> CompilerResult<UnsignedOffset> {
        let code = self.ensure_code_object()?;
        Ok(code.bytecode.len())
    }

    fn forward_offset_to(&self, to: UnsignedOffset) -> CompilerResult<SignedOffset> {
        Ok(self.current_offset()? as isize - to as isize - 1)
    }

    // We must mark these as isize because we are doing subtraction with potential negative values
    fn backward_offset_from(&self, from: UnsignedOffset) -> CompilerResult<SignedOffset> {
        Ok(from as isize - self.current_offset()? as isize - 1)
    }

    fn emit(&mut self, opcode: Opcode) -> CompilerResult<()> {
        let line_number = self.line_number;
        let offset = self.current_offset()?;

        let code = self.ensure_code_object_mut()?;
        code.bytecode.push(opcode);
        code.line_map.push((offset, line_number));
        Ok(())
    }

    fn emit_at(&mut self, offset: UnsignedOffset, opcode: Opcode) -> CompilerResult<()> {
        let code = self.ensure_code_object_mut()?;
        code.bytecode[offset] = opcode;
        Ok(())
    }

    /// Emit a `Placeholder` op and return its `UnsignedOffset`. This will later need to be updated
    /// using `emit_at` once the final jump target is known.
    fn emit_placeholder(&mut self) -> CompilerResult<UnsignedOffset> {
        let placeholder = self.current_offset()?;
        self.emit(Opcode::Placeholder)?;
        Ok(placeholder)
    }

    pub fn compile_stmt(&mut self, stmt: &Statement) -> CompilerResult<()> {
        self.line_number = stmt.start_line;

        match &stmt.kind {
            StatementKind::Pass => {}
            StatementKind::Expression(expr) => self.compile_expr(expr)?,
            StatementKind::Return(expr) => self.compile_return(expr)?,
            StatementKind::Assignment { left, right } => self.compile_assignment(left, right)?,
            StatementKind::WhileLoop(cond_ast) => self.compile_while_loop(cond_ast)?,
            StatementKind::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => self.compile_if_else(if_part, elif_parts, else_part)?,
            StatementKind::FunctionDef {
                name,
                args,
                body,
                decorators,
                is_async,
            } => self.compile_function_definition(name, args, body, decorators, is_async)?,
            StatementKind::ClassDef {
                name,
                parents,
                metaclass,
                body,
            } => self.compile_class_definition(name, parents, metaclass, body)?,
            _ => unimplemented!("Statement type {:?} not implemented for bytecode VM", stmt),
        };

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> CompilerResult<()> {
        match expr {
            Expr::None => self.compile_none(),
            Expr::Boolean(value) => self.compile_bool(*value),
            Expr::Integer(value) => self.emit(Opcode::PushInt(*value)),
            Expr::Float(i) => self.emit(Opcode::PushFloat(*i)),
            Expr::StringLiteral(value) => self.compile_string_literal(value),
            Expr::Variable(name) => self.compile_load(name),
            Expr::UnaryOperation { op, right } => self.compile_unary_operation(op, right),
            Expr::BinaryOperation { left, op, right } => {
                self.compile_binary_operation(left, op, right)
            }
            Expr::MemberAccess { object, field } => self.compile_member_access(object, field),
            Expr::FunctionCall { name, args, callee } => {
                self.compile_function_call(name, args, callee)
            }
            Expr::MethodCall { object, name, args } => self.compile_method_call(object, name, args),
            _ => unimplemented!("Expression type {:?} not implemented for bytecode VM", expr),
        }
    }

    fn generate_load(&mut self, name: &str) -> CompilerResult<Opcode> {
        let opcode = match self.ensure_context()? {
            Context::Global => Opcode::LoadGlobal(self.get_or_set_nonlocal_index(name)?),
            Context::Local => {
                // Check locals first
                if let Some(index) = self.get_local_index(name)? {
                    Opcode::LoadFast(index)
                } else {
                    // If not found locally, fall back to globals
                    Opcode::LoadGlobal(self.get_or_set_nonlocal_index(name)?)
                }
            }
        };
        Ok(opcode)
    }

    fn generate_store(&mut self, name: &str) -> CompilerResult<Opcode> {
        let opcode = match self.ensure_context()? {
            Context::Global => Opcode::StoreGlobal(self.get_or_set_nonlocal_index(name)?),
            Context::Local => Opcode::StoreFast(self.get_or_set_local_index(name)?),
        };
        Ok(opcode)
    }

    fn compile_return(&mut self, expr: &[Expr]) -> CompilerResult<()> {
        if expr.len() > 1 {
            unimplemented!("Multiple return values not yet supported in the bytecode VM.")
        }

        self.compile_expr(&expr[0])?;
        self.emit(Opcode::ReturnValue)?;
        Ok(())
    }

    fn compile_assignment(&mut self, left: &Expr, right: &Expr) -> CompilerResult<()> {
        match left {
            Expr::Variable(name) => {
                self.compile_expr(right)?;
                self.compile_store(name)?;
            }
            Expr::MemberAccess { object, field } => {
                // Push the object onto the stack
                self.compile_expr(object)?;
                // Push the value to be assigned onto the stack
                self.compile_expr(right)?;
                let attr_index = self.get_or_set_nonlocal_index(field)?;
                self.emit(Opcode::SetAttr(attr_index))?;
            }
            Expr::IndexAccess { .. } => {
                unimplemented!("Index access assignment not yet supported in bytecode VM.");
            }
            _ => {
                return Err(CompilerError::SyntaxError(
                    "cannot assign to that expression type here.".into(),
                ))
            }
        };

        Ok(())
    }

    /// Compiles a condition and block, returning the offset of the placeholder
    /// that should later be patched with a `JumpIfFalse`.
    fn compile_conditional_branch(
        &mut self,
        ast: &ConditionalAst,
    ) -> CompilerResult<UnsignedOffset> {
        self.compile_expr(&ast.condition)?;
        let placeholder = self.emit_placeholder()?;
        self.compile_ast(&ast.ast)?;
        Ok(placeholder)
    }

    fn compile_while_loop(&mut self, cond_ast: &ConditionalAst) -> CompilerResult<()> {
        let condition_start = self.current_offset()?;
        let post_condition_ph = self.compile_conditional_branch(cond_ast)?;

        // Unconditional jump back to the start of the condition
        let offset = self.backward_offset_from(condition_start)?;
        self.emit(Opcode::Jump(offset))?;

        // Update the JUMP_IF_FALSE offset now that we know the length of the loop body
        let offset = self.forward_offset_to(post_condition_ph)?;
        self.emit_at(post_condition_ph, Opcode::JumpIfFalse(offset))?;

        Ok(())
    }

    fn compile_if_else(
        &mut self,
        if_part: &ConditionalAst,
        elif_parts: &[ConditionalAst],
        else_part: &Option<Ast>,
    ) -> CompilerResult<()> {
        // This will collect placeholders for unconditional jumps at the end of each true branch
        let mut end_jump_placeholders = vec![];

        let mut post_condition_ph = self.compile_conditional_branch(if_part)?;

        if !elif_parts.is_empty() {
            // Jump over any elifs/else if the condition was true
            end_jump_placeholders.push(self.emit_placeholder()?);
        }

        // Compile each `elif`
        for (i, elif) in elif_parts.iter().enumerate() {
            // Patch the previous jump-if-false
            let offset = self.forward_offset_to(post_condition_ph)?;
            self.emit_at(post_condition_ph, Opcode::JumpIfFalse(offset))?;

            // Compile this elif condition and block
            let post_elif_condition_ph = self.compile_conditional_branch(elif)?;

            // Only emit a jump if this is not the last elif or else
            if i != elif_parts.len() - 1 || else_part.is_some() {
                end_jump_placeholders.push(self.emit_placeholder()?);
            }

            // Update for next loop iteration
            post_condition_ph = post_elif_condition_ph;
        }

        let offset = self.forward_offset_to(post_condition_ph)?;
        self.emit_at(post_condition_ph, Opcode::JumpIfFalse(offset))?;

        // Handle optional `else`
        if let Some(else_part) = else_part {
            self.compile_ast(else_part)?;
        }

        // Patch all end-of-true-branch jumps
        for placeholder in end_jump_placeholders {
            let offset = self.forward_offset_to(placeholder)?;
            self.emit_at(placeholder, Opcode::Jump(offset))?;
        }

        Ok(())
    }

    fn compile_function_definition(
        &mut self,
        name: &str,
        args: &Params,
        body: &Ast,
        decorators: &[Expr],
        is_async: &bool,
    ) -> CompilerResult<()> {
        if !decorators.is_empty() || *is_async {
            unimplemented!(
                "Decorators and async functions are not yet supported in the bytecode VM."
            )
        }

        let varnames = args
            .args
            .iter()
            .map(|p| p.arg.clone())
            .collect::<Vec<String>>();
        let code_object =
            CodeObject::with_args(Some(name.to_string()), &varnames, self.source.clone());

        self.context_stack.push(Context::Local);
        self.code_stack.push(code_object);
        self.compile_ast(body)?;
        let code = self
            .code_stack
            .pop()
            .ok_or_else(|| internal_error("Code stack underflow."))?;
        self.context_stack.pop();

        self.compile_code(code)?;
        self.emit(Opcode::MakeFunction)?;
        self.compile_store(name)?;
        Ok(())
    }

    fn compile_class_definition(
        &mut self,
        name: &str,
        parents: &[Expr],
        metaclass: &Option<String>,
        body: &Ast,
    ) -> CompilerResult<()> {
        if !parents.is_empty() {
            unimplemented!("Inheritance not yet supported in the bytecode VM.")
        }
        if metaclass.is_some() {
            unimplemented!("Metaclasses are not yet supported in the bytecode VM.")
        }

        let code_object = CodeObject::new(name, self.source.clone());

        self.context_stack.push(Context::Local);
        self.code_stack.push(code_object);

        self.compile_ast(body)?;
        self.emit(Opcode::EndClass)?;

        let code = self
            .code_stack
            .pop()
            .ok_or_else(|| internal_error("Code stack underflow."))?;
        self.context_stack.pop();

        self.emit(Opcode::LoadBuildClass)?;
        self.compile_code(code)?;

        // subtract one to ignore Opcode::LoadBuildClass
        let num_args = 1;
        self.emit(Opcode::Call(num_args))?;

        self.compile_store(name)?;
        Ok(())
    }

    fn compile_string_literal(&mut self, value: &str) -> CompilerResult<()> {
        self.compile_constant(Constant::String(value.to_string()))
    }

    fn compile_none(&mut self) -> CompilerResult<()> {
        self.compile_constant(Constant::None)
    }

    fn compile_bool(&mut self, bool: bool) -> CompilerResult<()> {
        self.compile_constant(Constant::Boolean(bool))
    }

    fn compile_code(&mut self, code: CodeObject) -> CompilerResult<()> {
        self.compile_constant(Constant::Code(code))
    }

    fn compile_constant(&mut self, constant: Constant) -> CompilerResult<()> {
        let index = self.get_or_set_constant_index(constant)?;
        self.emit(Opcode::LoadConst(index))?;
        Ok(())
    }

    fn compile_load(&mut self, name: &str) -> CompilerResult<()> {
        let load = self.generate_load(name)?;
        self.emit(load)
    }

    fn compile_store(&mut self, name: &str) -> CompilerResult<()> {
        let store = self.generate_store(name)?;
        self.emit(store)
    }

    fn compile_unary_operation(&mut self, op: &UnaryOp, right: &Expr) -> CompilerResult<()> {
        self.compile_expr(right)?;

        let opcode = match op {
            UnaryOp::Minus => Some(Opcode::UnaryNegative),
            // this acts as a no-op. can be overridden with __pos__ for custom classes
            UnaryOp::Plus => None,
            UnaryOp::Not => Some(Opcode::UnaryNot),
            UnaryOp::BitwiseNot => Some(Opcode::UnaryInvert),
            _ => unimplemented!(
                "{}",
                format!(
                    "Binary operation '{:?}' not yet supported in the bytecode VM.",
                    op
                )
            ),
        };
        if let Some(opcode) = opcode {
            self.emit(opcode)?;
        }
        Ok(())
    }

    fn compile_binary_operation(
        &mut self,
        left: &Expr,
        op: &BinOp,
        right: &Expr,
    ) -> CompilerResult<()> {
        self.compile_expr(left)?;
        self.compile_expr(right)?;

        let opcode = match op {
            BinOp::Add => Opcode::Add,
            BinOp::Sub => Opcode::Sub,
            BinOp::Mul => Opcode::Mul,
            BinOp::Div => Opcode::Div,
            BinOp::Equals => Opcode::Eq,
            BinOp::LessThan => Opcode::LessThan,
            BinOp::GreaterThan => Opcode::GreaterThan,
            _ => unimplemented!(
                "{}",
                format!(
                    "Binary operation '{:?}' not yet supported in the bytecode VM.",
                    op
                )
            ),
        };

        self.emit(opcode)?;
        Ok(())
    }

    fn compile_function_call(
        &mut self,
        name: &str,
        args: &CallArgs,
        callee: &Option<Box<Expr>>,
    ) -> CompilerResult<()> {
        if callee.is_some() {
            unimplemented!("Callees for function calls not yet supported in the bytecode VM.")
        }

        if name == "print" {
            if args.args[0].as_string().is_none() {
                unimplemented!("Non-string args not yet supported for print in the bytecode VM.")
            }
            if args.args.len() > 1 {
                unimplemented!("More than 1 arg not yet supported for print in the bytecode VM.")
            }
            let index = self
                .get_or_set_constant_index(Constant::String(args.args[0].as_string().unwrap()))?;
            self.emit(Opcode::PrintConst(index))?;
            return Ok(());
        }

        self.compile_load(name)?;

        // We push the args onto the stack in reverse call order so that we will pop
        // them off in call order.
        for arg in args.args.iter().rev() {
            self.compile_expr(arg)?;
        }

        self.emit(Opcode::Call(args.args.len()))?;
        Ok(())
    }

    fn compile_method_call(
        &mut self,
        object: &Expr,
        name: &str,
        args: &CallArgs,
    ) -> CompilerResult<()> {
        if !args.kwargs.is_empty() {
            unimplemented!(
                "Method calls with kwargs not yet supported for print in the bytecode VM."
            )
        }

        self.compile_member_access(object, name)?;
        for arg in args.args.iter().rev() {
            self.compile_expr(arg)?;
        }
        self.emit(Opcode::CallMethod(args.args.len()))?;
        Ok(())
    }

    fn compile_member_access(&mut self, object: &Expr, field: &str) -> CompilerResult<()> {
        self.compile_expr(object)?;
        let attr_index = self.get_or_set_nonlocal_index(field)?;
        self.emit(Opcode::LoadAttr(attr_index))?;
        Ok(())
    }

    fn get_or_set_local_index(&mut self, name: &str) -> CompilerResult<LocalIndex> {
        log(LogLevel::Debug, || {
            format!("Looking for '{}' in locals", name)
        });
        if let Some(index) = self.get_local_index(name)? {
            Ok(index)
        } else {
            let code = self.ensure_code_object_mut()?;
            let new_index = code.varnames.len();
            code.varnames.push(name.to_string());
            Ok(Index::new(new_index))
        }
    }

    fn get_local_index(&self, name: &str) -> CompilerResult<Option<LocalIndex>> {
        let code = self.ensure_code_object()?;
        Ok(find_index(&code.varnames, name).map(Index::new))
    }

    fn get_or_set_nonlocal_index(&mut self, name: &str) -> CompilerResult<NonlocalIndex> {
        log(LogLevel::Debug, || {
            format!("Looking for '{}' in globals", name)
        });
        let code = self.ensure_code_object_mut()?;
        let index = if let Some(index) = find_index(&code.names, name) {
            index
        } else {
            let new_index = code.names.len();
            code.names.push(name.to_string());
            new_index
        };
        Ok(Index::new(index))
    }

    fn get_or_set_constant_index(&mut self, value: Constant) -> CompilerResult<ConstantIndex> {
        log(LogLevel::Debug, || {
            format!("Looking for '{}' in constants", value)
        });
        let code = self.ensure_code_object_mut()?;
        let index = if let Some(index) = find_index(&code.constants, &value) {
            index
        } else {
            let next_index = code.constants.len();
            code.constants.push(value);
            next_index
        };
        Ok(Index::new(index))
    }

    /// This assumes we always have a context stack.
    fn ensure_context(&self) -> CompilerResult<&Context> {
        self.context_stack
            .last()
            .ok_or_else(|| internal_error("Failed to find current context."))
    }

    fn ensure_code_object_mut(&mut self) -> CompilerResult<&mut CodeObject> {
        self.code_stack
            .last_mut()
            .ok_or_else(|| internal_error("Failed to find current code object."))
    }

    fn ensure_code_object(&self) -> CompilerResult<&CodeObject> {
        self.code_stack
            .last()
            .ok_or_else(|| internal_error("Failed to find current code object."))
    }
}

fn internal_error(msg: &str) -> CompilerError {
    CompilerError::Internal(msg.to_string())
}

#[cfg(test)]
mod tests_bytecode {
    use super::*;

    use crate::{
        bytecode_vm::compiler::{test_utils::*, Bytecode},
        parser::{test_utils::*, types::ast},
    };

    impl Compiler {
        pub fn bytecode(&self) -> Bytecode {
            let code = self
                .ensure_code_object()
                .expect("Failed to fetch code object");
            code.bytecode.clone()
        }
    }

    #[test]
    fn expression() {
        let expr = bin_op!(int!(4), Mul, bin_op!(int!(2), Add, int!(3)));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::PushInt(4),
                Opcode::PushInt(2),
                Opcode::PushInt(3),
                Opcode::Add,
                Opcode::Mul,
            ]
        );
    }

    #[test]
    fn binary_operations() {
        let expr = bin_op!(int!(4), Add, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::PushInt(4), Opcode::PushFloat(5.1), Opcode::Add,]
        );

        let expr = bin_op!(int!(4), Sub, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::PushInt(4), Opcode::PushFloat(5.1), Opcode::Sub,]
        );

        let expr = bin_op!(int!(4), Mul, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::PushInt(4), Opcode::PushFloat(5.1), Opcode::Mul,]
        );

        let expr = bin_op!(int!(4), Div, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::PushInt(4), Opcode::PushFloat(5.1), Opcode::Div,]
        );

        let expr = bin_op!(int!(4), Equals, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::PushInt(4), Opcode::PushFloat(5.1), Opcode::Eq,]
        );

        let expr = bin_op!(int!(4), LessThan, int!(5));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::PushInt(4), Opcode::PushInt(5), Opcode::LessThan,]
        );

        let expr = bin_op!(int!(4), GreaterThan, int!(5));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::PushInt(4), Opcode::PushInt(5), Opcode::GreaterThan,]
        );
    }

    #[test]
    fn unary_operations() {
        let expr = unary_op!(Minus, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(bytecode, &[Opcode::PushInt(4), Opcode::UnaryNegative]);

        let expr = unary_op!(Plus, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(bytecode, &[Opcode::PushInt(4)]);

        let expr = unary_op!(Not, Expr::Boolean(false));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadConst(Index::new(0)), Opcode::UnaryNot]
        );

        let expr = unary_op!(BitwiseNot, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(bytecode, &[Opcode::PushInt(4), Opcode::UnaryInvert]);
    }

    #[test]
    fn assignment() {
        let s = stmt_assign!(var!("var"), bin_op!(int!(5), Sub, int!(2)));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::PushInt(5),
                Opcode::PushInt(2),
                Opcode::Sub,
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );

        let s = stmt_assign!(var!("var"), str!("Hello World"));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );

        let s = stmt_assign!(var!("var"), Expr::None);
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );

        let expr = bin_op!(int!(2), Add, var!("a"));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::PushInt(2),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Add,
            ]
        );
    }

    #[test]
    fn member_access() {
        let s = stmt_assign!(member_access!(var!("foo"), "x"), int!(4));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::PushInt(4),
                Opcode::SetAttr(Index::new(1))
            ]
        );

        let expr = member_access!(var!("foo"), "x");
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1))
            ]
        );
    }

    #[test]
    fn while_loop() {
        let s = stmt!(StatementKind::WhileLoop(ConditionalAst {
            condition: bin_op!(int!(4), LessThan, int!(5)),
            ast: ast![],
        }));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::PushInt(4),
                Opcode::PushInt(5),
                Opcode::LessThan,
                Opcode::JumpIfFalse(1),
                Opcode::Jump(-5),
            ]
        );
    }

    #[test]
    fn if_else_only_if() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: bin_op!(int!(4), LessThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![],
            else_part: None,
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::PushInt(4),
                Opcode::PushInt(5),
                Opcode::LessThan,
                Opcode::JumpIfFalse(2),
                Opcode::PushInt(-1),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn if_else_with_else() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: bin_op!(int!(4), LessThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-3))],
            },
            elif_parts: vec![],
            else_part: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::PushInt(4),
                Opcode::PushInt(5),
                Opcode::LessThan,
                Opcode::JumpIfFalse(2),
                Opcode::PushInt(-3),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::PushInt(3),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn if_else_with_elif() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: bin_op!(int!(4), GreaterThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![ConditionalAst {
                condition: bin_op!(int!(4), GreaterThan, int!(4)),
                ast: ast![stmt_assign!(var!("a"), int!(-2))],
            }],
            else_part: None,
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                // if: 0-2
                Opcode::PushInt(4),
                Opcode::PushInt(5),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(3), // jump to elif condition
                Opcode::PushInt(-1),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(6), // skip rest
                // elif: 7-9
                Opcode::PushInt(4),
                Opcode::PushInt(4),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(2), // jump past elif block if false
                Opcode::PushInt(-2),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn if_else_with_elif_and_else() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: bin_op!(int!(4), GreaterThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![ConditionalAst {
                condition: bin_op!(int!(4), GreaterThan, int!(4)),
                ast: ast![stmt_assign!(var!("a"), int!(-2))],
            }],
            else_part: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                // if: 0-2
                Opcode::PushInt(4),
                Opcode::PushInt(5),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(3), // jump to elif condition
                Opcode::PushInt(-1),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(9), // skip rest
                // elif: 7-9
                Opcode::PushInt(4),
                Opcode::PushInt(4),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(3), // jump past elif block if false
                Opcode::PushInt(-2),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(2), // skip else
                Opcode::PushInt(3),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn function_call() {
        let expr = func_call!("foo", call_args![var!("a"), var!("b")]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::LoadGlobal(Index::new(2)),
                Opcode::Call(2),
            ]
        );
    }

    #[test]
    fn method_call() {
        let expr = method_call!(var!("foo"), "bar", call_args![int!(88), int!(99)]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::PushInt(99),
                Opcode::PushInt(88),
                Opcode::CallMethod(2),
            ]
        );
    }
}

#[cfg(test)]
mod tests_compiler {
    use crate::bytecode_vm::VmContext;

    use super::*;

    fn compile(text: &str) -> CodeObject {
        VmContext::new(Source::from_text(text))
            .compile()
            .expect("Failed to compile test program!")
    }

    macro_rules! assert_code_eq {
        ($actual:expr, $expected:expr) => {
            assert_code_eq(&$actual, &$expected)
        };
    }

    macro_rules! compile_incremental {
        ( $( $line:expr ),* ) => {{
            let mut context = VmContext::default();
            $(
                context.add_line($line);
            )*
            context.compile().expect("Failed to compile")
        }};
    }

    /// This is designed to confirm everything in a CodeObject matches besides the Source and
    /// the line number mappings.
    fn assert_code_eq(actual: &CodeObject, expected: &CodeObject) {
        assert_eq!(actual.name, expected.name, "Code object names do not match");
        assert_eq!(
            actual.bytecode, expected.bytecode,
            "Code object bytecode does not match"
        );
        assert_eq!(
            actual.arg_count, expected.arg_count,
            "Code object arg_count does not match"
        );
        assert_eq!(
            actual.varnames, expected.varnames,
            "Code object varnames do not match"
        );
        assert_eq!(
            actual.names, expected.names,
            "Code object names do not match"
        );

        assert_eq!(
            actual.constants.len(),
            expected.constants.len(),
            "Code object constants do not match"
        );

        for (i, (a_const, e_const)) in actual
            .constants
            .iter()
            .zip(expected.constants.iter())
            .enumerate()
        {
            match (a_const, e_const) {
                (Constant::Code(a_code), Constant::Code(e_code)) => {
                    assert_code_eq!(a_code, e_code);
                }
                _ => {
                    assert_eq!(
                        a_const, e_const,
                        "Code object constant at index {} does not match",
                        i
                    );
                }
            }
        }
    }

    #[test]
    fn function_definition_with_parameters() {
        let text = r#"
def foo(a, b):
    a + b
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            name: Some("foo".into()),
            bytecode: vec![
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::Add,
            ],
            arg_count: 2,
            varnames: vec!["a".into(), "b".into()],
            names: vec![],
            constants: vec![],
            source: Source::default(),
            line_map: vec![],
        };

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["foo".into()],
            constants: vec![Constant::Code(fn_foo)],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_nested_function() {
        let text = r#"
def foo(a, b):
    def inner():
        return 10
    a + b
"#;
        let code = compile(text);

        let fn_inner = CodeObject {
            name: Some("inner".into()),
            bytecode: vec![Opcode::PushInt(10), Opcode::ReturnValue],
            arg_count: 0,
            varnames: vec![],
            names: vec![],
            constants: vec![],
            source: Source::default(),
            line_map: vec![],
        };

        let fn_foo = CodeObject {
            name: Some("foo".into()),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(2)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::Add,
            ],
            arg_count: 2,
            varnames: vec!["a".into(), "b".into(), "inner".into()],
            names: vec![],
            constants: vec![Constant::Code(fn_inner)],
            source: Source::default(),
            line_map: vec![],
        };

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["foo".to_string()],
            constants: vec![Constant::Code(fn_foo)],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_local_var() {
        let text = r#"
def foo():
    c = 10
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            name: Some("foo".into()),
            bytecode: vec![Opcode::PushInt(10), Opcode::StoreFast(Index::new(0))],
            arg_count: 0,
            varnames: vec!["c".into()],
            names: vec![],
            constants: vec![],
            source: Source::default(),
            line_map: vec![],
        };

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["foo".into()],
            constants: vec![Constant::Code(fn_foo)],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_local_var_and_return() {
        let text = r#"
def foo():
    c = 10
    return c
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            name: Some("foo".into()),
            bytecode: vec![
                Opcode::PushInt(10),
                Opcode::StoreFast(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::ReturnValue,
            ],
            arg_count: 0,
            varnames: vec!["c".into()],
            names: vec![],
            constants: vec![],
            source: Source::default(),
            line_map: vec![],
        };

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["foo".into()],
            constants: vec![Constant::Code(fn_foo)],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_two_calls_and_no_return() {
        let text = r#"
def hello():
    print("Hello")

def world():
    print("World")

hello()
world()
"#;
        let code = compile(text);

        let fn_hello = CodeObject {
            name: Some("hello".into()),
            bytecode: vec![Opcode::PrintConst(Index::new(0))],
            arg_count: 0,
            varnames: vec![],
            names: vec![],
            constants: vec![Constant::String("Hello".into())],
            source: Source::default(),
            line_map: vec![],
        };

        let fn_world = CodeObject {
            name: Some("world".into()),
            bytecode: vec![Opcode::PrintConst(Index::new(0))],
            arg_count: 0,
            varnames: vec![],
            names: vec![],
            constants: vec![Constant::String("World".into())],
            source: Source::default(),
            line_map: vec![],
        };

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::Call(0),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["hello".into(), "world".into()],
            constants: vec![Constant::Code(fn_hello), Constant::Code(fn_world)],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_definition() {
        let text = r#"
class Foo:
    def bar(self):
        return 99
"#;
        let code = compile(text);

        let fn_bar = CodeObject {
            name: Some("bar".into()),
            bytecode: vec![Opcode::PushInt(99), Opcode::ReturnValue],
            arg_count: 1,
            varnames: vec!["self".into()],
            names: vec![],
            constants: vec![],
            source: Source::default(),
            line_map: vec![],
        };

        let cls_foo = CodeObject {
            name: Some("Foo".into()),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
                Opcode::EndClass,
            ],
            arg_count: 0,
            varnames: vec!["bar".into()],
            names: vec![],
            constants: vec![Constant::Code(fn_bar)],
            source: Source::default(),
            line_map: vec![],
        };

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadBuildClass,
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["Foo".into()],
            constants: vec![Constant::Code(cls_foo)],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_definition_member_access() {
        let text = r#"
class Foo:
    def bar(self):
        return self.val
"#;
        let code = compile(text);

        let fn_bar = CodeObject {
            name: Some("bar".into()),
            bytecode: vec![
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadAttr(Index::new(0)),
                Opcode::ReturnValue,
            ],
            arg_count: 1,
            varnames: vec!["self".into()],
            names: vec!["val".into()],
            constants: vec![],
            source: Source::default(),
            line_map: vec![],
        };

        let cls_foo = CodeObject {
            name: Some("Foo".to_string()),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
                Opcode::EndClass,
            ],
            arg_count: 0,
            varnames: vec!["bar".into()],
            names: vec![],
            constants: vec![Constant::Code(fn_bar)],
            source: Source::default(),
            line_map: vec![],
        };

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadBuildClass,
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["Foo".to_string()],
            constants: vec![Constant::Code(cls_foo)],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
f = Foo()
"#;
        let code = compile(text);

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["Foo".into(), "f".into()],
            constants: vec![],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_instantiation_and_method_call() {
        let text = r#"
b = f.bar()
"#;
        let code = compile(text);

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::CallMethod(0),
                Opcode::StoreGlobal(Index::new(2)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["f".into(), "bar".into(), "b".into()],
            constants: vec![],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn incremental_compilation() {
        let first = r#"
def foo():
    return 10
"#;
        let second = r#"
a = foo()
"#;
        let code = compile_incremental![first, second];

        let fn_foo = CodeObject {
            name: Some("foo".into()),
            bytecode: vec![Opcode::PushInt(10), Opcode::ReturnValue],
            arg_count: 0,
            varnames: vec![],
            names: vec![],
            constants: vec![],
            source: Source::default(),
            line_map: vec![],
        };

        let expected = CodeObject {
            name: None,
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            names: vec!["foo".into(), "a".into()],
            constants: vec![Constant::Code(fn_foo)],
            source: Source::default(),
            line_map: vec![],
        };

        assert_code_eq!(code, expected);
    }
}
