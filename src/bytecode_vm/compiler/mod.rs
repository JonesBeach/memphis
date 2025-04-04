pub mod types;

use crate::{
    bytecode_vm::{types::CompilerError, Opcode},
    core::{log, LogLevel},
    domain::{Context, Source},
    parser::types::{
        Ast, BinOp, CallArgs, ConditionalBlock, Expr, Params, Statement, StatementKind, UnaryOp,
    },
};

use self::types::{CodeObject, CompiledProgram, Constant};

use super::indices::{ConstantIndex, Index, LocalIndex, NonlocalIndex};

type CompilerResult<T> = Result<T, CompilerError>;

pub struct Compiler {
    source: Source,

    /// Constants discovered during compilation. These will be compiled into the
    /// [`CompiledProgram`] which is handed off to the VM.
    constant_pool: Vec<Constant>,

    /// Keep a reference to the code object being constructed so we can associate things with it,
    /// (i.e. variable names).
    code_stack: Vec<CodeObject>,

    context_stack: Vec<Context>,

    line_number: usize,
}

impl Compiler {
    pub fn new(source: Source) -> Self {
        let code = CodeObject::new_root(source.clone());

        Self {
            source,
            constant_pool: vec![],
            code_stack: vec![code],
            context_stack: vec![Context::Global],
            line_number: 0,
        }
    }

    // We previously used this when we compiled the entire AST at once, before we switched to an
    // AST-walking approach to make line numbers work. We may bring this back.
    pub fn compile(&mut self, ast: &Ast) -> CompilerResult<CompiledProgram> {
        self.compile_ast(ast)?;
        self.emit(Opcode::Halt);

        let code = self.code_stack.pop().ok_or(CompilerError::StackUnderflow)?;
        Ok(CompiledProgram::new(code, self.constant_pool.clone()))
    }

    fn current_offset(&self) -> usize {
        let code = self.ensure_code_object();
        code.bytecode.len()
    }

    fn emit(&mut self, opcode: Opcode) {
        let line_number = self.line_number;
        let offset = self.current_offset();

        let code = self.ensure_code_object_mut();
        code.bytecode.push(opcode);
        code.line_map.push((offset, line_number));
    }

    fn emit_at(&mut self, offset: usize, opcode: Opcode) {
        let code = self.ensure_code_object_mut();
        code.bytecode[offset] = opcode;
    }

    fn compile_stmt(&mut self, stmt: &Statement) -> CompilerResult<()> {
        self.line_number = stmt.start_line;

        match &stmt.kind {
            StatementKind::Pass => {}
            StatementKind::Expression(expr) => self.compile_expr(expr)?,
            StatementKind::Return(expr) => self.compile_return(expr)?,
            StatementKind::Assignment { left, right } => self.compile_assignment(left, right)?,
            StatementKind::WhileLoop { condition, body } => {
                self.compile_while_loop(condition, body)?
            }
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
            Expr::None => {
                self.compile_none();
            }
            Expr::Boolean(value) => {
                self.compile_bool(*value);
            }
            Expr::Integer(value) => {
                self.emit(Opcode::Push(*value));
            }
            Expr::StringLiteral(value) => {
                self.compile_string_literal(value);
            }
            Expr::Variable(name) => self.compile_load(name),
            Expr::UnaryOperation { op, right } => self.compile_unary_operation(op, right)?,
            Expr::BinaryOperation { left, op, right } => {
                self.compile_binary_operation(left, op, right)?
            }
            Expr::MemberAccess { object, field } => self.compile_member_access(object, field)?,
            Expr::FunctionCall { name, args, callee } => {
                self.compile_function_call(name, args, callee)?
            }
            Expr::MethodCall { object, name, args } => {
                self.compile_method_call(object, name, args)?
            }
            _ => unimplemented!("Expression type {:?} not implemented for bytecode VM", expr),
        };

        Ok(())
    }

    fn compile_ast(&mut self, ast: &Ast) -> CompilerResult<()> {
        for stmt in ast.iter() {
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }

    fn generate_load(&mut self, name: &str) -> Opcode {
        match self.ensure_context() {
            Context::Global => Opcode::LoadGlobal(self.get_or_set_nonlocal_index(name)),
            Context::Local => {
                // Check locals first
                if let Some(index) = self.get_local_index(name) {
                    return Opcode::LoadFast(index);
                }

                // If not found locally, fall back to globals
                Opcode::LoadGlobal(self.get_or_set_nonlocal_index(name))
            }
        }
    }

    fn generate_store(&mut self, name: &str) -> Opcode {
        match self.ensure_context() {
            Context::Global => Opcode::StoreGlobal(self.get_or_set_nonlocal_index(name)),
            Context::Local => Opcode::StoreFast(self.get_or_set_local_index(name)),
        }
    }

    fn compile_return(&mut self, expr: &[Expr]) -> CompilerResult<()> {
        if expr.len() > 1 {
            unimplemented!("Multiple return values not yet supported in the bytecode VM.")
        }

        self.compile_expr(&expr[0])?;
        self.emit(Opcode::ReturnValue);
        Ok(())
    }

    fn compile_assignment(&mut self, left: &Expr, right: &Expr) -> CompilerResult<()> {
        match left {
            Expr::Variable(name) => {
                self.compile_expr(right)?;
                self.compile_store(name);
            }
            Expr::MemberAccess { object, field } => {
                // Push the object onto the stack
                self.compile_expr(object)?;
                // Push the value to be assigned onto the stack
                self.compile_expr(right)?;
                let attr_index = self.get_or_set_nonlocal_index(field);
                self.emit(Opcode::SetAttr(attr_index));
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

    fn compile_while_loop(&mut self, condition: &Expr, body: &Ast) -> CompilerResult<()> {
        let condition_start = self.current_offset();
        self.compile_expr(condition)?;

        // Temporary offset, we will change this once we know the length of the loop body
        let jump_if_false_placeholder = self.current_offset();
        self.emit(Opcode::Placeholder);

        self.compile_ast(body)?;

        // Unconditional jump back to the start of the condition
        // We must mark these as isize because we are doing subtraction with potential overflow
        let jump_back_offset = condition_start as isize - self.current_offset() as isize - 1;
        self.emit(Opcode::Jump(jump_back_offset));

        // Update the JUMP_IF_FALSE offset now that we know the length of the loop body
        let jump_if_false_offset =
            self.current_offset() as isize - jump_if_false_placeholder as isize - 1;
        self.emit_at(
            jump_if_false_placeholder,
            Opcode::JumpIfFalse(jump_if_false_offset),
        );

        Ok(())
    }

    fn compile_if_else(
        &mut self,
        if_part: &ConditionalBlock,
        elif_parts: &[ConditionalBlock],
        else_part: &Option<Ast>,
    ) -> CompilerResult<()> {
        if !elif_parts.is_empty() {
            unreachable!("elif not yet supported in the bytecode VM.")
        }

        self.compile_expr(&if_part.condition)?;

        // Temporary offset, we will change this once we know the length of the if condition body
        let jump_if_false_placeholder = self.current_offset();
        self.emit(Opcode::Placeholder);

        self.compile_ast(&if_part.block)?;

        if let Some(else_part) = else_part {
            let jump_else_placeholder = self.current_offset();
            self.emit(Opcode::Placeholder);

            let jump_if_false_offset =
                self.current_offset() as isize - jump_if_false_placeholder as isize - 1;
            self.emit_at(
                jump_if_false_placeholder,
                Opcode::JumpIfFalse(jump_if_false_offset),
            );

            self.compile_ast(else_part)?;

            let jump_else_offset =
                self.current_offset() as isize - jump_else_placeholder as isize - 1;
            self.emit_at(jump_else_placeholder, Opcode::Jump(jump_else_offset));
        } else {
            let jump_if_false_offset =
                self.current_offset() as isize - jump_if_false_placeholder as isize - 1;
            self.emit_at(
                jump_if_false_placeholder,
                Opcode::JumpIfFalse(jump_if_false_offset),
            );
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
        let code = self.code_stack.pop().expect("Internal Compiler Error");
        self.context_stack.pop();

        self.compile_code(code);
        self.emit(Opcode::MakeFunction);
        self.compile_store(name);
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
        self.emit(Opcode::EndClass);

        let code = self.code_stack.pop().expect("Internal Compiler Error");
        self.context_stack.pop();

        self.emit(Opcode::LoadBuildClass);
        self.compile_code(code);

        // subtract one to ignore Opcode::LoadBuildClass
        let num_args = 1;
        self.emit(Opcode::Call(num_args));

        self.compile_store(name);
        Ok(())
    }

    fn compile_string_literal(&mut self, value: &str) {
        self.compile_constant(Constant::String(value.to_string()))
    }

    fn compile_none(&mut self) {
        self.compile_constant(Constant::None)
    }

    fn compile_bool(&mut self, bool: bool) {
        self.compile_constant(Constant::Boolean(bool))
    }

    fn compile_code(&mut self, code: CodeObject) {
        self.compile_constant(Constant::Code(code))
    }

    fn compile_constant(&mut self, constant: Constant) {
        let index = self.get_or_set_constant_index(constant);
        self.emit(Opcode::LoadConst(index))
    }

    fn compile_load(&mut self, name: &str) {
        let load = self.generate_load(name);
        self.emit(load);
    }

    fn compile_store(&mut self, name: &str) {
        let store = self.generate_store(name);
        self.emit(store);
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
            self.emit(opcode);
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
            BinOp::Add => Opcode::Iadd,
            BinOp::Sub => Opcode::Isub,
            BinOp::Mul => Opcode::Imul,
            BinOp::Div => Opcode::Idiv,
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

        self.emit(opcode);
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
            let index =
                self.get_or_set_constant_index(Constant::String(args.args[0].as_string().unwrap()));
            self.emit(Opcode::PrintConst(index));
            return Ok(());
        }

        self.compile_load(name);

        // We push the args onto the stack in reverse call order so that we will pop
        // them off in call order.
        for arg in args.args.iter().rev() {
            self.compile_expr(arg)?;
        }

        self.emit(Opcode::Call(args.args.len()));
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
        self.emit(Opcode::CallMethod(args.args.len()));
        Ok(())
    }

    fn compile_member_access(&mut self, object: &Expr, field: &str) -> CompilerResult<()> {
        self.compile_expr(object)?;
        let attr_index = self.get_or_set_nonlocal_index(field);
        self.emit(Opcode::LoadAttr(attr_index));
        Ok(())
    }

    fn get_or_set_local_index(&mut self, name: &str) -> LocalIndex {
        log(LogLevel::Debug, || {
            format!("Looking for '{}' in locals", name)
        });
        if let Some(index) = self.get_local_index(name) {
            index
        } else {
            let code = self.ensure_code_object_mut();
            let new_index = code.varnames.len();
            code.varnames.push(name.to_string());
            Index::new(new_index)
        }
    }

    fn get_local_index(&self, name: &str) -> Option<LocalIndex> {
        let code = self.ensure_code_object();
        find_index(&code.varnames, name).map(Index::new)
    }

    fn get_or_set_nonlocal_index(&mut self, name: &str) -> NonlocalIndex {
        log(LogLevel::Debug, || {
            format!("Looking for '{}' in globals", name)
        });
        let code = self.ensure_code_object_mut();
        if let Some(index) = find_index(&code.names, name) {
            Index::new(index)
        } else {
            let new_index = code.names.len();
            code.names.push(name.to_string());
            Index::new(new_index)
        }
    }

    fn get_or_set_constant_index(&mut self, value: Constant) -> ConstantIndex {
        log(LogLevel::Debug, || {
            format!("Looking for '{}' in constants", value)
        });
        if let Some(index) = find_index(&self.constant_pool, &value) {
            Index::new(index)
        } else {
            let next_index = self.constant_pool.len();
            self.constant_pool.push(value);
            Index::new(next_index)
        }
    }

    /// This assumes we always have a context stack.
    fn ensure_context(&self) -> &Context {
        self.context_stack
            .last()
            .expect("Internal Compiler Error: failed to find context.")
    }

    fn ensure_code_object_mut(&mut self) -> &mut CodeObject {
        self.code_stack
            .last_mut()
            .expect("Internal Compiler Error: failed to find current code object.")
    }

    fn ensure_code_object(&self) -> &CodeObject {
        self.code_stack
            .last()
            .expect("Internal Compiler Error: failed to find current code object.")
    }
}

pub fn find_index<T, Q>(vec: &[T], query: &Q) -> Option<usize>
where
    T: PartialEq<Q>,
    Q: ?Sized,
{
    vec.iter()
        .enumerate()
        .find_map(|(index, value)| if value == query { Some(index) } else { None })
}

#[cfg(test)]
mod bytecode_tests {
    use super::{types::Bytecode, *};

    use crate::{
        ast, bin_op, call_args, func_call, int, member_access, parser::test_utils::*, stmt_assign,
        str, unary_op, var,
    };

    fn init() -> Compiler {
        Compiler::new(Source::default())
    }

    fn compile_expr(expr: Expr) -> Bytecode {
        compile_stmt(stmt(StatementKind::Expression(expr)))
    }

    fn compile_stmt(stmt: Statement) -> Bytecode {
        let mut compiler = init();
        compiler
            .compile_stmt(&stmt)
            .expect("Failed to compile test Statement!");
        let code = compiler.ensure_code_object();
        code.bytecode.clone()
    }

    #[test]
    fn expression() {
        let expr = bin_op!(int!(4), Mul, bin_op!(int!(2), Add, int!(3)));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::Push(4),
                Opcode::Push(2),
                Opcode::Push(3),
                Opcode::Iadd,
                Opcode::Imul,
            ]
        );
    }

    #[test]
    fn binary_operations() {
        let expr = bin_op!(int!(4), LessThan, int!(5));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::Push(4), Opcode::Push(5), Opcode::LessThan,]
        );

        let expr = bin_op!(int!(4), GreaterThan, int!(5));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::Push(4), Opcode::Push(5), Opcode::GreaterThan,]
        );
    }

    #[test]
    fn unary_operations() {
        let expr = unary_op!(Minus, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(bytecode, &[Opcode::Push(4), Opcode::UnaryNegative]);

        let expr = unary_op!(Plus, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(bytecode, &[Opcode::Push(4)]);

        let expr = unary_op!(Not, Expr::Boolean(false));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadConst(Index::new(0)), Opcode::UnaryNot]
        );

        let expr = unary_op!(BitwiseNot, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(bytecode, &[Opcode::Push(4), Opcode::UnaryInvert]);
    }

    #[test]
    fn assignment() {
        let s = stmt_assign!(var!("var"), bin_op!(int!(5), Sub, int!(2)));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::Push(5),
                Opcode::Push(2),
                Opcode::Isub,
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
                Opcode::Push(2),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Iadd,
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
                Opcode::Push(4),
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
        let s = stmt(StatementKind::WhileLoop {
            condition: bin_op!(int!(4), LessThan, int!(5)),
            body: ast![],
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::Push(4),
                Opcode::Push(5),
                Opcode::LessThan,
                Opcode::JumpIfFalse(1),
                Opcode::Jump(-5),
            ]
        );
    }

    #[test]
    fn if_else() {
        let s = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: bin_op!(int!(4), LessThan, int!(5)),
                block: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![],
            else_part: None,
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::Push(4),
                Opcode::Push(5),
                Opcode::LessThan,
                Opcode::JumpIfFalse(2),
                Opcode::Push(-1),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );

        let s = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: bin_op!(int!(4), LessThan, int!(5)),
                block: ast![stmt_assign!(var!("a"), int!(-3))],
            },
            elif_parts: vec![],
            else_part: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::Push(4),
                Opcode::Push(5),
                Opcode::LessThan,
                Opcode::JumpIfFalse(3),
                Opcode::Push(-3),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(2),
                Opcode::Push(3),
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
        let expr = Expr::MethodCall {
            object: Box::new(var!("foo")),
            name: "bar".to_string(),
            args: call_args![int!(88), int!(99)],
        };
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::Push(99),
                Opcode::Push(88),
                Opcode::CallMethod(2),
            ]
        );
    }
}

#[cfg(test)]
mod compiler_state_tests {
    use super::*;

    use crate::init::MemphisContext;

    fn compile(text: &str) -> CompiledProgram {
        let mut context = MemphisContext::from_text(text);
        context.compile().expect("Failed to compile test program!")
    }

    fn get_names_index(program: &CompiledProgram, name: &str) -> usize {
        find_index(&program.code.names, name).unwrap_or_else(|| panic!("Name '{}' not found", name))
    }

    fn get_varnames_index(code: &CodeObject, name: &str) -> usize {
        find_index(&code.varnames, name).unwrap_or_else(|| panic!("Varname '{}' not found", name))
    }

    fn get_code_at_index(program: &CompiledProgram, index: usize) -> &CodeObject {
        let Some(Constant::Code(code)) = program.constant_pool.get(index) else {
            panic!("Code at index {} not found!", index)
        };
        code
    }

    /// This is designed to confirm everything in a CodeObject matches besides the Source and
    /// the line number mappings.
    macro_rules! assert_code_eq {
        ($actual:expr, $expected:expr) => {
            assert_eq!(
                $actual.name, $expected.name,
                "Code object names do not match"
            );
            assert_eq!(
                $actual.bytecode, $expected.bytecode,
                "Code object bytecode does not match"
            );
            assert_eq!(
                $actual.arg_count, $expected.arg_count,
                "Code object arg_count does not match"
            );
            assert_eq!(
                $actual.varnames, $expected.varnames,
                "Code object varnames do not match"
            );
            assert_eq!(
                $actual.names, $expected.names,
                "Code object names do not match"
            );
        };
    }

    #[test]
    fn function_definition_with_parameters() {
        let text = r#"
def foo(a, b):
    a + b
"#;
        let program = compile(text);
        assert_eq!(
            program.code.bytecode,
            vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ]
        );
        assert_eq!(program.constant_pool.len(), 1);
        let foo = get_code_at_index(&program, 0);
        assert_code_eq!(
            foo,
            &CodeObject {
                name: Some("foo".into()),
                bytecode: vec![
                    Opcode::LoadFast(Index::new(0)),
                    Opcode::LoadFast(Index::new(1)),
                    Opcode::Iadd,
                ],
                arg_count: 2,
                varnames: vec!["a".into(), "b".into()],
                names: vec![],
                source: Source::default(),
                line_map: vec![],
            }
        );
        assert_eq!(program.code.names.len(), 1);
        assert_eq!(get_names_index(&program, "foo"), 0);
    }

    #[test]
    fn function_definition_with_nested_function() {
        let text = r#"
def foo(a, b):
    def inner():
        return 10
    a + b
"#;
        let program = compile(text);
        assert_eq!(
            program.code.bytecode,
            vec![
                Opcode::LoadConst(Index::new(1)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ]
        );
        assert_eq!(program.constant_pool.len(), 2);
        let inner = get_code_at_index(&program, 0);
        assert_code_eq!(
            inner,
            &CodeObject {
                name: Some("inner".into()),
                bytecode: vec![Opcode::Push(10), Opcode::ReturnValue,],
                arg_count: 0,
                varnames: vec![],
                names: vec![],
                source: Source::default(),
                line_map: vec![],
            }
        );
        let foo = get_code_at_index(&program, 1);
        assert_code_eq!(
            foo,
            &CodeObject {
                name: Some("foo".into()),
                bytecode: vec![
                    Opcode::LoadConst(Index::new(0)),
                    Opcode::MakeFunction,
                    Opcode::StoreFast(Index::new(2)),
                    Opcode::LoadFast(Index::new(0)),
                    Opcode::LoadFast(Index::new(1)),
                    Opcode::Iadd,
                ],
                arg_count: 2,
                varnames: vec!["a".into(), "b".into(), "inner".into()],
                names: vec![],
                source: Source::default(),
                line_map: vec![],
            }
        );
        assert_eq!(program.code.names.len(), 1);
        assert_eq!(get_names_index(&program, "foo"), 0);
    }

    #[test]
    fn function_definition_with_local_var() {
        let text = r#"
def foo():
    c = 10
"#;
        let program = compile(text);
        assert_eq!(
            program.code.bytecode,
            vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ]
        );
        assert_eq!(program.constant_pool.len(), 1);
        let foo = get_code_at_index(&program, 0);
        assert_code_eq!(
            foo,
            &CodeObject {
                name: Some("foo".into()),
                bytecode: vec![Opcode::Push(10), Opcode::StoreFast(Index::new(0))],
                arg_count: 0,
                varnames: vec!["c".into()],
                names: vec![],
                source: Source::default(),
                line_map: vec![],
            }
        );
        assert_eq!(program.code.names.len(), 1);
        assert_eq!(get_names_index(&program, "foo"), 0);
    }

    #[test]
    fn function_definition_with_local_var_and_return() {
        let text = r#"
def foo():
    c = 10
    return c
"#;
        let program = compile(text);
        assert_eq!(
            program.code.bytecode,
            vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ]
        );
        assert_eq!(program.constant_pool.len(), 1);
        let foo = get_code_at_index(&program, 0);
        assert_code_eq!(
            foo,
            &CodeObject {
                name: Some("foo".into()),
                bytecode: vec![
                    Opcode::Push(10),
                    Opcode::StoreFast(Index::new(0)),
                    Opcode::LoadFast(Index::new(0)),
                    Opcode::ReturnValue
                ],
                arg_count: 0,
                varnames: vec!["c".into()],
                names: vec![],
                source: Source::default(),
                line_map: vec![],
            }
        );
        assert_eq!(program.code.names.len(), 1);
        assert_eq!(get_names_index(&program, "foo"), 0);
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
        let program = compile(text);
        assert_eq!(
            program.code.bytecode,
            vec![
                Opcode::LoadConst(Index::new(1)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(3)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::Call(0),
                Opcode::Halt,
            ]
        );
        assert_eq!(program.constant_pool.len(), 4);
        assert_eq!(program.constant_pool[0], Constant::String("Hello".into()));
        let hello = get_code_at_index(&program, 1);
        assert_code_eq!(
            hello,
            &CodeObject {
                name: Some("hello".into()),
                bytecode: vec![Opcode::PrintConst(Index::new(0)),],
                arg_count: 0,
                varnames: vec![],
                names: vec![],
                source: Source::default(),
                line_map: vec![],
            }
        );
        assert_eq!(program.constant_pool[2], Constant::String("World".into()));
        let world = get_code_at_index(&program, 3);
        assert_code_eq!(
            world,
            &CodeObject {
                name: Some("world".into()),
                bytecode: vec![Opcode::PrintConst(Index::new(2)),],
                arg_count: 0,
                varnames: vec![],
                names: vec![],
                source: Source::default(),
                line_map: vec![],
            }
        );
        assert_eq!(program.code.names.len(), 2);
        assert_eq!(get_names_index(&program, "hello"), 0);
        assert_eq!(get_names_index(&program, "world"), 1);
    }

    #[test]
    fn class_definition() {
        let text = r#"
class Foo:
    def bar(self):
        return 99
"#;
        let program = compile(text);
        assert_eq!(program.constant_pool.len(), 2);
        let bar = get_code_at_index(&program, 0);
        assert_code_eq!(
            bar,
            &CodeObject {
                name: Some("bar".into()),
                bytecode: vec![Opcode::Push(99), Opcode::ReturnValue,],
                arg_count: 1,
                varnames: vec!["self".into()],
                names: vec![],
                source: Source::default(),
                line_map: vec![],
            }
        );
        let foo = get_code_at_index(&program, 1);
        assert_eq!(
            foo.bytecode,
            vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
                Opcode::EndClass,
            ]
        );
        assert_eq!(foo.varnames.len(), 1);
        assert_eq!(get_varnames_index(foo, "bar"), 0);
        assert_eq!(
            program.code.bytecode,
            vec![
                Opcode::LoadBuildClass,
                Opcode::LoadConst(Index::new(1)),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ]
        );
        assert_eq!(program.code.names.len(), 1);
        assert_eq!(get_names_index(&program, "Foo"), 0);
    }

    #[test]
    fn class_definition_member_access() {
        let text = r#"
class Foo:
    def bar(self):
        return self.val
"#;
        let program = compile(text);
        assert_eq!(program.constant_pool.len(), 2);
        let bar = get_code_at_index(&program, 0);
        assert_code_eq!(
            bar,
            &CodeObject {
                name: Some("bar".into()),
                bytecode: vec![
                    Opcode::LoadFast(Index::new(0)),
                    Opcode::LoadAttr(Index::new(0)),
                    Opcode::ReturnValue,
                ],
                arg_count: 1,
                varnames: vec!["self".into()],
                names: vec!["val".into()],
                source: Source::default(),
                line_map: vec![],
            }
        );
        let foo = get_code_at_index(&program, 1);
        assert_eq!(
            foo.bytecode,
            vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
                Opcode::EndClass,
            ]
        );
        assert_eq!(foo.varnames.len(), 1);
        assert_eq!(get_varnames_index(&foo, "bar"), 0);
        assert_eq!(
            program.code.bytecode,
            vec![
                Opcode::LoadBuildClass,
                Opcode::LoadConst(Index::new(1)),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Halt,
            ]
        );
        assert_eq!(program.code.names.len(), 1);
        assert_eq!(get_names_index(&program, "Foo"), 0);
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
class Foo:
    def bar():
        return 99

f = Foo()
"#;
        let program = compile(text);
        assert_eq!(
            program.code.bytecode,
            vec![
                Opcode::LoadBuildClass,
                Opcode::LoadConst(Index::new(1)),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::Halt,
            ]
        );
        assert_eq!(program.code.names.len(), 2);
        assert_eq!(get_names_index(&program, "Foo"), 0);
        assert_eq!(get_names_index(&program, "f"), 1);
    }

    #[test]
    fn class_instantiation_and_method_call() {
        let text = r#"
class Foo:
    def bar(self):
        return 99

f = Foo()
b = f.bar()
"#;
        let program = compile(text);
        assert_eq!(
            program.code.bytecode,
            vec![
                Opcode::LoadBuildClass,
                Opcode::LoadConst(Index::new(1)),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::LoadAttr(Index::new(2)),
                Opcode::CallMethod(0),
                Opcode::StoreGlobal(Index::new(3)),
                Opcode::Halt,
            ]
        );
        assert_eq!(program.code.names.len(), 4);
        assert_eq!(get_names_index(&program, "Foo"), 0);
        assert_eq!(get_names_index(&program, "f"), 1);
        assert_eq!(get_names_index(&program, "bar"), 2);
        assert_eq!(get_names_index(&program, "b"), 3);
        assert_eq!(program.constant_pool.len(), 2);

        // this should be the code for the bar method
        let bar = get_code_at_index(&program, 0);
        assert_eq!(bar.bytecode, vec![Opcode::Push(99), Opcode::ReturnValue,]);

        // this should be the code for the class definition
        let class = get_code_at_index(&program, 1);
        assert_eq!(class.name(), "Foo");
    }
}
