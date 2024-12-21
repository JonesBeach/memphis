pub mod types;

use crate::{
    bytecode_vm::{types::CompilerError, Opcode},
    core::{log, LogLevel},
    domain::Context,
    parser::types::{
        Ast, BinOp, Block, ConditionalBlock, Expr, ParsedArgDefinitions, ParsedArguments,
        Statement, UnaryOp,
    },
};

use self::types::{Bytecode, CodeObject, CompiledProgram, Constant};

use super::indices::{BytecodeIndex, ConstantIndex, Index};

pub struct Compiler {
    /// Constants discovered during compilation. These will be compiled into the
    /// [`CompiledProgram`] which is handed off to the VM.
    constant_pool: Vec<Constant>,

    /// Keep a reference to the code object being constructed so we can associate things with it,
    /// (i.e. variable names).
    code_stack: Vec<CodeObject>,

    context_stack: Vec<Context>,
}

impl Compiler {
    pub fn new() -> Self {
        let code = CodeObject::new("__main__".to_string());
        Self {
            constant_pool: vec![],
            code_stack: vec![code],
            context_stack: vec![Context::Global],
        }
    }

    pub fn compile(&mut self, ast: Ast) -> Result<CompiledProgram, CompilerError> {
        let mut bytecode = vec![];
        for stmt in ast.iter() {
            let opcodes = self.compile_stmt(stmt)?;
            bytecode.extend(opcodes);
        }
        bytecode.push(Opcode::Halt);

        let mut code = self.code_stack.pop().ok_or(CompilerError::StackUnderflow)?;

        code.bytecode = bytecode;

        Ok(CompiledProgram::new(code, self.constant_pool.clone()))
    }

    fn compile_block(&mut self, block: &Block) -> Result<Bytecode, CompilerError> {
        let mut opcodes = vec![];
        for stmt in block.statements.iter() {
            opcodes.extend(self.compile_stmt(stmt)?);
        }
        Ok(opcodes)
    }

    fn compile_return(&mut self, expr: &[Expr]) -> Result<Bytecode, CompilerError> {
        if expr.len() > 1 {
            unimplemented!("Multiple return values not yet supported in the bytecode VM.")
        }

        let mut opcodes = vec![];
        opcodes.extend(self.compile_expr(&expr[0])?);
        opcodes.push(Opcode::ReturnValue);
        Ok(opcodes)
    }

    fn compile_store(&mut self, name: &str) -> Opcode {
        let index = self.get_or_set_local_index(name);
        self.compile_store_to_index(index)
    }

    fn compile_store_to_index(&mut self, index: BytecodeIndex) -> Opcode {
        match self.ensure_context() {
            Context::Global => Opcode::StoreGlobal(index),
            Context::Local => Opcode::StoreFast(index),
        }
    }

    fn compile_assignment(&mut self, left: &Expr, right: &Expr) -> Result<Bytecode, CompilerError> {
        let mut opcodes = vec![];

        match left {
            Expr::Variable(name) => {
                opcodes.extend(self.compile_expr(right)?);
                let opcode = self.compile_store(name);
                opcodes.push(opcode);
                Ok(opcodes)
            }
            Expr::MemberAccess { object, field } => {
                // Push the object onto the stack
                opcodes.extend(self.compile_expr(object)?);
                // Push the value to be assigned onto the stack
                opcodes.extend(self.compile_expr(right)?);
                let attr_index = self.get_or_set_nonlocal_index(field);
                opcodes.push(Opcode::SetAttr(attr_index));
                Ok(opcodes)
            }
            Expr::IndexAccess { .. } => {
                unimplemented!("Index access assignment not yet supported in bytecode VM.");
            }
            _ => Err(CompilerError::SyntaxError(
                "cannot assign to that expression type here.".into(),
            )),
        }
    }

    fn compile_while_loop(
        &mut self,
        condition: &Expr,
        body: &Block,
    ) -> Result<Bytecode, CompilerError> {
        let mut opcodes = vec![];
        let condition_start = opcodes.len();
        opcodes.extend(self.compile_expr(condition)?);

        // Temporary offset, we will change this once we know the length of the loop body
        let jump_if_false_placeholder = opcodes.len();
        opcodes.push(Opcode::Placeholder);

        opcodes.extend(self.compile_block(body)?);

        // Unconditional jump back to the start of the condition
        // We must mark these as isize because we are doing subtraction with potential overflow
        let jump_back_offset = condition_start as isize - opcodes.len() as isize - 1;
        opcodes.push(Opcode::Jump(jump_back_offset));

        // Update the JUMP_IF_FALSE offset now that we know the length of the loop body
        let jump_if_false_offset = opcodes.len() as isize - jump_if_false_placeholder as isize - 1;
        opcodes[jump_if_false_placeholder] = Opcode::JumpIfFalse(jump_if_false_offset);

        Ok(opcodes)
    }

    fn compile_if_else(
        &mut self,
        if_part: &ConditionalBlock,
        elif_parts: &[ConditionalBlock],
        else_part: &Option<Block>,
    ) -> Result<Bytecode, CompilerError> {
        if !elif_parts.is_empty() {
            unreachable!("elif not yet supported in the bytecode VM.")
        }

        let mut opcodes = vec![];
        opcodes.extend(self.compile_expr(&if_part.condition)?);

        // Temporary offset, we will change this once we know the length of the if condition body
        let jump_if_false_placeholder = opcodes.len();
        opcodes.push(Opcode::Placeholder);

        opcodes.extend(self.compile_block(&if_part.block)?);
        if let Some(else_part) = else_part {
            let jump_else_placeholder = opcodes.len();
            opcodes.push(Opcode::Placeholder);

            let jump_if_false_offset =
                opcodes.len() as isize - jump_if_false_placeholder as isize - 1;
            opcodes[jump_if_false_placeholder] = Opcode::JumpIfFalse(jump_if_false_offset);

            opcodes.extend(self.compile_block(else_part)?);
            let jump_else_offset = opcodes.len() as isize - jump_else_placeholder as isize - 1;
            opcodes[jump_else_placeholder] = Opcode::Jump(jump_else_offset);
        } else {
            let jump_if_false_offset =
                opcodes.len() as isize - jump_if_false_placeholder as isize - 1;
            opcodes[jump_if_false_placeholder] = Opcode::JumpIfFalse(jump_if_false_offset);
        }

        Ok(opcodes)
    }

    fn compile_function_definition(
        &mut self,
        name: &str,
        args: &ParsedArgDefinitions,
        body: &Block,
        decorators: &[Expr],
        is_async: &bool,
    ) -> Result<Bytecode, CompilerError> {
        if !decorators.is_empty() || *is_async {
            unimplemented!(
                "Decorators and async functions are not yet supported in the bytecode VM."
            )
        }

        self.context_stack.push(Context::Local);

        let varnames = args.args.iter().map(|p| p.arg.clone()).collect();
        let code_object = CodeObject::with_args(name.to_string(), varnames);

        self.code_stack.push(code_object);
        let bytecode = self.compile_block(body)?;

        let mut code = self.code_stack.pop().unwrap();
        self.context_stack.pop();

        code.bytecode = bytecode;
        let name_index = self.get_or_set_local_index(name);

        Ok(vec![
            self.compile_code(code),
            Opcode::MakeFunction,
            self.compile_store_to_index(name_index),
        ])
    }

    fn compile_class_definition(
        &mut self,
        name: &str,
        parents: &[Expr],
        metaclass: &Option<String>,
        body: &Block,
    ) -> Result<Bytecode, CompilerError> {
        if !parents.is_empty() {
            unimplemented!("Inheritance not yet supported in the bytecode VM.")
        }
        if metaclass.is_some() {
            unimplemented!("Metaclasses are not yet supported in the bytecode VM.")
        }

        let code_object = CodeObject::new(name.to_string());

        self.context_stack.push(Context::Local);
        self.code_stack.push(code_object);

        let class_body = self.compile_block(body)?;

        let mut code = self.code_stack.pop().unwrap();
        self.context_stack.pop();

        code.bytecode = class_body;
        code.bytecode.push(Opcode::EndClass);

        let mut bytecode = vec![Opcode::LoadBuildClass];
        bytecode.push(self.compile_code(code));

        // subtract one to ignore Opcode::LoadBuildClass
        let num_args = bytecode.len() - 1;
        bytecode.push(Opcode::Call(num_args));

        let name_index = self.get_or_set_local_index(name);
        bytecode.push(self.compile_store_to_index(name_index));

        Ok(bytecode)
    }

    fn compile_string_literal(&mut self, value: &str) -> Opcode {
        self.compile_constant(Constant::String(value.to_string()))
    }

    fn compile_none(&mut self) -> Opcode {
        self.compile_constant(Constant::None)
    }

    fn compile_bool(&mut self, bool: bool) -> Opcode {
        self.compile_constant(Constant::Boolean(bool))
    }

    fn compile_code(&mut self, code: CodeObject) -> Opcode {
        self.compile_constant(Constant::Code(code))
    }

    fn compile_constant(&mut self, constant: Constant) -> Opcode {
        let index = self.get_or_set_constant_index(constant);
        Opcode::LoadConst(index)
    }

    fn compile_variable(&mut self, name: &str) -> Opcode {
        let (context, index) = self.get_local_index(name);
        match context {
            Context::Local => Opcode::LoadFast(index),
            Context::Global => Opcode::LoadGlobal(index),
        }
    }

    fn compile_unary_operation(
        &mut self,
        op: &UnaryOp,
        right: &Expr,
    ) -> Result<Bytecode, CompilerError> {
        let mut opcodes = Vec::new();
        opcodes.extend(self.compile_expr(right)?);
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
            opcodes.push(opcode);
        }
        Ok(opcodes)
    }

    fn compile_binary_operation(
        &mut self,
        left: &Expr,
        op: &BinOp,
        right: &Expr,
    ) -> Result<Bytecode, CompilerError> {
        let mut opcodes = Vec::new();
        opcodes.extend(self.compile_expr(left)?);
        opcodes.extend(self.compile_expr(right)?);
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
        opcodes.push(opcode);
        Ok(opcodes)
    }

    fn compile_function_call(
        &mut self,
        name: &str,
        args: &ParsedArguments,
        callee: &Option<Box<Expr>>,
    ) -> Result<Bytecode, CompilerError> {
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
            return Ok(vec![Opcode::PrintConst(index)]);
        }

        let mut opcodes = vec![self.compile_variable(name)];

        // We push the args onto the stack in reverse call order so that we will pop
        // them off in call order.
        for arg in args.args.iter().rev() {
            opcodes.extend(self.compile_expr(arg)?);
        }

        let argc = opcodes.len() - 1;
        opcodes.push(Opcode::Call(argc));
        Ok(opcodes)
    }

    fn compile_method_call(
        &mut self,
        object: &Expr,
        name: &str,
        args: &ParsedArguments,
    ) -> Result<Bytecode, CompilerError> {
        if !args.kwargs.is_empty() {
            unimplemented!(
                "Method calls with kwargs not yet supported for print in the bytecode VM."
            )
        }

        let mut bytecode = vec![];
        bytecode.extend(self.compile_member_access(object, name)?);
        for arg in args.args.iter().rev() {
            bytecode.extend(self.compile_expr(arg)?);
        }
        bytecode.push(Opcode::CallMethod(args.args.len()));
        Ok(bytecode)
    }

    fn compile_member_access(
        &mut self,
        object: &Expr,
        field: &str,
    ) -> Result<Bytecode, CompilerError> {
        let mut bytecode = vec![];
        bytecode.extend(self.compile_expr(object)?);
        let attr_index = self.get_or_set_nonlocal_index(field);
        bytecode.push(Opcode::LoadAttr(attr_index));
        Ok(bytecode)
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Bytecode, CompilerError> {
        match expr {
            Expr::None => Ok(vec![self.compile_none()]),
            Expr::Boolean(value) => Ok(vec![self.compile_bool(*value)]),
            Expr::Integer(value) => Ok(vec![Opcode::Push(*value)]),
            Expr::StringLiteral(value) => Ok(vec![self.compile_string_literal(value)]),
            Expr::Variable(name) => Ok(vec![self.compile_variable(name)]),
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

    fn compile_stmt(&mut self, stmt: &Statement) -> Result<Bytecode, CompilerError> {
        match stmt {
            Statement::Pass => Ok(vec![]),
            Statement::Expression(expr) => self.compile_expr(expr),
            Statement::Return(expr) => self.compile_return(expr),
            Statement::Assignment { left, right } => self.compile_assignment(left, right),
            Statement::WhileLoop { condition, body } => self.compile_while_loop(condition, body),
            Statement::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => self.compile_if_else(if_part, elif_parts, else_part),
            Statement::FunctionDef {
                name,
                args,
                body,
                decorators,
                is_async,
            } => self.compile_function_definition(name, args, body, decorators, is_async),
            Statement::ClassDef {
                name,
                parents,
                metaclass,
                body,
            } => self.compile_class_definition(name, parents, metaclass, body),
            _ => unimplemented!("Statement type {:?} not implemented for bytecode VM", stmt),
        }
    }

    fn get_or_set_local_index(&mut self, name: &str) -> BytecodeIndex {
        match self.ensure_context() {
            Context::Global => self.get_or_set_nonlocal_index(name),
            Context::Local => {
                log(LogLevel::Debug, || {
                    format!("Looking for '{}' in locals", name)
                });
                let code = self.ensure_code_object();
                let next_index = Index::new(code.varnames.len());
                code.varnames.push(name.to_string());
                next_index
            }
        }
    }

    fn get_local_index(&mut self, name: &str) -> (Context, BytecodeIndex) {
        match self.ensure_context() {
            Context::Global => (Context::Global, self.get_or_set_nonlocal_index(name)),
            Context::Local => {
                // Check locals first
                log(LogLevel::Debug, || {
                    format!("Looking for '{}' in locals", name)
                });
                let code = self.ensure_code_object();
                if let Some(index) = find_index(&code.varnames, name) {
                    return (Context::Local, Index::new(index));
                }

                // If not found locally, fall back to globals
                (Context::Global, self.get_or_set_nonlocal_index(name))
            }
        }
    }

    fn get_or_set_nonlocal_index(&mut self, name: &str) -> BytecodeIndex {
        log(LogLevel::Debug, || {
            format!("Looking for '{}' in globals", name)
        });
        let code = self.ensure_code_object();
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

    fn ensure_code_object(&mut self) -> &mut CodeObject {
        self.code_stack
            .last_mut()
            .expect("Internal Compiler Error: failed to find current code object.")
    }
}

pub fn find_index<T, Q>(vec: &[T], query: &Q) -> Option<usize>
where
    T: PartialEq<Q>,
    Q: ?Sized,
{
    vec.iter().enumerate().find_map(
        |(index, value)| {
            if value == query {
                Some(index)
            } else {
                None
            }
        },
    )
}

#[cfg(test)]
mod bytecode_tests {
    use super::*;

    use crate::parser::types::ParsedArguments;

    fn init_compiler() -> Compiler {
        Compiler::new()
    }

    #[test]
    fn expression() {
        let mut compiler = init_compiler();
        let expr = Expr::BinaryOperation {
            left: Box::new(Expr::Integer(4)),
            op: BinOp::Mul,
            right: Box::new(Expr::BinaryOperation {
                left: Box::new(Expr::Integer(2)),
                op: BinOp::Add,
                right: Box::new(Expr::Integer(3)),
            }),
        };

        match compiler.compile_expr(&expr) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::Push(4),
                        Opcode::Push(2),
                        Opcode::Push(3),
                        Opcode::Iadd,
                        Opcode::Imul,
                    ]
                );
            }
        }
    }

    #[test]
    fn binary_operations() {
        let mut compiler = init_compiler();
        let expr = Expr::BinaryOperation {
            left: Box::new(Expr::Integer(4)),
            op: BinOp::LessThan,
            right: Box::new(Expr::Integer(5)),
        };

        match compiler.compile_expr(&expr) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![Opcode::Push(4), Opcode::Push(5), Opcode::LessThan,]
                );
            }
        }

        let mut compiler = init_compiler();
        let expr = Expr::BinaryOperation {
            left: Box::new(Expr::Integer(4)),
            op: BinOp::GreaterThan,
            right: Box::new(Expr::Integer(5)),
        };

        match compiler.compile_expr(&expr) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![Opcode::Push(4), Opcode::Push(5), Opcode::GreaterThan,]
                );
            }
        }
    }

    #[test]
    fn unary_operations() {
        let mut compiler = init_compiler();
        let expr = Expr::UnaryOperation {
            op: UnaryOp::Minus,
            right: Box::new(Expr::Integer(4)),
        };

        match compiler.compile_expr(&expr) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(bytecode, vec![Opcode::Push(4), Opcode::UnaryNegative]);
            }
        }

        let mut compiler = init_compiler();
        let expr = Expr::UnaryOperation {
            op: UnaryOp::Plus,
            right: Box::new(Expr::Integer(4)),
        };

        match compiler.compile_expr(&expr) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(bytecode, vec![Opcode::Push(4)]);
            }
        }

        let mut compiler = init_compiler();
        let expr = Expr::UnaryOperation {
            op: UnaryOp::Not,
            right: Box::new(Expr::Boolean(false)),
        };

        match compiler.compile_expr(&expr) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![Opcode::LoadConst(Index::new(0)), Opcode::UnaryNot]
                );
            }
        }

        let mut compiler = init_compiler();
        let expr = Expr::UnaryOperation {
            op: UnaryOp::BitwiseNot,
            right: Box::new(Expr::Integer(4)),
        };

        match compiler.compile_expr(&expr) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(bytecode, vec![Opcode::Push(4), Opcode::UnaryInvert]);
            }
        }
    }

    #[test]
    fn assignment() {
        let mut compiler = init_compiler();
        let stmt = Statement::Assignment {
            left: Expr::Variable("var".into()),
            right: Expr::BinaryOperation {
                left: Box::new(Expr::Integer(5)),
                op: BinOp::Sub,
                right: Box::new(Expr::Integer(2)),
            },
        };

        match compiler.compile_stmt(&stmt) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::Push(5),
                        Opcode::Push(2),
                        Opcode::Isub,
                        Opcode::StoreGlobal(Index::new(0)),
                    ]
                );
            }
        }

        let mut compiler = init_compiler();
        let stmt = Statement::Assignment {
            left: Expr::Variable("var".into()),
            right: Expr::StringLiteral("Hello World".into()),
        };

        match compiler.compile_stmt(&stmt) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::LoadConst(Index::new(0)),
                        Opcode::StoreGlobal(Index::new(0)),
                    ]
                );
            }
        }

        let mut compiler = init_compiler();
        let stmt = Statement::Assignment {
            left: Expr::Variable("var".into()),
            right: Expr::None,
        };

        match compiler.compile_stmt(&stmt) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::LoadConst(Index::new(0)),
                        Opcode::StoreGlobal(Index::new(0)),
                    ]
                );
            }
        }

        let mut compiler = init_compiler();
        let stmt = Statement::Expression(Expr::BinaryOperation {
            left: Box::new(Expr::Integer(2)),
            op: BinOp::Add,
            right: Box::new(Expr::Variable("a".into())),
        });

        match compiler.compile_stmt(&stmt) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::Push(2),
                        Opcode::LoadGlobal(Index::new(0)),
                        Opcode::Iadd,
                    ]
                );
            }
        }
    }

    #[test]
    fn member_access() {
        let mut compiler = init_compiler();
        let stmt = Statement::Assignment {
            left: Expr::MemberAccess {
                object: Box::new(Expr::Variable("foo".into())),
                field: "x".into(),
            },
            right: Expr::Integer(4),
        };

        match compiler.compile_stmt(&stmt) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::LoadGlobal(Index::new(0)),
                        Opcode::Push(4),
                        Opcode::SetAttr(Index::new(1))
                    ]
                );
            }
        }

        let mut compiler = init_compiler();
        let stmt = Statement::Expression(Expr::MemberAccess {
            object: Box::new(Expr::Variable("foo".into())),
            field: "x".into(),
        });

        match compiler.compile_stmt(&stmt) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::LoadGlobal(Index::new(0)),
                        Opcode::LoadAttr(Index::new(1))
                    ]
                );
            }
        }
    }

    #[test]
    fn while_loop() {
        let mut compiler = init_compiler();
        let stmt = Statement::WhileLoop {
            condition: Expr::BinaryOperation {
                left: Box::new(Expr::Integer(4)),
                op: BinOp::LessThan,
                right: Box::new(Expr::Integer(5)),
            },
            body: Block::new(vec![]),
        };

        match compiler.compile_stmt(&stmt) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::Push(4),
                        Opcode::Push(5),
                        Opcode::LessThan,
                        Opcode::JumpIfFalse(1),
                        Opcode::Jump(-5),
                    ]
                );
            }
        }
    }

    #[test]
    fn if_else() {
        let mut compiler = init_compiler();
        let stmt = Statement::IfElse {
            if_part: ConditionalBlock {
                condition: Expr::BinaryOperation {
                    left: Box::new(Expr::Integer(4)),
                    op: BinOp::LessThan,
                    right: Box::new(Expr::Integer(5)),
                },
                block: Block::new(vec![Statement::Assignment {
                    left: Expr::Variable("a".into()),
                    right: Expr::Integer(-1),
                }]),
            },
            elif_parts: vec![],
            else_part: None,
        };

        match compiler.compile_stmt(&stmt) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::Push(4),
                        Opcode::Push(5),
                        Opcode::LessThan,
                        Opcode::JumpIfFalse(2),
                        Opcode::Push(-1),
                        Opcode::StoreGlobal(Index::new(0)),
                    ]
                );
            }
        }

        let mut compiler = init_compiler();
        let stmt = Statement::IfElse {
            if_part: ConditionalBlock {
                condition: Expr::BinaryOperation {
                    left: Box::new(Expr::Integer(4)),
                    op: BinOp::LessThan,
                    right: Box::new(Expr::Integer(5)),
                },
                block: Block::new(vec![Statement::Assignment {
                    left: Expr::Variable("a".into()),
                    right: Expr::Integer(-3),
                }]),
            },
            elif_parts: vec![],
            else_part: Some(Block::new(vec![Statement::Assignment {
                left: Expr::Variable("a".into()),
                right: Expr::Integer(3),
            }])),
        };

        match compiler.compile_stmt(&stmt) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
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
        }
    }

    #[test]
    fn function_call() {
        let mut compiler = init_compiler();
        let expr = Expr::FunctionCall {
            name: "foo".into(),
            args: ParsedArguments {
                args: vec![Expr::Variable("a".into()), Expr::Variable("b".into())],
                kwargs: vec![],
                args_var: None,
            },
            callee: None,
        };

        match compiler.compile_expr(&expr) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::LoadGlobal(Index::new(0)),
                        Opcode::LoadGlobal(Index::new(1)),
                        Opcode::LoadGlobal(Index::new(2)),
                        Opcode::Call(2),
                    ]
                );
            }
        }
    }

    #[test]
    fn method_call() {
        let mut compiler = init_compiler();
        let expr = Expr::MethodCall {
            object: Box::new(Expr::Variable("foo".to_string())),
            name: "bar".to_string(),
            args: ParsedArguments {
                args: vec![Expr::Integer(88), Expr::Integer(99)],
                kwargs: vec![],
                args_var: None,
            },
        };

        match compiler.compile_expr(&expr) {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(bytecode) => {
                assert_eq!(
                    bytecode,
                    vec![
                        Opcode::LoadGlobal(Index::new(0)),
                        Opcode::LoadAttr(Index::new(1)),
                        Opcode::Push(99),
                        Opcode::Push(88),
                        Opcode::CallMethod(2),
                    ]
                );
            }
        }
    }
}

#[cfg(test)]
mod compiler_state_tests {
    use super::*;

    use crate::init::MemphisContext;

    fn init(text: &str) -> MemphisContext {
        MemphisContext::from_text(text)
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

    #[test]
    fn function_definition_with_parameters() {
        let text = r#"
def foo(a, b):
    a + b
"#;
        let mut context = init(text);

        match context.compile() {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(program) => {
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
                assert_eq!(
                    foo,
                    &CodeObject {
                        name: "foo".into(),
                        bytecode: vec![
                            Opcode::LoadFast(Index::new(0)),
                            Opcode::LoadFast(Index::new(1)),
                            Opcode::Iadd,
                        ],
                        arg_count: 2,
                        varnames: vec!["a".into(), "b".into()],
                        names: vec![],
                    }
                );
                assert_eq!(program.code.names.len(), 1);
                assert_eq!(get_names_index(&program, "foo"), 0);
            }
        }
    }

    #[test]
    fn function_definition_with_nested_function() {
        let text = r#"
def foo(a, b):
    def inner():
        return 10
    a + b
"#;
        let mut context = init(text);

        match context.compile() {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(program) => {
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
                assert_eq!(
                    inner,
                    &CodeObject {
                        name: "inner".into(),
                        bytecode: vec![Opcode::Push(10), Opcode::ReturnValue,],
                        arg_count: 0,
                        varnames: vec![],
                        names: vec![],
                    }
                );
                let foo = get_code_at_index(&program, 1);
                assert_eq!(
                    foo,
                    &CodeObject {
                        name: "foo".into(),
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
                    }
                );
                assert_eq!(program.code.names.len(), 1);
                assert_eq!(get_names_index(&program, "foo"), 0);
            }
        }
    }

    #[test]
    fn function_definition_with_local_var() {
        let text = r#"
def foo():
    c = 10
"#;
        let mut context = init(text);

        match context.compile() {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(program) => {
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
                assert_eq!(
                    foo,
                    &CodeObject {
                        name: "foo".into(),
                        bytecode: vec![Opcode::Push(10), Opcode::StoreFast(Index::new(0))],
                        arg_count: 0,
                        varnames: vec!["c".into()],
                        names: vec![],
                    }
                );
                assert_eq!(program.code.names.len(), 1);
                assert_eq!(get_names_index(&program, "foo"), 0);
            }
        }
    }

    #[test]
    fn function_definition_with_local_var_and_return() {
        let text = r#"
def foo():
    c = 10
    return c
"#;
        let mut context = init(text);

        match context.compile() {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(program) => {
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
                assert_eq!(
                    foo,
                    &CodeObject {
                        name: "foo".into(),
                        bytecode: vec![
                            Opcode::Push(10),
                            Opcode::StoreFast(Index::new(0)),
                            Opcode::LoadFast(Index::new(0)),
                            Opcode::ReturnValue
                        ],
                        arg_count: 0,
                        varnames: vec!["c".into()],
                        names: vec![],
                    }
                );
                assert_eq!(program.code.names.len(), 1);
                assert_eq!(get_names_index(&program, "foo"), 0);
            }
        }
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
        let mut context = init(text);

        match context.compile() {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(program) => {
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
                assert_eq!(
                    hello,
                    &CodeObject {
                        name: "hello".into(),
                        bytecode: vec![Opcode::PrintConst(Index::new(0)),],
                        arg_count: 0,
                        varnames: vec![],
                        names: vec![],
                    }
                );
                assert_eq!(program.constant_pool[2], Constant::String("World".into()));
                let world = get_code_at_index(&program, 3);
                assert_eq!(
                    world,
                    &CodeObject {
                        name: "world".into(),
                        bytecode: vec![Opcode::PrintConst(Index::new(2)),],
                        arg_count: 0,
                        varnames: vec![],
                        names: vec![],
                    }
                );
                assert_eq!(program.code.names.len(), 2);
                assert_eq!(get_names_index(&program, "hello"), 0);
                assert_eq!(get_names_index(&program, "world"), 1);
            }
        }
    }

    #[test]
    fn class_definition() {
        let text = r#"
class Foo:
    def bar(self):
        return 99
"#;
        let mut context = init(text);

        match context.compile() {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(program) => {
                assert_eq!(program.constant_pool.len(), 2);
                let bar = get_code_at_index(&program, 0);
                assert_eq!(
                    bar,
                    &CodeObject {
                        name: "bar".into(),
                        bytecode: vec![Opcode::Push(99), Opcode::ReturnValue,],
                        arg_count: 1,
                        varnames: vec!["self".into()],
                        names: vec![],
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
        }
    }

    #[test]
    fn class_definition_member_access() {
        let text = r#"
class Foo:
    def bar(self):
        return self.val
"#;
        let mut context = init(text);

        match context.compile() {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(program) => {
                assert_eq!(program.constant_pool.len(), 2);
                let bar = get_code_at_index(&program, 0);
                assert_eq!(
                    bar,
                    &CodeObject {
                        name: "bar".into(),
                        bytecode: vec![
                            Opcode::LoadFast(Index::new(0)),
                            Opcode::LoadAttr(Index::new(0)),
                            Opcode::ReturnValue,
                        ],
                        arg_count: 1,
                        varnames: vec!["self".into()],
                        names: vec!["val".into()],
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
        }
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
class Foo:
    def bar():
        return 99

f = Foo()
"#;
        let mut context = init(text);

        match context.compile() {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(program) => {
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
        }
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
        let mut context = init(text);

        match context.compile() {
            Err(e) => panic!("Unexpected error: {:?}", e),
            Ok(program) => {
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
                assert_eq!(class.name, "Foo");
            }
        }
    }
}
