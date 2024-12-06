use std::collections::HashMap;

pub mod types;

use crate::{
    bytecode_vm::{types::CompilerError, Opcode},
    core::Stack,
    domain::Context,
    parser::{
        types::{
            BinOp, Block, ConditionalBlock, Expr, ParsedArgDefinitions, ParsedArguments, Statement,
            UnaryOp,
        },
        Parser,
    },
    types::errors::MemphisError,
};

use self::types::{Bytecode, BytecodeNameMap, CodeObject, CompiledProgram, Constant};

use super::indices::{BytecodeIndex, ConstantIndex, Index};

pub struct Compiler {
    /// Variables defined in global scope, this maps their name to an index for use by the VM.
    /// Variables defined in the local scope will be mapped inside of a [`CodeObject`].
    name_map: BytecodeNameMap,

    /// Constants discovered during compilation. These will be compiled into the
    /// [`CompiledProgram`] which is handed off to the VM.
    constant_pool: Vec<Constant>,

    /// Keep a reference to the code object being constructed so we can associate things with it,
    /// (i.e. variable names).
    code_stack: Vec<CodeObject>,

    context_stack: Stack<Context>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            name_map: HashMap::new(),
            constant_pool: vec![],
            code_stack: vec![],
            context_stack: Stack::with_initial(Context::Global),
        }
    }

    pub fn compile(&mut self, parser: &mut Parser) -> Result<CompiledProgram, MemphisError> {
        let parsed_program = parser.parse().map_err(MemphisError::Parser)?;

        let mut bytecode = vec![];
        for stmt in parsed_program.iter() {
            let opcodes = self.compile_stmt(stmt).map_err(MemphisError::Compiler)?;
            bytecode.extend(opcodes);
        }
        bytecode.push(Opcode::Halt);

        let code = CodeObject {
            name: "__main__".into(),
            bytecode,
            arg_count: 0,
            varnames: vec![],
        };

        Ok(CompiledProgram::new(
            code,
            self.constant_pool.clone(),
            self.name_map.clone(),
        ))
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
        let index = self.get_or_set_var_index(name);
        match self.read_context() {
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
                let attr_index =
                    self.get_or_set_constant_index(Constant::String(field.to_string()));
                // TODO this should probably use the name map instead of the constant index
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

        let mut varnames = vec![];
        for param in args.args.iter() {
            varnames.push(param.arg.clone());
        }

        let code_object = CodeObject {
            name: name.to_string(),
            bytecode: vec![],
            arg_count: args.args.len(),
            varnames,
        };

        self.code_stack.push(code_object);
        let bytecode = self.compile_block(body)?;

        let mut code = self.code_stack.pop().unwrap();
        self.context_stack.pop();

        code.bytecode = bytecode;
        let code_index = self.get_or_set_constant_index(Constant::Code(code));
        let name_index = self.get_or_set_constant_index(Constant::String(name.to_string()));

        Ok(vec![
            Opcode::LoadConst(code_index),
            Opcode::LoadConst(name_index),
            Opcode::MakeFunction,
            self.compile_store(name),
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

        let code_object = CodeObject {
            name: format!("<class '{}'>", name),
            bytecode: vec![],
            arg_count: 0,
            varnames: vec![],
        };

        self.context_stack.push(Context::Local);
        self.code_stack.push(code_object);

        let class_body = self.compile_block(body)?;

        let mut code = self.code_stack.pop().unwrap();
        self.context_stack.pop();

        code.bytecode = class_body;
        code.bytecode.push(Opcode::EndClass);

        let mut bytecode = vec![Opcode::LoadBuildClass];
        let code_index = self.get_or_set_constant_index(Constant::Code(code));
        bytecode.push(Opcode::LoadConst(code_index));
        let name_index = self.get_or_set_constant_index(Constant::String(name.to_string()));
        bytecode.push(Opcode::LoadConst(name_index));

        // the 2 here refers to the name of the class and the class body
        // once we support base classes, it will become 3
        bytecode.push(Opcode::PopAndCall(2));

        let _ = self.get_or_set_var_index(name);

        Ok(bytecode)
    }

    fn compile_string_literal(&mut self, value: &str) -> Result<Bytecode, CompilerError> {
        let index = self.get_or_set_constant_index(Constant::String(value.to_string()));
        Ok(vec![Opcode::LoadConst(index)])
    }

    fn compile_variable(&mut self, name: &str) -> Result<Bytecode, CompilerError> {
        if let Some(index) = self.get_var_index(name) {
            let opcode = match self.read_context() {
                Context::Local => Opcode::LoadFast(index),
                Context::Global => Opcode::LoadGlobal(index),
            };
            Ok(vec![opcode])
        } else {
            Err(CompilerError::NameError(format!(
                "name '{}' is not defined",
                name
            )))
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
            let index =
                self.get_or_set_constant_index(Constant::String(args.args[0].as_string().unwrap()));
            Ok(vec![Opcode::PrintConst(index)])
        } else {
            let index = self.get_var_index(name);
            if index.is_none() {
                unimplemented!(
                    "{}",
                    format!("Function '{}' not yet supported in the bytecode VM.", name)
                )
            }
            let index = index.unwrap();
            let mut opcodes = vec![];
            // We push the args onto the stack in reverse call order so that we will pop
            // them off in call order.
            for arg in args.args.iter().rev() {
                opcodes.extend(self.compile_expr(arg)?);
            }
            opcodes.push(Opcode::Call(index));
            Ok(opcodes)
        }
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
        let attr_index = self.get_or_set_constant_index(Constant::String(field.to_string()));
        // TODO this should probably use the name map instead of constant index
        bytecode.push(Opcode::LoadAttr(attr_index));
        Ok(bytecode)
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Bytecode, CompilerError> {
        match expr {
            Expr::None => {
                let index = self.get_or_set_constant_index(Constant::None);
                Ok(vec![Opcode::LoadConst(index)])
            }
            Expr::Boolean(value) => {
                let index = self.get_or_set_constant_index(Constant::Boolean(*value));
                Ok(vec![Opcode::LoadConst(index)])
            }
            Expr::Integer(value) => Ok(vec![Opcode::Push(*value)]),
            Expr::StringLiteral(value) => self.compile_string_literal(value),
            Expr::Variable(name) => self.compile_variable(name),
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

    fn get_or_set_var_index(&mut self, name: &str) -> BytecodeIndex {
        match self.read_context() {
            Context::Global => {
                if let Some(index) = self.name_map.get(name) {
                    *index
                } else {
                    let next_index = Index::new(self.name_map.len());
                    self.name_map.insert(name.into(), next_index);
                    next_index
                }
            }
            Context::Local => {
                if let Some(code) = self.code_stack.last_mut() {
                    let next_index = Index::new(code.varnames.len());
                    code.varnames.push(name.to_string());
                    next_index
                } else {
                    panic!("Not in local scope");
                }
            }
        }
    }

    pub fn get_var_index(&self, name: &str) -> Option<BytecodeIndex> {
        match self.read_context() {
            Context::Global => self.name_map.get(name).copied(),
            Context::Local => {
                if let Some(code) = self.code_stack.last() {
                    if let Some(index) = find_index(&code.varnames, &name.to_string()) {
                        return Some(Index::new(index));
                    }
                }

                None
            }
        }
    }

    fn get_or_set_constant_index(&mut self, value: Constant) -> ConstantIndex {
        if let Some(index) = find_index(&self.constant_pool, &value) {
            Index::new(index)
        } else {
            let next_index = self.constant_pool.len();
            self.constant_pool.push(value);
            Index::new(next_index)
        }
    }

    /// This assumes we always have a context stack.
    fn read_context(&self) -> Context {
        self.context_stack.top().expect("failed to find context")
    }
}

fn find_index<T: PartialEq>(vec: &[T], query: &T) -> Option<usize> {
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
        compiler.get_or_set_var_index("a");
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

        let mut compiler = init_compiler();
        let stmt = Statement::Expression(Expr::BinaryOperation {
            left: Box::new(Expr::Integer(2)),
            op: BinOp::Add,
            right: Box::new(Expr::Variable("b".into())),
        });

        match compiler.compile_stmt(&stmt) {
            Err(e) => {
                assert_eq!(
                    e,
                    CompilerError::NameError("name 'b' is not defined".into())
                );
            }
            Ok(_) => panic!("Expected an error!"),
        }
    }

    #[test]
    fn member_access() {
        let mut compiler = init_compiler();
        compiler.get_or_set_var_index("foo");
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
                        Opcode::SetAttr(Index::new(0))
                    ]
                );
            }
        }

        let mut compiler = init_compiler();
        compiler.get_or_set_var_index("foo");
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
                        Opcode::LoadAttr(Index::new(0))
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
        compiler.get_or_set_var_index("a");
        compiler.get_or_set_var_index("b");
        compiler.get_or_set_var_index("foo");
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
                        Opcode::LoadGlobal(Index::new(1)),
                        Opcode::LoadGlobal(Index::new(0)),
                        Opcode::Call(Index::new(2)),
                    ]
                );
            }
        }
    }

    #[test]
    fn method_call() {
        let mut compiler = init_compiler();
        compiler.get_or_set_var_index("foo");
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
                        Opcode::LoadAttr(Index::new(0)),
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

    fn name_map(program: &CompiledProgram, name: &str) -> usize {
        *program.name_map.get(name).cloned().unwrap()
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
                        Opcode::LoadConst(Index::new(1)),
                        Opcode::MakeFunction,
                        Opcode::StoreGlobal(Index::new(0)),
                        Opcode::Halt,
                    ]
                );
                assert_eq!(
                    program.constant_pool[0],
                    Constant::Code(CodeObject {
                        name: "foo".into(),
                        bytecode: vec![
                            Opcode::LoadFast(Index::new(0)),
                            Opcode::LoadFast(Index::new(1)),
                            Opcode::Iadd,
                        ],
                        arg_count: 2,
                        varnames: vec!["a".into(), "b".into()],
                    })
                );
                assert_eq!(program.constant_pool[1], Constant::String("foo".into()));
                assert_eq!(name_map(&program, "foo"), 0);
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
                        Opcode::LoadConst(Index::new(1)),
                        Opcode::MakeFunction,
                        Opcode::StoreGlobal(Index::new(0)),
                        Opcode::Halt,
                    ]
                );
                assert_eq!(
                    program.constant_pool[0],
                    Constant::Code(CodeObject {
                        name: "foo".into(),
                        bytecode: vec![Opcode::Push(10), Opcode::StoreFast(Index::new(0))],
                        arg_count: 0,
                        varnames: vec!["c".into()],
                    })
                );
                assert_eq!(program.constant_pool[1], Constant::String("foo".into()));
                assert_eq!(name_map(&program, "foo"), 0);
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
                        Opcode::LoadConst(Index::new(1)),
                        Opcode::MakeFunction,
                        Opcode::StoreGlobal(Index::new(0)),
                        Opcode::Halt,
                    ]
                );
                assert_eq!(
                    program.constant_pool[0],
                    Constant::Code(CodeObject {
                        name: "foo".into(),
                        bytecode: vec![
                            Opcode::Push(10),
                            Opcode::StoreFast(Index::new(0)),
                            Opcode::LoadFast(Index::new(0)),
                            Opcode::ReturnValue
                        ],
                        arg_count: 0,
                        varnames: vec!["c".into()],
                    })
                );
                assert_eq!(program.constant_pool[1], Constant::String("foo".into()));
                assert_eq!(name_map(&program, "foo"), 0);
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
                        Opcode::LoadConst(Index::new(2)),
                        Opcode::MakeFunction,
                        Opcode::StoreGlobal(Index::new(0)),
                        Opcode::LoadConst(Index::new(4)),
                        Opcode::LoadConst(Index::new(5)),
                        Opcode::MakeFunction,
                        Opcode::StoreGlobal(Index::new(1)),
                        Opcode::Call(Index::new(0)),
                        Opcode::Call(Index::new(1)),
                        Opcode::Halt,
                    ]
                );
                assert_eq!(program.constant_pool.len(), 6);
                assert_eq!(program.constant_pool[0], Constant::String("Hello".into()));
                assert_eq!(
                    program.constant_pool[1],
                    Constant::Code(CodeObject {
                        name: "hello".into(),
                        bytecode: vec![Opcode::PrintConst(Index::new(0)),],
                        arg_count: 0,
                        varnames: vec![],
                    })
                );
                assert_eq!(program.constant_pool[2], Constant::String("hello".into()));
                assert_eq!(program.constant_pool[3], Constant::String("World".into()));
                assert_eq!(
                    program.constant_pool[4],
                    Constant::Code(CodeObject {
                        name: "world".into(),
                        bytecode: vec![Opcode::PrintConst(Index::new(3)),],
                        arg_count: 0,
                        varnames: vec![],
                    })
                );
                assert_eq!(program.constant_pool[5], Constant::String("world".into()));
                assert_eq!(name_map(&program, "hello"), 0);
                assert_eq!(name_map(&program, "world"), 1);
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
                assert_eq!(program.constant_pool.len(), 4);
                let Some(Constant::Code(code)) = program.constant_pool.get(0) else {
                    panic!()
                };
                assert_eq!(
                    code,
                    &CodeObject {
                        name: "bar".into(),
                        bytecode: vec![Opcode::Push(99), Opcode::ReturnValue,],
                        arg_count: 1,
                        varnames: vec!["self".into()],
                    }
                );
                assert_eq!(program.constant_pool[1], Constant::String("bar".into()));
                let Some(Constant::Code(code)) = program.constant_pool.get(2) else {
                    panic!()
                };
                assert_eq!(
                    code.bytecode,
                    vec![
                        Opcode::LoadConst(Index::new(0)),
                        Opcode::LoadConst(Index::new(1)),
                        Opcode::MakeFunction,
                        Opcode::StoreFast(Index::new(0)),
                        Opcode::EndClass,
                    ]
                );
                assert_eq!(code.varnames.len(), 1);
                assert_eq!(code.varnames[0], "bar");
                assert_eq!(program.constant_pool[3], Constant::String("Foo".into()));
                assert_eq!(
                    program.code.bytecode,
                    vec![
                        Opcode::LoadBuildClass,
                        Opcode::LoadConst(Index::new(2)),
                        Opcode::LoadConst(Index::new(3)),
                        Opcode::PopAndCall(2),
                        Opcode::Halt,
                    ]
                );
                assert_eq!(program.name_map.len(), 1);
                assert_eq!(name_map(&program, "Foo"), 0);
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
                        Opcode::LoadConst(Index::new(2)),
                        Opcode::LoadConst(Index::new(3)),
                        Opcode::PopAndCall(2),
                        Opcode::Call(Index::new(0)),
                        Opcode::StoreGlobal(Index::new(1)),
                        Opcode::Halt,
                    ]
                );
                assert_eq!(program.name_map.len(), 2);
                assert_eq!(name_map(&program, "Foo"), 0);
                assert_eq!(name_map(&program, "f"), 1);
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
                        Opcode::LoadConst(Index::new(2)),
                        Opcode::LoadConst(Index::new(3)),
                        Opcode::PopAndCall(2),
                        Opcode::Call(Index::new(0)),
                        Opcode::StoreGlobal(Index::new(1)),
                        Opcode::LoadGlobal(Index::new(1)),
                        Opcode::LoadAttr(Index::new(1)),
                        Opcode::CallMethod(0),
                        Opcode::StoreGlobal(Index::new(2)),
                        Opcode::Halt,
                    ]
                );
                assert_eq!(program.name_map.len(), 3);
                assert_eq!(name_map(&program, "Foo"), 0);
                assert_eq!(name_map(&program, "f"), 1);
                assert_eq!(name_map(&program, "b"), 2);
                assert_eq!(program.constant_pool.len(), 4);

                // this should be the code for the bar method
                let Some(Constant::Code(c)) = program.constant_pool.get(0) else {
                    panic!()
                };
                assert_eq!(c.bytecode, vec![Opcode::Push(99), Opcode::ReturnValue,]);

                // this should be the code for the class definition
                let Some(Constant::Code(_)) = program.constant_pool.get(2) else {
                    panic!()
                };

                assert_eq!(program.constant_pool[1], Constant::String("bar".into()));
                assert_eq!(program.constant_pool[3], Constant::String("Foo".into()));
            }
        }
    }
}
