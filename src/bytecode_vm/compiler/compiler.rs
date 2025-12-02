use crate::{
    bytecode_vm::{
        compiler::{CodeObject, CompilerError, Constant, Opcode},
        find_index,
        indices::{ConstantIndex, FreeIndex, Index, LocalIndex, NonlocalIndex},
        CompilerResult,
    },
    core::{log, LogLevel},
    domain::{resolve_import_path, Context, FunctionType, ImportPath, ModuleName},
    parser::types::{
        Ast, BinOp, CallArgs, Callee, CompareOp, ConditionalAst, DictOperation, Expr, LogicalOp,
        LoopIndex, Params, RegularImport, SelectMode, Statement, StatementKind, UnaryOp,
    },
};

use super::opcode::{SignedOffset, UnsignedOffset};

/// A Python bytecode compiler.
pub struct Compiler {
    filename: String,
    module_name: ModuleName,

    /// Keep a reference to the code object being constructed so we can associate things with it,
    /// (variable names, constants, etc.).
    code_stack: Vec<CodeObject>,

    /// The most recent line number seen from the Ast.
    line_number: UnsignedOffset,
}

impl Compiler {
    pub fn new(module_name: ModuleName, filename: &str) -> Self {
        let code = CodeObject::new(module_name.clone(), filename);

        Self {
            filename: filename.to_string(),
            module_name,
            code_stack: vec![code],
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

    fn compile_ast_with_code(&mut self, ast: &Ast, code: CodeObject) -> CompilerResult<CodeObject> {
        self.code_stack.push(code);
        self.compile_ast(ast)?;
        self.code_stack
            .pop()
            .ok_or_else(|| internal_error("Code stack underflow."))
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
        Ok(self.current_offset()? as SignedOffset - to as SignedOffset - 1)
    }

    // We must mark these as signed because we are doing subtraction which could product a negative
    // value.
    fn backward_offset_from(&self, from: UnsignedOffset) -> CompilerResult<SignedOffset> {
        Ok(from as SignedOffset - self.current_offset()? as SignedOffset - 1)
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
            StatementKind::Expression(expr) => {
                self.compile_expr(expr)?;

                match expr {
                    // Yield and YieldFrom are special cases where they don't leave a value on the
                    // stack. This still feels a bit odd to me.
                    Expr::Yield(_) | Expr::YieldFrom(_) | Expr::Await(_) => {}
                    _ => {
                        // If an expression is used as a statement, we must tell the VM to discard
                        // the result from the stack.
                        self.emit(Opcode::PopTop)?;
                    }
                }
            }
            StatementKind::Return(expr) => self.compile_return(expr)?,
            StatementKind::Assignment { left, right } => self.compile_assignment(left, right)?,
            StatementKind::WhileLoop(cond_ast) => self.compile_while_loop(cond_ast)?,
            StatementKind::ForInLoop {
                index,
                iterable,
                body,
                else_block,
            } => self.compile_for_in_loop(index, iterable, body, else_block)?,
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
            StatementKind::RegularImport(items) => self.compile_regular_import(items)?,
            StatementKind::SelectiveImport { import_path, mode } => {
                self.compile_selective_import(import_path, mode)?
            }
            _ => return Err(unsupported(&format!("Statement type: {stmt:?}"))),
        };

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> CompilerResult<()> {
        match expr {
            Expr::None => self.compile_none(),
            Expr::Boolean(value) => self.compile_bool(*value),
            Expr::Integer(value) => self.compile_int(*value),
            Expr::Float(value) => self.compile_float(*value),
            Expr::StringLiteral(value) => self.compile_string_literal(value),
            Expr::Variable(name) => self.compile_load(name),
            Expr::List(items) => self.compile_list(items),
            Expr::Tuple(items) => self.compile_tuple(items),
            Expr::Dict(dict_op) => self.compile_dict(dict_op),
            Expr::UnaryOperation { op, right } => self.compile_unary_op(op, right),
            Expr::BinaryOperation { left, op, right } => self.compile_binary_op(left, op, right),
            Expr::ComparisonChain { left, ops } => self.compile_comparison_chain(left, ops),
            Expr::LogicalOperation { left, op, right } => self.compile_logical_op(left, op, right),
            Expr::MemberAccess { object, field } => self.compile_member_access(object, field),
            Expr::FunctionCall { callee, args } => self.compile_function_call(callee, args),
            Expr::Yield(value) => self.compile_yield(value),
            Expr::YieldFrom(value) => self.compile_yield_from(value),
            Expr::Await(expr) => self.compile_await(expr),
            _ => Err(unsupported(&format!("Expression type: {expr:?}"))),
        }
    }

    fn generate_load(&mut self, name: &str) -> CompilerResult<Opcode> {
        match self.context() {
            Context::Global => Ok(Opcode::LoadGlobal(self.get_or_set_nonlocal_index(name)?)),
            Context::Local => {
                // Check locals first (top of the stack)
                if let Some(index) = self.get_local_index(name)? {
                    return Ok(Opcode::LoadFast(index));
                }

                // Now check if this is a free variable, meaning a variable captured from a
                // non-global outer function.
                // We skip the first (top) entry because it's the current code object and the last
                // (bottom) entry because it's the global scope.
                let enclosing_scopes = &self.code_stack[1..self.code_stack.len() - 1];
                for code in enclosing_scopes.iter().rev() {
                    if self.resolve_local_index_for_code(name, code).is_some() {
                        // This would be a local in an enclosing scope, but we need an index
                        // relative to our own code object.
                        return Ok(Opcode::LoadFree(self.get_or_set_free_var(name)?));
                    }
                }

                // If it's not local or free, it's global. Put that quote on the wall.
                Ok(Opcode::LoadGlobal(self.get_or_set_nonlocal_index(name)?))
            }
        }
    }

    fn generate_store(&mut self, name: &str) -> CompilerResult<Opcode> {
        let opcode = match self.context() {
            Context::Global => Opcode::StoreGlobal(self.get_or_set_nonlocal_index(name)?),
            Context::Local => Opcode::StoreFast(self.get_or_set_local_index(name)?),
        };
        Ok(opcode)
    }

    fn compile_return(&mut self, expr: &[Expr]) -> CompilerResult<()> {
        if expr.len() > 1 {
            return Err(unsupported(
                "Multiple return values not yet supported in the bytecode VM.",
            ));
        } else if expr.len() == 1 {
            self.compile_expr(&expr[0])?;
        }

        self.emit(Opcode::ReturnValue)?;
        Ok(())
    }

    fn compile_yield(&mut self, expr: &Option<Box<Expr>>) -> CompilerResult<()> {
        if let Some(expr) = expr {
            self.compile_expr(expr)?;
        }

        self.emit(Opcode::YieldValue)?;
        Ok(())
    }

    fn compile_yield_from(&mut self, expr: &Expr) -> CompilerResult<()> {
        self.compile_expr(expr)?;
        self.emit(Opcode::YieldFrom)?;
        Ok(())
    }

    fn compile_await(&mut self, expr: &Expr) -> CompilerResult<()> {
        self.compile_expr(expr)?;
        self.emit(Opcode::Await)?;
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
                return Err(unsupported(
                    "Index access assignment not yet supported in bytecode VM.",
                ))
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

    fn compile_for_in_loop(
        &mut self,
        index: &LoopIndex,
        iterable: &Expr,
        body: &Ast,
        else_block: &Option<Ast>,
    ) -> CompilerResult<()> {
        if else_block.is_some() {
            return Err(unsupported(
                "'else' not yet supported for a for loop in bytecode VM.",
            ));
        }
        let LoopIndex::Variable(index) = index else {
            return Err(unsupported(
                "Tuple indicies not yet supported in bytecode VM.",
            ));
        };
        self.compile_expr(iterable)?;
        self.emit(Opcode::GetIter)?;

        // where FOR_ITER will live
        let loop_header = self.current_offset()?;

        // Emit placeholder FOR_ITER with dummy offset
        let for_iter_placeholder = self.emit_placeholder()?;

        self.compile_store(index)?;
        self.compile_ast(body)?;

        // jump back to loop_header
        let jump_back_offset = self.backward_offset_from(loop_header)?;
        self.emit(Opcode::Jump(jump_back_offset))?;

        // patch FOR_ITER to jump to end_of_loop
        let for_iter_offset = self.forward_offset_to(for_iter_placeholder)?;
        self.emit_at(for_iter_placeholder, Opcode::ForIter(for_iter_offset))?;
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

    /// Load a CodeObject and turn it into a function or closure.
    fn make_function(&mut self, code: CodeObject) -> CompilerResult<()> {
        let free_vars = code.freevars.clone();
        self.compile_code(code)?;

        if free_vars.is_empty() {
            self.emit(Opcode::MakeFunction)?;
        } else {
            // We push the free vars onto the stack in reverse order so that we will pop
            // them off in order.
            for free_var in free_vars.iter().rev() {
                self.compile_load(free_var)?;
            }
            self.emit(Opcode::MakeClosure(free_vars.len()))?;
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
        let function_type = if body.has_yield() {
            FunctionType::Generator
        } else if *is_async {
            FunctionType::Async
        } else {
            FunctionType::Regular
        };

        let varnames = args
            .args
            .iter()
            .map(|p| p.arg.clone())
            .collect::<Vec<String>>();
        let code_object = CodeObject::new_function(
            name,
            self.module_name.clone(),
            &self.filename,
            &varnames,
            function_type,
        );

        let code = self.compile_ast_with_code(body, code_object)?;

        // Compile decorators in reverse
        for decorator in decorators.iter().rev() {
            self.compile_expr(decorator)?;
        }

        // Make the functino/closure itself out of the compiled code
        self.make_function(code)?;

        // Apply the decorators - innermost outward
        for _ in decorators {
            // The 1 is for the function we are wrapping
            self.emit(Opcode::Call(1))?;
        }

        // Bind the final decorated function
        self.compile_store(name)?;
        Ok(())
    }

    fn compile_regular_import(&mut self, items: &[RegularImport]) -> CompilerResult<()> {
        for item in items {
            let index = self.get_or_set_nonlocal_index(&item.module_path.as_str())?;
            self.emit(Opcode::ImportName(index))?;

            let symbol_index = item
                .alias
                .as_ref()
                .map(|alias| self.get_or_set_nonlocal_index(alias))
                .unwrap_or_else(|| {
                    let head = item.module_path.head().expect("No head!");
                    self.get_or_set_nonlocal_index(head)
                })?;
            self.emit(Opcode::StoreGlobal(symbol_index))?;
        }
        Ok(())
    }

    fn compile_selective_import(
        &mut self,
        import_path: &ImportPath,
        mode: &SelectMode,
    ) -> CompilerResult<()> {
        let module_name = resolve_import_path(import_path, &self.module_name)
            .map_err(|e| CompilerError::import_error(e.message()))?;

        let index = self.get_or_set_nonlocal_index(&module_name.as_str())?;
        self.emit(Opcode::ImportName(index))?;

        match mode {
            SelectMode::All => self.emit(Opcode::ImportAll)?,
            SelectMode::List(items) => {
                for item in items {
                    let attr_index = self.get_or_set_nonlocal_index(item.original())?;
                    self.emit(Opcode::LoadAttr(attr_index))?;

                    let alias_index = self.get_or_set_nonlocal_index(item.imported())?;
                    self.emit(Opcode::StoreGlobal(alias_index))?;
                }
            }
        }

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
            return Err(unsupported(
                "Inheritance not yet supported in the bytecode VM.",
            ));
        }
        if metaclass.is_some() {
            return Err(unsupported(
                "Metaclasses are not yet supported in the bytecode VM.",
            ));
        }

        let code_object = CodeObject::new_function(
            name,
            self.module_name.clone(),
            &self.filename,
            &[],
            FunctionType::Regular,
        );
        let code = self.compile_ast_with_code(body, code_object)?;

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

    fn compile_int(&mut self, int: i64) -> CompilerResult<()> {
        self.compile_constant(Constant::Int(int))
    }

    fn compile_float(&mut self, float: f64) -> CompilerResult<()> {
        self.compile_constant(Constant::Float(float))
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

    fn compile_expr_slice(&mut self, items: &[Expr]) -> CompilerResult<()> {
        for item in items {
            self.compile_expr(item)?;
        }
        Ok(())
    }

    fn compile_list(&mut self, items: &[Expr]) -> CompilerResult<()> {
        self.compile_expr_slice(items)?;
        self.emit(Opcode::BuildList(items.len()))?;
        Ok(())
    }

    fn compile_tuple(&mut self, items: &[Expr]) -> CompilerResult<()> {
        self.compile_expr_slice(items)?;
        self.emit(Opcode::BuildTuple(items.len()))?;
        Ok(())
    }

    fn compile_dict(&mut self, dict_ops: &[DictOperation]) -> CompilerResult<()> {
        for dict_op in dict_ops.iter() {
            let (key, value) = match dict_op {
                DictOperation::Pair(key, value) => (key, value),
                DictOperation::Unpack(_) => unimplemented!("Unpacking not yet supported."),
            };
            self.compile_expr(key)?;
            self.compile_expr(value)?;
        }
        self.emit(Opcode::BuildMap(dict_ops.len()))?;
        Ok(())
    }

    fn compile_unary_op(&mut self, op: &UnaryOp, right: &Expr) -> CompilerResult<()> {
        self.compile_expr(right)?;

        let opcode = match op {
            UnaryOp::Minus => Some(Opcode::UnaryNegative),
            // this acts as a no-op. can be overridden with __pos__ for custom classes
            UnaryOp::Plus => None,
            UnaryOp::Not => Some(Opcode::UnaryNot),
            UnaryOp::BitwiseNot => Some(Opcode::UnaryInvert),
            _ => return Err(unsupported(&format!("unary op: {op:?}"))),
        };
        if let Some(opcode) = opcode {
            self.emit(opcode)?;
        }
        Ok(())
    }

    fn compile_binary_op(
        &mut self,
        left: &Expr,
        bin_op: &BinOp,
        right: &Expr,
    ) -> CompilerResult<()> {
        self.compile_expr(left)?;
        self.compile_expr(right)?;

        let opcode = Opcode::try_from_bin_op(bin_op)
            .ok_or_else(|| unsupported(&format!("binary op: {bin_op:?}")))?;

        self.emit(opcode)?;
        Ok(())
    }

    /// Pseudocode for an operator chain:
    /// evaluate left
    /// for each (op, right):
    ///     evaluate right
    ///     compare op
    ///     if false: goto end
    ///     if not last iteration: pop true
    ///     else: leave it
    /// end:
    fn compile_comparison_chain(
        &mut self,
        left: &Expr,
        ops: &[(CompareOp, Expr)],
    ) -> CompilerResult<()> {
        if ops.is_empty() {
            panic!("Comparison chain must have >= 1 op.");
        }

        self.compile_expr(left)?;

        let mut false_jumps = vec![];
        for (i, (op, right)) in ops.iter().enumerate() {
            let last_op = i == ops.len() - 1;

            self.compile_expr(right)?;

            // Preserve the right-hand side for the next comparison
            if !last_op {
                self.emit(Opcode::DupTop)?;
                self.emit(Opcode::RotThree)?;
            }

            self.emit(Opcode::from(op))?;

            // If any comparison evaluates to False, jump to end.
            // Otherwise, pop the True and continue the chain.
            // Unless it's the last operation, and we should leave the result on the stack.
            if !last_op {
                // At the end of the loop, we will patch this with a JumpIfFalse
                false_jumps.push(self.emit_placeholder()?);

                self.emit(Opcode::PopTop)?;
            }
        }

        // Patch all fail jumps to here
        for placeholder in false_jumps {
            let offset = self.forward_offset_to(placeholder)?;
            self.emit_at(placeholder, Opcode::JumpIfFalse(offset))?;
        }

        Ok(())
    }

    fn compile_logical_op(
        &mut self,
        left: &Expr,
        op: &LogicalOp,
        right: &Expr,
    ) -> CompilerResult<()> {
        // Compile the first operand.
        self.compile_expr(left)?;

        // This will be replaced with a jmp.
        let ph = self.emit_placeholder()?;

        // Discard the first operand if we got this far.
        self.emit(Opcode::PopTop)?;

        // Compile the second operand.
        self.compile_expr(right)?;

        // Calculate the offset which represents the position after this logical op, then update
        // the placeholder.
        let offset = self.forward_offset_to(ph)?;
        let opcode = match op {
            LogicalOp::And => Opcode::JumpIfFalse(offset),
            LogicalOp::Or => Opcode::JumpIfTrue(offset),
        };
        self.emit_at(ph, opcode)?;

        Ok(())
    }

    fn compile_function_call(&mut self, callee: &Callee, args: &CallArgs) -> CompilerResult<()> {
        match callee {
            Callee::Expr(callee) => self.compile_expr(callee)?,
            Callee::Symbol(name) => self.compile_load(name)?,
        };

        // We push the args onto the stack in reverse call order so that we will pop
        // them off in call order.
        for arg in args.args.iter().rev() {
            self.compile_expr(arg)?;
        }

        self.emit(Opcode::Call(args.args.len()))?;
        Ok(())
    }

    fn compile_member_access(&mut self, object: &Expr, field: &str) -> CompilerResult<()> {
        self.compile_expr(object)?;
        let attr_index = self.get_or_set_nonlocal_index(field)?;
        self.emit(Opcode::LoadAttr(attr_index))?;
        Ok(())
    }

    fn get_or_set_local_index(&mut self, name: &str) -> CompilerResult<LocalIndex> {
        log(LogLevel::Trace, || {
            format!("Looking for '{name}' in locals")
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
        Ok(self.resolve_local_index_for_code(name, code))
    }

    fn get_or_set_free_var(&mut self, name: &str) -> CompilerResult<FreeIndex> {
        let code = self.ensure_code_object_mut()?;
        let index = if let Some(index) = find_index(&code.freevars, name) {
            index
        } else {
            let new_index = code.freevars.len();
            code.freevars.push(name.to_string());
            new_index
        };
        Ok(Index::new(index))
    }

    fn resolve_local_index_for_code(&self, name: &str, code: &CodeObject) -> Option<LocalIndex> {
        find_index(&code.varnames, name).map(Index::new)
    }

    fn get_or_set_nonlocal_index(&mut self, name: &str) -> CompilerResult<NonlocalIndex> {
        log(LogLevel::Trace, || {
            format!("Looking for '{name}' in globals")
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
        log(LogLevel::Trace, || {
            format!("Looking for '{value}' in constants")
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

    /// Since an instance of this `Compiler` operates on a single module, we can assume
    /// that the outer code object is the global scope and any others are local scopes.
    fn context(&self) -> Context {
        match self.code_stack.len() {
            1 => Context::Global,
            _ => Context::Local,
        }
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

// We previously used a panic macro for this, but using a type lets us display errors in other
// clients.
fn unsupported(msg: &str) -> CompilerError {
    CompilerError::Unsupported(msg.to_string())
}

#[cfg(test)]
mod tests_bytecode {
    use super::*;

    use crate::{
        bytecode_vm::compiler::{test_utils::*, Bytecode},
        parser::{
            test_utils::*,
            types::{ast, ImportedItem},
        },
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
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LoadConst(Index::new(2)),
                Opcode::Add,
                Opcode::Mul,
            ]
        );
    }

    #[test]
    fn binary_expressions_mathematical_op() {
        let expr = bin_op!(int!(4), Add, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Add,
            ]
        );

        let expr = bin_op!(int!(4), Sub, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Sub,
            ]
        );

        let expr = bin_op!(int!(4), Mul, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Mul,
            ]
        );

        let expr = bin_op!(int!(4), Div, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Div,
            ]
        );
    }

    #[test]
    fn binary_expressions_compare_op() {
        let expr = cmp_op!(int!(4), Equals, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Eq,
            ]
        );

        let expr = cmp_op!(int!(4), NotEquals, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Ne,
            ]
        );

        let expr = cmp_op!(int!(4), Is, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Is,
            ]
        );

        let expr = cmp_op!(int!(4), IsNot, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::IsNot,
            ]
        );

        let expr = cmp_op!(int!(4), LessThan, int!(5));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LessThan,
            ]
        );

        let expr = cmp_op!(int!(4), GreaterThan, int!(5));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::GreaterThan,
            ]
        );

        let expr = cmp_op!(int!(4), In, list![int!(5)]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(1),
                Opcode::In,
            ]
        );

        let expr = cmp_op!(int!(4), NotIn, list![int!(5)]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(1),
                Opcode::NotIn,
            ]
        );
    }

    #[test]
    fn operator_chaining() {
        let expr = cmp_chain!(int!(4), [(Equals, float!(5.1))]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Eq,
            ]
        );

        let expr = cmp_chain!(int!(4), [(Equals, float!(5.1)), (Equals, int!(4))]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::DupTop,
                Opcode::RotThree,
                Opcode::Eq,
                Opcode::JumpIfFalse(3),
                Opcode::PopTop,
                Opcode::LoadConst(Index::new(0)),
                Opcode::Eq,
            ]
        );
    }

    #[test]
    fn binary_expressions_logical_op() {
        let expr = logic_op!(bool!(true), And, bool!(false));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::JumpIfFalse(2),
                Opcode::PopTop,
                Opcode::LoadConst(Index::new(1)),
            ]
        );

        let expr = logic_op!(bool!(true), Or, bool!(false));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::JumpIfTrue(2),
                Opcode::PopTop,
                Opcode::LoadConst(Index::new(1)),
            ]
        );
    }

    #[test]
    fn unary_operations() {
        let expr = unary_op!(Minus, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadConst(Index::new(0)), Opcode::UnaryNegative]
        );

        let expr = unary_op!(Plus, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(bytecode, &[Opcode::LoadConst(Index::new(0))]);

        let expr = unary_op!(Not, Expr::Boolean(false));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadConst(Index::new(0)), Opcode::UnaryNot]
        );

        let expr = unary_op!(BitwiseNot, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadConst(Index::new(0)), Opcode::UnaryInvert]
        );
    }

    #[test]
    fn assignment() {
        let s = stmt_assign!(var!("var"), bin_op!(int!(5), Sub, int!(2)));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
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
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Add,
            ]
        );
    }

    #[test]
    fn lists() {
        let expr = list![int!(2), int!(3)];
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(2),
            ]
        );
    }

    #[test]
    fn tuples() {
        let expr = tuple![int!(2), int!(3)];
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildTuple(2),
            ]
        );
    }

    #[test]
    fn dictionaries() {
        let expr = dict![dict_pair!(str!("a"), int!(1))];
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildMap(1),
            ]
        );
    }

    #[test]
    fn await_expr() {
        let expr = await_expr!(var!("x"));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadGlobal(Index::new(0)), Opcode::Await,]
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
                Opcode::LoadConst(Index::new(0)),
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
    fn expression_as_statement() {
        let s = stmt_expr!(func_call!("print"));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::PopTop,
            ]
        );
    }

    #[test]
    fn while_loop() {
        let s = stmt!(StatementKind::WhileLoop(ConditionalAst {
            condition: cmp_op!(int!(4), LessThan, int!(5)),
            ast: ast![],
        }));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LessThan,
                Opcode::JumpIfFalse(1),
                Opcode::Jump(-5),
            ]
        );
    }

    #[test]
    fn for_in_loop() {
        let s = stmt!(StatementKind::ForInLoop {
            index: LoopIndex::Variable("i".to_string()),
            iterable: list![int!(1), int!(2)],
            body: ast![stmt_assign!(var!("a"), int!(-1))],
            else_block: None
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(2),
                Opcode::GetIter,
                Opcode::ForIter(4),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::Jump(-5),
            ]
        );
    }

    #[test]
    fn if_else_only_if() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(int!(4), LessThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![],
            else_part: None,
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LessThan,
                Opcode::JumpIfFalse(2),
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn if_else_with_else() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(int!(4), LessThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-3))],
            },
            elif_parts: vec![],
            else_part: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LessThan,
                Opcode::JumpIfFalse(2),
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(3)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn if_else_with_elif() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(int!(4), GreaterThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![ConditionalAst {
                condition: cmp_op!(int!(4), GreaterThan, int!(4)),
                ast: ast![stmt_assign!(var!("a"), int!(-2))],
            }],
            else_part: None,
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                // if: 0-2
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(3), // jump to elif condition
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(6), // skip rest
                // elif: 7-9
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(2), // jump past elif block if false
                Opcode::LoadConst(Index::new(3)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn if_else_with_elif_and_else() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(int!(4), GreaterThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![ConditionalAst {
                condition: cmp_op!(int!(4), GreaterThan, int!(4)),
                ast: ast![stmt_assign!(var!("a"), int!(-2))],
            }],
            else_part: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                // if: 0-2
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(3), // jump to elif condition
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(9), // skip rest
                // elif: 7-9
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(3), // jump past elif block if false
                Opcode::LoadConst(Index::new(3)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(2), // skip else
                Opcode::LoadConst(Index::new(4)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn function_call_user_defined() {
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
    fn function_call_builtin() {
        let expr = func_call!("list", call_args![]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadGlobal(Index::new(0)), Opcode::Call(0),]
        );
    }

    #[test]
    fn method_call() {
        let expr = func_call_callee!(
            member_access!(var!("foo"), "bar"),
            call_args![int!(88), int!(99)]
        );
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Call(2),
            ]
        );
    }

    #[test]
    fn function_call_with_callee() {
        let expr = func_call_callee!(
            func_call!("test_decorator", call_args![var!("get_val_undecorated")]),
            call_args![]
        );
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::Call(1),
                Opcode::Call(0),
            ]
        );
    }

    #[test]
    fn regular_import() {
        let expr = stmt!(StatementKind::RegularImport(vec![import!("other")]));
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn regular_import_multiple_layers() {
        let expr = stmt!(StatementKind::RegularImport(vec![import!("other.module")]));
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ]
        );
    }

    #[test]
    fn regular_import_alias() {
        let expr = stmt!(StatementKind::RegularImport(vec![import!("other", "foo")]));
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ]
        );
    }

    #[test]
    fn regular_import_multiple() {
        let expr = stmt!(StatementKind::RegularImport(vec![
            import!("first"),
            import!("second"),
        ]));
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::ImportName(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ]
        );
    }

    #[test]
    fn regular_import_multiple_alias() {
        let expr = stmt!(StatementKind::RegularImport(vec![
            import!("first", "first_alias"),
            import!("second", "second_alias"),
        ]));
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::ImportName(Index::new(2)),
                Opcode::StoreGlobal(Index::new(3)),
            ]
        );
    }

    #[test]
    fn selective_import_all() {
        let expr = stmt!(StatementKind::SelectiveImport {
            import_path: ImportPath::from("other"),
            mode: SelectMode::All,
        });
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[Opcode::ImportName(Index::new(0)), Opcode::ImportAll,]
        );
    }

    #[test]
    fn selective_import_single() {
        let expr = stmt!(StatementKind::SelectiveImport {
            import_path: ImportPath::from("other"),
            mode: SelectMode::List(vec![ImportedItem::direct("foo")]),
        });
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ]
        );
    }

    #[test]
    fn selective_import_single_alias() {
        let expr = stmt!(StatementKind::SelectiveImport {
            import_path: ImportPath::from("other"),
            mode: SelectMode::List(vec![ImportedItem::aliased("foo", "bar")]),
        });
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(2)),
            ]
        );
    }

    #[test]
    fn selective_import_multiple_alias() {
        let expr = stmt!(StatementKind::SelectiveImport {
            import_path: ImportPath::from("other"),
            mode: SelectMode::List(vec![
                ImportedItem::aliased("foo", "bar"),
                ImportedItem::aliased("one", "two")
            ]),
        });
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(2)),
                Opcode::LoadAttr(Index::new(3)),
                Opcode::StoreGlobal(Index::new(4)),
            ]
        );
    }
}

#[cfg(test)]
mod tests_compiler {
    use crate::{bytecode_vm::compiler::test_utils::*, domain::FunctionType};

    use super::*;

    #[test]
    fn function_definition_early_return() {
        let text = r#"
def foo():
    return
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![Opcode::ReturnValue],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_parameters() {
        let text = r#"
def foo(a, b):
    pass
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![],
            arg_count: 2,
            varnames: vec!["a".into(), "b".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_decorator() {
        let text = r#"
@decorate
def foo():
    pass
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["decorate".into(), fn_foo.name().into()],
            constants: vec![Constant::Code(fn_foo)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_multiple_decorators() {
        let text = r#"
@outer
@inner
def foo():
    pass
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::Call(1),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(2)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["inner".into(), "outer".into(), fn_foo.name().into()],
            constants: vec![Constant::Code(fn_foo)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };
        assert_code_eq!(code, expected);
    }

    #[test]
    fn generator_definition() {
        let text = r#"
def foo():
    yield 1
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::YieldValue],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(1)],
            line_map: vec![],
            function_type: FunctionType::Generator,
        };

        let expected = wrap_top_level_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn generator_definition_yield_from() {
        let text = r#"
def foo():
    yield from [1, 2]
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(2),
                Opcode::YieldFrom,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(1), Constant::Int(2)],
            line_map: vec![],
            function_type: FunctionType::Generator,
        };

        let expected = wrap_top_level_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn async_function_definition() {
        let text = r#"
async def foo():
    pass
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Async,
        };

        let expected = wrap_top_level_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_nested_function() {
        let text = r#"
def foo(a, b):
    def inner():
        return 10
    return a + b
"#;
        let code = compile(text);

        let fn_inner = CodeObject {
            module_name: ModuleName::main(),
            name: "inner".into(),
            filename: "<stdin>".into(),
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(10)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(2)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::Add,
                Opcode::ReturnValue,
            ],
            arg_count: 2,
            varnames: vec!["a".into(), "b".into(), "inner".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Code(fn_inner)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_local_var() {
        let text = r#"
def foo():
    c = 10
    d = 11.1
    e = 11.1
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreFast(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::StoreFast(Index::new(1)),
                // this should still be index 1 because we should reuse the 11.1
                Opcode::LoadConst(Index::new(1)),
                Opcode::StoreFast(Index::new(2)),
            ],
            arg_count: 0,
            varnames: vec!["c".into(), "d".into(), "e".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(10), Constant::Float(11.1)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_function(fn_foo);
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
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreFast(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::ReturnValue,
            ],
            arg_count: 0,
            varnames: vec!["c".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(10)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_function(fn_foo);
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
            module_name: ModuleName::main(),
            name: "hello".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::PopTop,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["print".into()],
            constants: vec![Constant::String("Hello".into())],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let fn_world = CodeObject {
            module_name: ModuleName::main(),
            name: "world".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::PopTop,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["print".into()],
            constants: vec![Constant::String("World".into())],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::PopTop,
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::Call(0),
                Opcode::PopTop,
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["hello".into(), "world".into()],
            constants: vec![Constant::Code(fn_hello), Constant::Code(fn_world)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn closure_definition() {
        let text = r#"
def make_adder(x):
    def inner_adder(y):
        return x + y
    return inner_adder
"#;
        let code = compile(text);

        let fn_inner_adder = CodeObject {
            module_name: ModuleName::main(),
            name: "inner_adder".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadFree(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::Add,
                Opcode::ReturnValue,
            ],
            arg_count: 1,
            varnames: vec!["y".into()],
            freevars: vec!["x".into()],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let fn_make_adder = CodeObject {
            module_name: ModuleName::main(),
            name: "make_adder".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::MakeClosure(1),
                Opcode::StoreFast(Index::new(1)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::ReturnValue,
            ],
            arg_count: 1,
            varnames: vec!["x".into(), "inner_adder".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Code(fn_inner_adder)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_function(fn_make_adder);
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
            module_name: ModuleName::main(),
            name: "bar".into(),
            filename: "<stdin>".into(),
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue],
            arg_count: 1,
            varnames: vec!["self".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(99)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let cls_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "Foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
            ],
            arg_count: 0,
            varnames: vec!["bar".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Code(fn_bar)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_class("Foo", cls_foo);
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
            module_name: ModuleName::main(),
            name: "bar".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadAttr(Index::new(0)),
                Opcode::ReturnValue,
            ],
            arg_count: 1,
            varnames: vec!["self".into()],
            freevars: vec![],
            names: vec!["val".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let cls_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "Foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
            ],
            arg_count: 0,
            varnames: vec!["bar".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Code(fn_bar)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_class("Foo", cls_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
f = Foo()
"#;
        let code = compile(text);

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["Foo".into(), "f".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn method_call() {
        let text = r#"
b = f.bar()
"#;
        let code = compile(text);

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(2)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["f".into(), "bar".into(), "b".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn selective_import_relative_one_layer() {
        let text = r#"
from .outer import foo
"#;
        let err = compile_err(text);

        match err {
            CompilerError::ImportError(msg) => assert_eq!(
                msg,
                "attempted relative import with no known parent package".to_string()
            ),
            _ => panic!("Expected an ImportError"),
        }
    }

    #[test]
    #[ignore]
    fn selective_import_relative_two_layers() {
        let text = r#"
from .outer.inner import foo
"#;
        let code = compile(text);

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::ImportName(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::Halt,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["outer.inner".into(), "foo".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(10)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
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
            freevars: vec![],
            names: vec!["foo".into(), "a".into()],
            constants: vec![Constant::Code(fn_foo)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        assert_code_eq!(code, expected);
    }
}
