use std::time::Duration;

use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Opcode},
        indices::{ConstantIndex, FreeIndex, LocalIndex, NonlocalIndex},
        runtime::{FunctionObject, List, Method, Module, Object, Reference, Tuple},
        VmResult, VmValue,
    },
    core::{log, log_impure, Container, LogLevel},
    domain::{Dunder, FunctionType},
    runtime::MemphisState,
};

use super::{
    builtins, error_builder::ErrorBuilder, executor::VmExecutor, frame::Frame,
    module_loader::ModuleLoader, BuiltinFunction, CallStack, Coroutine, Generator, Runtime,
};

#[derive(Debug)]
pub enum StepResult {
    Return(Reference),
    Yield(Reference),
    Await(Container<Coroutine>),
    Sleep(Duration),
    Continue,
    Halt,
}

pub struct VirtualMachine {
    runtime: Container<Runtime>,

    state: Container<MemphisState>,

    module_loader: ModuleLoader,

    pub executor: VmExecutor,

    pub error_builder: ErrorBuilder,

    call_stack: CallStack,
}

impl VirtualMachine {
    pub fn new(state: Container<MemphisState>, runtime: Container<Runtime>) -> Self {
        let module_loader = ModuleLoader::new(state.clone(), runtime.clone());
        let error_builder = ErrorBuilder::new(state.clone());
        let call_stack = CallStack::new(state.clone());

        Self {
            state,
            runtime,
            module_loader,
            executor: VmExecutor::default(),
            error_builder,
            call_stack,
        }
    }

    pub fn execute(&mut self, code: CodeObject) -> VmResult<VmValue> {
        self.load(code)?;
        self.run_loop()
    }

    pub fn read_global(&self, name: &str) -> Option<VmValue> {
        let reference = self.load_global_by_name(name).ok()?;
        self.deref(reference).ok()
    }

    /// Read a global variable from the `__main__` module.
    // TODO this should really only be available in test/repl mode, but we currently call this in
    // the Interpreter trait. The other option is splitting Interpreter into two traits and putting
    // the read one behind a test/repl flag.
    fn load_global_by_name(&self, name: &str) -> VmResult<Reference> {
        let module = self
            .runtime
            .borrow()
            .read_module(&Dunder::Main)
            .unwrap_or_else(|| panic!("Failed to read module: {}", Dunder::Main));

        let module_binding = module.borrow();
        Ok(module_binding
            .read(name)
            .unwrap_or_else(|| panic!("Failed to find var: {name}")))
    }

    pub fn current_module(&self) -> VmResult<Container<Module>> {
        Ok(self.current_frame()?.module.clone())
    }

    pub fn resolve_module(&self, name: &str) -> VmResult<Container<Module>> {
        self.runtime
            .borrow()
            .read_module(name)
            .ok_or_else(|| self.error_builder.runtime_error())
    }

    fn current_frame(&self) -> VmResult<&Frame> {
        self.call_stack.top_frame()
    }

    fn current_frame_mut(&mut self) -> VmResult<&mut Frame> {
        self.call_stack.top_frame_mut()
    }

    fn read_constant(&self, index: ConstantIndex) -> VmResult<Option<VmValue>> {
        Ok(self
            .current_frame()?
            .function
            .code_object
            .constants
            .get(*index)
            .map(|c| c.into()))
    }

    fn update_fn<F>(&mut self, obj_ref: Reference, function: F)
    where
        F: FnOnce(&mut VmValue),
    {
        if let Some(object_value) = self.runtime.borrow_mut().heap.get_mut(obj_ref) {
            function(object_value)
        }
    }

    fn store_global(&mut self, index: NonlocalIndex, value: Reference) -> VmResult<()> {
        let name = self.resolve_name(index)?;
        self.current_module()?.borrow_mut().write(name, value);
        Ok(())
    }

    fn store_local(&mut self, index: LocalIndex, value: Reference) -> VmResult<()> {
        let current_frame = self.current_frame_mut()?;
        if current_frame.locals.len() == *index {
            current_frame.locals.push(value);
        } else {
            current_frame.locals[*index] = value;
        }
        Ok(())
    }

    fn load_local(&self, index: LocalIndex) -> VmResult<Reference> {
        Ok(self.current_frame()?.locals[*index])
    }

    fn load_free(&self, index: FreeIndex) -> VmResult<Reference> {
        Ok(self.current_frame()?.function.freevars[*index])
    }

    fn load_global(&self, index: NonlocalIndex) -> VmResult<Reference> {
        let name = self.resolve_name(index)?;

        if let Some(val) = self.current_module()?.borrow().read(name) {
            return Ok(val);
        }

        if let Some(builtins_mod) = self.runtime.borrow().read_module(&Dunder::Builtins) {
            if let Some(val) = builtins_mod.borrow().read(name) {
                return Ok(val);
            }
        }

        Err(self.error_builder.name_error(name))
    }

    fn resolve_name(&self, index: NonlocalIndex) -> VmResult<&str> {
        Ok(&self.current_frame()?.function.code_object.names[*index])
    }

    fn peek(&mut self) -> VmResult<Reference> {
        let frame = self.current_frame_mut()?;

        if let Some(value) = frame.stack.last() {
            return Ok(*value);
        }

        Err(self.error_builder.runtime_error())
    }

    fn pop(&mut self) -> VmResult<Reference> {
        let frame = self.current_frame_mut()?;

        if let Some(value) = frame.stack.pop() {
            log_impure(LogLevel::Trace, || {
                println!("After pop:");
                self.dump_frame();
            });
            return Ok(value);
        }

        Err(self.error_builder.runtime_error())
    }

    fn push(&mut self, value: Reference) -> VmResult<()> {
        let frame = self.current_frame_mut()?;
        frame.stack.push(value);

        log_impure(LogLevel::Trace, || {
            println!("After push:");
            self.dump_frame();
        });

        Ok(())
    }

    fn dump_frame(&self) {
        let frame = self.current_frame().expect("No frame!");

        for (index, stack_var) in frame.stack.iter().rev().enumerate() {
            println!(
                "stack[{index}] = {}",
                stack_var.display_annotated(&self.runtime.borrow().heap)
            );
        }

        for (index, local) in frame.locals.iter().rev().enumerate() {
            println!(
                "local[{index}] = {}",
                local.display_annotated(&self.runtime.borrow().heap)
            );
        }
    }

    /// Extract primitives and resolve any references to a [`VmValue`]. All modifications should
    /// occur through VM instructions.
    pub fn deref(&self, reference: Reference) -> VmResult<VmValue> {
        let val = match reference {
            Reference::ObjectRef(_) => self
                .runtime
                .borrow()
                .heap
                .get(reference)
                .cloned()
                .ok_or_else(|| self.error_builder.runtime_error())?,
            // convert primitives directly
            _ => reference.into(),
        };

        Ok(val)
    }

    /// Resolves an attribute without applying method binding (used in tests or low-level access).
    pub fn resolve_raw_attr(&self, object: &VmValue, name: &str) -> VmResult<Reference> {
        if let Some(object) = object.as_object() {
            object
                .read(name, self)?
                .ok_or_else(|| self.error_builder.attribute_error(name))
        } else if let Some(module) = object.as_module() {
            module
                .borrow()
                .read(name)
                .ok_or_else(|| self.error_builder.attribute_error(name))
        } else {
            unimplemented!()
        }
    }

    /// Resolves an attribute and applies method binding if it is a function.
    pub fn resolve_attr(&mut self, object_ref: Reference, name: &str) -> VmResult<Reference> {
        let object = self.deref(object_ref)?;

        let attr_ref = self.resolve_raw_attr(&object, name)?;
        let attr_val = self.deref(attr_ref)?;

        let bound = match attr_val {
            VmValue::Function(f) => {
                self.heapify(VmValue::Method(Method::new(object_ref, f.clone())))
            }
            _ => attr_ref,
        };

        Ok(bound)
    }

    /// Primitives are stored inline on the stack, we create a reference to the global store for
    /// all other types.
    pub fn heapify(&mut self, value: VmValue) -> Reference {
        match value {
            // This case is only needed when we receive a generic `VmValue`, i.e. loading a
            // constant. For cases where we know we have a boolean, it is preferred to use
            // `to_heapified_bool` directly.
            VmValue::Bool(bool_val) => self.to_heapified_bool(bool_val),
            VmValue::Int(_) | VmValue::Float(_) => value.into_ref(),
            _ => self.runtime.borrow_mut().heap.allocate(value),
        }
    }

    pub fn none(&self) -> Reference {
        self.runtime.borrow().heap.none()
    }

    pub fn true_(&self) -> Reference {
        self.runtime.borrow().heap.true_()
    }

    pub fn false_(&self) -> Reference {
        self.runtime.borrow().heap.false_()
    }

    pub fn to_heapified_bool(&self, value: bool) -> Reference {
        if value {
            self.true_()
        } else {
            self.false_()
        }
    }

    /// Dereferences the top value on the stack.
    fn peek_value(&mut self) -> VmResult<VmValue> {
        let reference = self.peek()?;
        self.deref(reference)
    }

    /// Pops and dereferences a value.
    fn pop_value(&mut self) -> VmResult<VmValue> {
        let reference = self.pop()?;
        self.deref(reference)
    }

    fn push_value(&mut self, value: VmValue) -> VmResult<()> {
        let reference = self.heapify(value);
        self.push(reference)
    }

    /// Load and initialize a module, storing its reference in both the global scope and runtime
    /// module store.
    /// This ensures that newly created frames can resolve their originating module.
    fn load_module(&mut self, index: NonlocalIndex) -> VmResult<()> {
        let name = self.resolve_name(index)?.to_owned();

        let module = self.module_loader.resolve_module(&name)?;
        let module_ref = self.heapify(VmValue::Module(module.clone()));
        self.store_global(index, module_ref)?;

        Ok(())
    }

    fn collect_n(&mut self, n: usize) -> VmResult<Vec<Reference>> {
        let mut items = Vec::with_capacity(n);
        for _ in 0..n {
            items.push(self.pop()?);
        }
        // Reverse the items since we pop them off in reverse order
        items.reverse();
        Ok(items)
    }

    fn try_string_multiplication(a: &VmValue, b: &VmValue) -> Option<VmValue> {
        match (a, b) {
            (VmValue::String(s), VmValue::Int(n)) | (VmValue::Int(n), VmValue::String(s)) => {
                let result = if *n < 0 {
                    "".to_string()
                } else {
                    s.repeat(*n as usize)
                };
                Some(VmValue::String(result))
            }
            _ => None,
        }
    }

    fn binary_op<F>(&mut self, opcode: Opcode, op: F, force_float: bool) -> VmResult<()>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let b = self.pop_value()?;
        let a = self.pop_value()?;
        let result = if opcode == Opcode::Mul {
            if let Some(result) = Self::try_string_multiplication(&a, &b) {
                result
            } else {
                self.binary_numeric_op(op, &a, &b, force_float)?
            }
        } else {
            self.binary_numeric_op(op, &a, &b, force_float)?
        };

        self.push_value(result)?;
        Ok(())
    }

    fn cmp_op<F>(&mut self, op: F) -> VmResult<()>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        let b = self.pop_value()?;
        let a = self.pop_value()?;
        let result_ref = self.dynamic_cmp(&a, &b, op)?;
        self.push(result_ref)?;
        Ok(())
    }

    fn binary_numeric_op<F>(
        &mut self,
        op: F,
        a: &VmValue,
        b: &VmValue,
        force_float: bool,
    ) -> VmResult<VmValue>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let result = match (a, b) {
            (VmValue::Int(x), VmValue::Int(y)) => {
                let res = op(*x as f64, *y as f64);
                if force_float {
                    VmValue::Float(res)
                } else {
                    VmValue::Int(res as i64)
                }
            }
            (VmValue::Float(x), VmValue::Float(y)) => VmValue::Float(op(*x, *y)),
            (VmValue::Int(x), VmValue::Float(y)) => VmValue::Float(op(*x as f64, *y)),
            (VmValue::Float(x), VmValue::Int(y)) => VmValue::Float(op(*x, *y as f64)),
            _ => {
                return Err(self
                    .error_builder
                    .type_error("Unsupported operand types for binary operation"))
            }
        };

        Ok(result)
    }

    fn dynamic_cmp<F>(&self, a: &VmValue, b: &VmValue, op: F) -> VmResult<Reference>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        let result = match (a, b) {
            (VmValue::Int(x), VmValue::Int(y)) => op(*x as f64, *y as f64),
            (VmValue::Float(x), VmValue::Float(y)) => op(*x, *y),
            (VmValue::Int(x), VmValue::Float(y)) => op(*x as f64, *y),
            (VmValue::Float(x), VmValue::Int(y)) => op(*x, *y as f64),
            _ => {
                return Err(self
                    .error_builder
                    .type_error("Unsupported operand types for comparison"))
            }
        };

        Ok(self.to_heapified_bool(result))
    }

    fn dynamic_negate(&self, value: &VmValue) -> VmResult<VmValue> {
        let result = match value {
            VmValue::Int(x) => VmValue::Int(-x),
            VmValue::Float(x) => VmValue::Float(-x),
            _ => {
                return Err(self
                    .error_builder
                    .type_error("Unsupported operand type for unary '-'"))
            }
        };

        Ok(result)
    }

    fn value_in_iter(&mut self, needle: VmValue, haystack: VmValue) -> VmResult<bool> {
        let iter = builtins::iter_internal(self, haystack)?;
        loop {
            match builtins::next_internal(self, iter)? {
                Some(item_ref) => {
                    if needle == self.deref(item_ref)? {
                        return Ok(true);
                    }
                }
                None => return Ok(false),
            }
        }
    }

    // === Declarative VM Call Interface ===

    /// Push a new `Frame` to the call stack and immediately execute it to completion, returning
    /// its return value.
    pub fn call(&mut self, frame: Frame) -> VmResult<Reference> {
        let (step_result, _frame) = self.run_frame(frame)?;
        Ok(unwrap_return_value(step_result))
    }

    /// Push a new `Frame` to the call stack and immediately execute it to completion, returning
    /// the frame. Useful for class definitions.
    pub fn call_and_return_frame(&mut self, frame: Frame) -> VmResult<Frame> {
        let (step_result, frame) = self.run_frame(frame)?;
        match step_result {
            StepResult::Return(val) => assert_eq!(
                val,
                self.none(),
                "`call_and_return_frame` expects the frame to return None."
            ),
            other => panic!("Unexpected step result in `call_and_return_frame`: {other:?}"),
        }
        Ok(frame)
    }

    pub fn resume_generator(
        &mut self,
        generator: Container<Generator>,
    ) -> VmResult<Option<Reference>> {
        let frame = generator.borrow_mut().frame.clone();
        let (step_result, new_frame) = self.run_frame(frame)?;
        let return_val = match step_result {
            StepResult::Yield(val) => Some(val),
            StepResult::Return(_) => None,
            StepResult::Sleep(_) | StepResult::Await(_) => {
                panic!("Async generators are not currently supported.")
            }
            other => panic!("Unexpected step result in `resume_generator`: {other:?}"),
        };
        generator.borrow_mut().frame = new_frame;

        Ok(return_val)
    }

    pub fn step_coroutine(&mut self, coroutine: Container<Coroutine>) -> VmResult<StepResult> {
        let (step_result, new_frame) = self.run_frame(coroutine.borrow().frame.clone())?;
        coroutine.borrow_mut().frame = new_frame;
        Ok(step_result)
    }

    /// Push a frame and run it, capturing the result and returning the frame.
    /// We need to capture the frame when it is finished for creating new Classes and for saving
    /// the state of a Coroutine.
    fn run_frame(&mut self, frame: Frame) -> VmResult<(StepResult, Frame)> {
        self.call_stack.push(frame);
        self.run_top_frame()
    }

    // === Internal Execution Flow ===

    /// Executes all frames in the call stack to completion.
    /// This is the main loop of the VM.
    fn run_loop(&mut self) -> VmResult<VmValue> {
        let mut result = self.none();
        while !self.call_stack.is_finished() {
            let (step_result, _frame) = self.run_top_frame()?;
            result = match step_result {
                StepResult::Return(val) => val,
                StepResult::Halt => self.none(),
                other => panic!("Unexpected step result in `run_loop`: {other:?}"),
            };
        }

        self.deref(result)
    }

    /// Run the top frame in the call stack to completion and then return.
    fn run_top_frame(&mut self) -> VmResult<(StepResult, Frame)> {
        while self.call_stack.top().is_some_and(|f| !f.is_finished()) {
            let result = self.step_frame()?;
            match result {
                StepResult::Continue => continue,
                StepResult::Return(val) => {
                    let frame = self
                        .call_stack
                        .pop()
                        .ok_or_else(|| self.error_builder.runtime_error())?;
                    return Ok((StepResult::Return(val), frame));
                }
                StepResult::Await(_) | StepResult::Sleep(_) | StepResult::Yield(_) => {
                    let frame = self
                        .call_stack
                        .pop()
                        .ok_or_else(|| self.error_builder.runtime_error())?;
                    return Ok((result, frame));
                }
                StepResult::Halt => {
                    let frame = self
                        .call_stack
                        .pop()
                        .ok_or_else(|| self.error_builder.runtime_error())?;
                    return Ok((StepResult::Halt, frame));
                }
            }
        }

        // If we fell out of the loop: frame is finished with no explicit return
        let frame = self
            .call_stack
            .pop()
            .ok_or_else(|| self.error_builder.runtime_error())?;
        Ok((StepResult::Return(self.none()), frame))
    }

    /// Run the next instruction on the top frame in the call stack.
    fn step_frame(&mut self) -> VmResult<StepResult> {
        let frame = self.current_frame()?;

        // Save this in case we encounter a runtime exception and need to record this info in
        // the stack trace
        self.state.set_line_number(frame.current_line());
        let opcode = frame.current_inst();

        log_impure(LogLevel::Debug, || self.dump_frame());
        log(LogLevel::Debug, || frame.current_inst_annotated());

        match opcode {
            Opcode::Add => {
                self.binary_op(opcode, |a, b| a + b, false).map_err(|_| {
                    self.error_builder
                        .type_error("Unsupported operand types for +")
                })?;
            }
            Opcode::Sub => {
                self.binary_op(opcode, |a, b| a - b, false).map_err(|_| {
                    self.error_builder
                        .type_error("Unsupported operand types for -")
                })?;
            }
            Opcode::Mul => {
                self.binary_op(opcode, |a, b| a * b, false).map_err(|_| {
                    self.error_builder
                        .type_error("Unsupported operand types for *")
                })?;
            }
            Opcode::Div => {
                self.binary_op(opcode, |a, b| a / b, true).map_err(|_| {
                    self.error_builder
                        .type_error("Unsupported operand types for /")
                })?;
            }
            Opcode::Eq => {
                let right = self.pop_value()?;
                let left = self.pop_value()?;
                self.push(self.to_heapified_bool(left == right))?;
            }
            Opcode::LessThan => {
                self.cmp_op(|a, b| a < b).map_err(|_| {
                    self.error_builder
                        .type_error("Unsupported operand types for <")
                })?;
            }
            Opcode::LessThanOrEq => {
                self.cmp_op(|a, b| a <= b).map_err(|_| {
                    self.error_builder
                        .type_error("Unsupported operand types for <=")
                })?;
            }
            Opcode::GreaterThan => {
                self.cmp_op(|a, b| a > b).map_err(|_| {
                    self.error_builder
                        .type_error("Unsupported operand types for >")
                })?;
            }
            Opcode::GreaterThanOrEq => {
                self.cmp_op(|a, b| a >= b).map_err(|_| {
                    self.error_builder
                        .type_error("Unsupported operand types for >=")
                })?;
            }
            Opcode::In => {
                let haystack = self.pop_value()?;
                let needle = self.pop_value()?;

                let in_result = self.value_in_iter(needle, haystack)?;
                self.push(self.to_heapified_bool(in_result))?;
            }
            Opcode::NotIn => {
                let haystack = self.pop_value()?;
                let needle = self.pop_value()?;

                let in_result = !self.value_in_iter(needle, haystack)?;
                self.push(self.to_heapified_bool(in_result))?;
            }
            Opcode::UnaryNegative => {
                let value = self.pop_value()?;
                let result = self.dynamic_negate(&value)?;
                self.push_value(result)?;
            }
            Opcode::UnaryNot => {
                let right = self.pop_value()?.to_boolean();
                self.push(self.to_heapified_bool(!right))?;
            }
            Opcode::UnaryInvert => {
                let right = self.pop_value()?.as_integer().ok_or_else(|| {
                    self.error_builder
                        .type_error("Unsupported operand type for '~'")
                })?;
                self.push(Reference::Int(!right))?;
            }
            Opcode::LoadConst(index) => {
                // After loading a constant for the first time, it becomes an object managed by
                // the heap like any other object.
                let value = self
                    .read_constant(index)?
                    .ok_or_else(|| self.error_builder.runtime_error())?;
                self.push_value(value)?;
            }
            Opcode::StoreFast(index) => {
                let reference = self.pop()?;
                self.store_local(index, reference)?;
            }
            Opcode::StoreGlobal(index) => {
                let reference = self.pop()?;
                self.store_global(index, reference)?;
            }
            Opcode::LoadFast(index) => {
                let reference = self.load_local(index)?;
                self.push(reference)?;
            }
            Opcode::LoadFree(index) => {
                let reference = self.load_free(index)?;
                self.push(reference)?;
            }
            Opcode::LoadGlobal(index) => {
                let reference = self.load_global(index)?;
                self.push(reference)?;
            }
            Opcode::LoadAttr(index) => {
                let attr_name = self.resolve_name(index)?.to_owned();
                let object_ref = self.pop()?;

                let bound_attr = self.resolve_attr(object_ref, &attr_name)?;
                self.push(bound_attr)?;
            }
            Opcode::SetAttr(index) => {
                let value = self.pop()?;
                let obj_ref = self.pop()?;

                let name = self.resolve_name(index)?.to_owned();
                self.update_fn(obj_ref, |object_value| {
                    let VmValue::Object(object) = object_value else {
                        todo!()
                    };
                    object.write(&name, value);
                });
            }
            Opcode::LoadBuildClass => {
                self.push_value(VmValue::BuiltinFunction(BuiltinFunction::new(
                    "load_build_class",
                    builtins::build_class,
                )))?;
            }
            Opcode::BuildTuple(n) => {
                let items = self.collect_n(n)?;
                self.push_value(VmValue::Tuple(Tuple::new(items)))?;
            }
            Opcode::BuildList(n) => {
                let items = self.collect_n(n)?;
                self.push_value(VmValue::List(List::new(items)))?;
            }
            Opcode::GetIter => {
                let obj = self.pop_value()?;
                let iterator_ref = builtins::iter_internal(self, obj)?;
                self.push(iterator_ref)?;
            }
            Opcode::ForIter(offset) => {
                // Don’t pop, we need the iterator on the stack for the next iteration
                let iter_ref = self.peek()?;
                let next_ref = builtins::next_internal(self, iter_ref)?;

                if let Some(next_ref) = next_ref {
                    // Iterator stays, value now lives above it
                    self.push(next_ref)?;
                } else {
                    // Pop the iterator only if exhausted
                    let _ = self.pop()?;
                    self.call_stack.jump_to_offset(offset)?;
                }
            }
            Opcode::Jump(offset) => {
                self.call_stack.jump_to_offset(offset)?;
            }
            Opcode::JumpIfFalse(offset) => {
                if !self.peek_value()?.to_boolean() {
                    self.call_stack.jump_to_offset(offset)?;
                }
            }
            Opcode::JumpIfTrue(offset) => {
                if self.peek_value()?.to_boolean() {
                    self.call_stack.jump_to_offset(offset)?;
                }
            }
            Opcode::PopTop => {
                let _ = self.pop()?;
            }
            Opcode::MakeFunction => {
                let code_value = self.pop_value()?;
                let code = code_value.expect_code(self)?;
                let function = FunctionObject::new(code.clone());
                self.push_value(VmValue::Function(function))?;
            }
            Opcode::MakeClosure(num_free) => {
                let freevars = (0..num_free)
                    .map(|_| self.pop())
                    .collect::<Result<Vec<_>, _>>()?;
                let code_value = self.pop_value()?;
                let code = code_value.expect_code(self)?;
                let function = FunctionObject::new_with_free(code.clone(), freevars);
                self.push_value(VmValue::Function(function))?;
            }
            Opcode::Call(argc) => {
                let args = (0..argc)
                    .map(|_| self.pop())
                    .collect::<Result<Vec<_>, _>>()?;
                let callable_ref = self.pop()?;
                let callable = self.deref(callable_ref)?;

                match callable {
                    VmValue::BuiltinFunction(builtin) => {
                        let reference = builtin.call(self, args)?;
                        self.push(reference)?;
                    }
                    VmValue::Function(ref function) => {
                        let frame = Frame::from_function(self, function.clone(), args)?;
                        match function.function_type() {
                            FunctionType::Regular => {
                                let return_val_ref = self.call(frame)?;
                                self.push(return_val_ref)?;
                            }
                            FunctionType::Generator => {
                                let generator = Container::new(Generator::new(frame));
                                self.push_value(VmValue::Generator(generator))?
                            }
                            FunctionType::Async => {
                                let coroutine = Container::new(Coroutine::new(frame));
                                self.push_value(VmValue::Coroutine(coroutine))?;
                            }
                        }
                    }
                    VmValue::Method(ref method) => {
                        let frame = Frame::from_method(self, method.clone(), args)?;
                        let return_val_ref = self.call(frame)?;
                        self.push(return_val_ref)?;
                    }
                    VmValue::Class(ref class) => {
                        let object = VmValue::Object(Object::new(callable_ref));
                        let reference = self.heapify(object);

                        if let Some(init_method) = class.read(Dunder::Init) {
                            let init_value = self.deref(init_method)?;
                            let init_fn = init_value.expect_function(self)?;
                            let method = Method::new(reference, init_fn.clone());

                            // The object reference must be on the stack for
                            // after the constructor executes.
                            self.push(reference)?;

                            let frame = Frame::from_method(self, method, args)?;
                            let _ = self.call(frame)?;
                        } else {
                            self.push(reference)?;
                        }
                    }
                    _ => unimplemented!(),
                };
            }
            Opcode::ReturnValue => {
                let return_val_ref = self.pop()?;
                return Ok(StepResult::Return(return_val_ref));
            }
            Opcode::YieldValue => {
                let yield_val_ref = self.pop()?;
                self.call_stack.advance_pc()?;
                return Ok(StepResult::Yield(yield_val_ref));
            }
            Opcode::YieldFrom => {
                if !self.current_frame()?.has_subgenerator() {
                    // First time hitting this instruction: pop the iterable and store it
                    let iterable = self.pop_value()?;
                    let iterator_ref = builtins::iter_internal(self, iterable)?;
                    self.current_frame_mut()?.set_subgenerator(iterator_ref);
                }

                // Extract iterator_ref in a separate scope to avoid borrow overlap
                let iterator_ref = match self.current_frame()?.subgenerator_ref() {
                    Some(r) => r,
                    None => unreachable!("YieldFrom without a sub-generator"),
                };

                // Actually try the next() call
                match builtins::next_internal(self, iterator_ref)? {
                    Some(val) => {
                        return Ok(StepResult::Yield(val)); // yield and don't advance PC
                    }
                    None => {
                        // Sub-generator is done, clean up and continue
                        self.current_frame_mut()?.clear_subgenerator();
                        self.call_stack.advance_pc()?; // advance past YieldFrom
                        return Ok(StepResult::Continue);
                    }
                }
            }
            Opcode::Await => {
                let value = self.pop_value()?;

                self.call_stack.advance_pc()?;
                match value {
                    VmValue::SleepFuture(duration) => {
                        return Ok(StepResult::Sleep(duration));
                    }
                    VmValue::Coroutine(co) => {
                        return Ok(StepResult::Await(co.clone()));
                    }
                    _ => {
                        return Err(self.error_builder.type_error("Expected awaitable"));
                    }
                }
            }
            Opcode::ImportName(index) => {
                self.load_module(index)?;
            }
            Opcode::Halt => {
                return Ok(StepResult::Halt);
            }
            // This is in an internal error that indicates a jump offset was not properly set
            // by the compiler. This opcode should not leak into the VM.
            Opcode::Placeholder => return Err(self.error_builder.runtime_error()),
        }

        // Increment PC for all instructions.
        self.call_stack.advance_pc()?;
        Ok(StepResult::Continue)
    }

    fn load(&mut self, code: CodeObject) -> VmResult<()> {
        log(LogLevel::Debug, || format!("{code:?}"));

        let function = FunctionObject::new(code);
        let frame = Frame::from_function(self, function, vec![])?;

        self.call_stack.push(frame);
        Ok(())
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self::new(
            Container::new(MemphisState::default()),
            Container::new(Runtime::default()),
        )
    }
}

/// Expects the frame to return a value. Panics if the result was not `StepResult::Return`.
fn unwrap_return_value(step_result: StepResult) -> Reference {
    match step_result {
        StepResult::Return(val) => val,
        other => panic!("Expected StepResult::Return, got {other:?}"),
    }
}

#[cfg(test)]
/// These test conditions related to the VM's internal state.
mod tests {
    use super::*;

    use crate::bytecode_vm::test_utils::*;

    #[test]
    /// We're testing for basic memory-efficiency here. The original implementation created
    /// unnecessary copies of the object.
    fn object_store_duplicates() {
        let text = r#"
class Foo:
    def __init__(self):
        self.x = 44

f = Foo()
"#;
        let ctx = run(text);
        let runtime = ctx.interpreter().vm().runtime.borrow();
        let objects: Vec<&VmValue> = runtime
            .heap
            .iter()
            .filter(|object| matches!(object, VmValue::Object(_)))
            .collect();
        assert_eq!(objects.len(), 1);
    }
}
