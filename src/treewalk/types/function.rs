use std::{
    any::Any,
    fmt::{Display, Error, Formatter},
};

use crate::{
    core::{log, Container, LogLevel},
    domain::{DebugStackFrame, Dunder, ToDebugStackFrame},
    parser::{
        static_analysis::{FunctionAnalysisVisitor, YieldDetector},
        types::{Ast, Closure, Expr, ParsedArgDefinitions},
    },
    resolved_args,
    treewalk::{Interpreter, Scope, State},
    types::errors::InterpreterError,
};

use super::{
    domain::{
        traits::{
            Callable, DataDescriptor, DescriptorProvider, MemberReader, MemberWriter,
            NonDataDescriptor, Typed,
        },
        Type,
    },
    utils::{EnvironmentFrame, ResolvedArguments},
    Cell, Class, Dict, ExprResult, Module, Str, Tuple,
};

/// How we evaluate a [`Function`] depends on whether it is async or a generator or a
/// traditional function.
pub enum FunctionType {
    Regular,
    Generator,
    Async,
}

/// This is a placeholder for what is calcuated on a functions [`Dunder::Code`].
/// TODO this is a stub, we may need to flesh this out with bytecode if we ever want to support
/// self-modifying code or whatever this is used for.
#[derive(Clone)]
pub struct Code;

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub args: ParsedArgDefinitions,
    pub body: Ast,
    pub module: Container<Module>,
    pub class_context: Option<Container<Class>>,
    pub line_number: usize,
    pub decorators: Vec<Expr>,
    pub is_async: bool,
    pub captured_env: Container<EnvironmentFrame>,
    pub scope: Scope,
    pub closure: Closure,
}

impl Typed for Function {
    fn get_type() -> Type {
        Type::Function
    }
}

impl DescriptorProvider for Function {
    fn get_descriptors() -> Vec<Box<dyn NonDataDescriptor>> {
        vec![
            Box::new(CodeAttribute),
            Box::new(DictDescriptor),
            Box::new(GlobalsAttribute),
            Box::new(ClosureAttribute),
            Box::new(ModuleAttribute),
            Box::new(DocAttribute),
            Box::new(NameAttribute),
            Box::new(QualnameAttribute),
            Box::new(AnnotationsAttribute),
            Box::new(TypeParamsAttribute),
        ]
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.module == other.module
    }
}

impl ToDebugStackFrame for Function {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new(
            self.name(),
            self.module.borrow().path().to_path_buf(),
            self.line_number(),
        )
    }
}

impl Function {
    pub fn new(
        state: Container<State>,
        name: String,
        args: ParsedArgDefinitions,
        body: Ast,
        decorators: Vec<Expr>,
        is_async: bool,
    ) -> Self {
        let module = state.current_module();
        let class_context = state.current_class();
        let line_number = state.call_stack().line_number();
        let captured_env = state.get_environment_frame();

        let mut visitor = FunctionAnalysisVisitor::new();
        body.accept(&mut visitor);

        Self {
            name,
            args,
            body,
            module,
            class_context,
            line_number,
            decorators,
            is_async,
            captured_env,
            scope: Scope::default(),
            closure: visitor.into(),
        }
    }

    pub fn new_lambda(state: Container<State>, args: ParsedArgDefinitions, body: Ast) -> Self {
        Self::new(state, "<lambda>".into(), args, body, vec![], false)
    }

    pub fn new_anonymous_generator(state: Container<State>, body: Ast) -> Self {
        Self::new(
            state,
            "<anonymous_generator>".into(),
            ParsedArgDefinitions::default(),
            body,
            vec![],
            false,
        )
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn is_generator(&self) -> bool {
        let mut detector = YieldDetector::new();
        self.body.accept(&mut detector);
        detector.found_yield
    }

    fn get_globals(&self) -> ExprResult {
        ExprResult::Dict(Container::new(Dict::default()))
    }

    fn get_code(&self) -> ExprResult {
        ExprResult::Code(Container::new(Code))
    }

    fn get_closure(&self) -> ExprResult {
        let mut items = vec![];
        for key in self.closure.get_free_vars() {
            let value = self.captured_env.borrow().read(key.as_str()).unwrap();
            items.push(ExprResult::Cell(Container::new(Cell::new(value))));
        }

        match items.is_empty() {
            true => ExprResult::None,
            false => ExprResult::Tuple(Tuple::new(items)),
        }
    }
}

impl MemberReader for Container<Function> {
    /// This is really the same logic as in Container<Object>::get_member. Maybe we can combine
    /// those at some point.
    fn get_member(
        &self,
        interpreter: &Interpreter,
        name: &str,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        log(LogLevel::Debug, || {
            format!("Searching for: {}.{}", self, name)
        });

        if let Some(attr) = self.borrow().scope.get(name) {
            log(LogLevel::Debug, || format!("Found: {}.{}", self, name));
            return Ok(Some(attr));
        }

        let class = interpreter.state.get_type_class(Type::Function);

        if let Some(attr) = class.get_from_class(name) {
            log(LogLevel::Debug, || format!("Found: {}::{}", class, name));
            let instance = ExprResult::Function(self.clone());
            let owner = instance.get_class(interpreter);
            return Ok(Some(attr.resolve_nondata_descriptor(
                interpreter,
                Some(instance),
                owner,
            )?));
        }

        Ok(None)
    }
}

impl MemberWriter for Container<Function> {
    fn set_member(
        &mut self,
        _interpreter: &Interpreter,
        name: &str,
        value: ExprResult,
    ) -> Result<(), InterpreterError> {
        self.borrow_mut().scope.insert(name, value);
        Ok(())
    }

    fn delete_member(
        &mut self,
        _interpreter: &Interpreter,
        name: &str,
    ) -> Result<(), InterpreterError> {
        self.borrow_mut().scope.delete(name);
        Ok(())
    }
}

impl Container<Function> {
    pub fn apply_decorators(
        &self,
        interpreter: &Interpreter,
    ) -> Result<ExprResult, InterpreterError> {
        let mut result = ExprResult::Function(self.clone());
        if self.borrow().decorators.is_empty() {
            return Ok(result);
        }

        let decorators = self.borrow().decorators.clone();
        for decorator in decorators.iter() {
            let decorator_result = interpreter.evaluate_expr(decorator)?;

            let function =
                decorator_result
                    .as_callable()
                    .ok_or(InterpreterError::ExpectedFunction(
                        interpreter.state.call_stack(),
                    ))?;

            result = interpreter.call(function, &resolved_args![result])?;
        }

        Ok(result)
    }
}

impl Callable for Container<Function> {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        let scope = Scope::new(interpreter, self, &args)?;
        interpreter.invoke_function(self.clone(), scope)
    }

    fn name(&self) -> String {
        self.borrow().name.clone()
    }

    fn function_type(&self) -> FunctionType {
        if self.borrow().is_async {
            FunctionType::Async
        } else if self.borrow().is_generator() {
            FunctionType::Generator
        } else {
            FunctionType::Regular
        }
    }

    fn as_any(&self) -> &dyn Any {
        // returning a reference to self, not self directly. This is required so that there is a
        // known size at compile-time.
        self
    }
}

impl Display for Container<Function> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<function {} at {:p}>", self.borrow().name, self)
    }
}

#[derive(Clone)]
struct ClosureAttribute;

impl NonDataDescriptor for ClosureAttribute {
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(match instance {
            Some(instance) => instance
                .as_function()
                .ok_or(InterpreterError::ExpectedFunction(
                    interpreter.state.call_stack(),
                ))?
                .borrow()
                .get_closure(),
            None => ExprResult::NonDataDescriptor(Container::new(Box::new(self.clone()))),
        })
    }

    fn name(&self) -> String {
        Dunder::Closure.into()
    }
}

#[derive(Clone)]
struct CodeAttribute;

impl NonDataDescriptor for CodeAttribute {
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(match instance {
            Some(instance) => instance
                .as_function()
                .ok_or(InterpreterError::ExpectedFunction(
                    interpreter.state.call_stack(),
                ))?
                .borrow()
                .get_code(),
            None => ExprResult::DataDescriptor(Container::new(Box::new(self.clone()))),
        })
    }

    fn name(&self) -> String {
        Dunder::Code.into()
    }
}

impl DataDescriptor for CodeAttribute {
    fn set_attr(
        &self,
        _interpreter: &Interpreter,
        _instance: ExprResult,
        _value: ExprResult,
    ) -> Result<(), InterpreterError> {
        todo!();
    }

    fn delete_attr(
        &self,
        _interpreter: &Interpreter,
        _instance: ExprResult,
    ) -> Result<(), InterpreterError> {
        todo!();
    }
}

#[derive(Clone)]
struct GlobalsAttribute;

impl NonDataDescriptor for GlobalsAttribute {
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(match instance {
            Some(instance) => instance
                .as_function()
                .ok_or(InterpreterError::ExpectedFunction(
                    interpreter.state.call_stack(),
                ))?
                .borrow()
                .get_globals(),
            None => ExprResult::NonDataDescriptor(Container::new(Box::new(self.clone()))),
        })
    }

    fn name(&self) -> String {
        Dunder::Globals.into()
    }
}

#[derive(Clone)]
struct ModuleAttribute;

impl NonDataDescriptor for ModuleAttribute {
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(match instance {
            Some(instance) => {
                let name = instance
                    .as_function()
                    .ok_or(InterpreterError::ExpectedFunction(
                        interpreter.state.call_stack(),
                    ))?
                    .borrow()
                    .module
                    .borrow()
                    .name()
                    .to_string();
                ExprResult::String(Str::new(name))
            }
            None => ExprResult::NonDataDescriptor(Container::new(Box::new(self.clone()))),
        })
    }

    fn name(&self) -> String {
        Dunder::Module.into()
    }
}

#[derive(Clone)]
struct DocAttribute;

impl NonDataDescriptor for DocAttribute {
    fn get_attr(
        &self,
        _interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(match instance {
            // TODO store doc strings
            Some(_) => ExprResult::String(Str::new("".into())),
            None => ExprResult::NonDataDescriptor(Container::new(Box::new(self.clone()))),
        })
    }

    fn name(&self) -> String {
        Dunder::Doc.into()
    }
}

#[derive(Clone)]
struct NameAttribute;

impl NonDataDescriptor for NameAttribute {
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(match instance {
            Some(instance) => {
                let name = instance
                    .as_function()
                    .ok_or(InterpreterError::ExpectedFunction(
                        interpreter.state.call_stack(),
                    ))?
                    .borrow()
                    .name
                    .clone();
                ExprResult::String(Str::new(name))
            }
            None => ExprResult::NonDataDescriptor(Container::new(Box::new(self.clone()))),
        })
    }

    fn name(&self) -> String {
        Dunder::Name.into()
    }
}

#[derive(Clone)]
struct QualnameAttribute;

impl NonDataDescriptor for QualnameAttribute {
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(match instance {
            Some(instance) => {
                let name = instance
                    .as_function()
                    .ok_or(InterpreterError::ExpectedFunction(
                        interpreter.state.call_stack(),
                    ))?
                    .borrow()
                    .name
                    .clone();
                ExprResult::String(Str::new(name))
            }
            None => ExprResult::NonDataDescriptor(Container::new(Box::new(self.clone()))),
        })
    }

    fn name(&self) -> String {
        Dunder::Qualname.into()
    }
}

#[derive(Clone)]
struct AnnotationsAttribute;

impl NonDataDescriptor for AnnotationsAttribute {
    fn get_attr(
        &self,
        _interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(match instance {
            Some(_) => ExprResult::Dict(Container::new(Dict::default())),
            None => ExprResult::NonDataDescriptor(Container::new(Box::new(self.clone()))),
        })
    }

    fn name(&self) -> String {
        Dunder::Annotations.into()
    }
}

#[derive(Clone)]
struct TypeParamsAttribute;

impl NonDataDescriptor for TypeParamsAttribute {
    fn get_attr(
        &self,
        _interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(match instance {
            Some(_) => ExprResult::Tuple(Tuple::default()),
            None => ExprResult::NonDataDescriptor(Container::new(Box::new(self.clone()))),
        })
    }

    fn name(&self) -> String {
        Dunder::TypeParams.into()
    }
}

#[derive(Clone)]
struct DictDescriptor;

/// This is really the same logic as in Container<Object>::get_attr. Maybe we can combine those at
/// some point.
impl NonDataDescriptor for DictDescriptor {
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        let scope = match instance {
            Some(i) => i
                .as_function()
                .ok_or(InterpreterError::ExpectedObject(
                    interpreter.state.call_stack(),
                ))?
                .borrow()
                .scope
                .clone(),
            None => owner.borrow().scope.clone(),
        };
        Ok(ExprResult::Dict(scope.as_dict(interpreter)))
    }

    fn name(&self) -> String {
        Dunder::Dict.into()
    }
}
