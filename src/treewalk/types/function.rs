use std::{
    any::Any,
    fmt::{Display, Error, Formatter},
};

use crate::{
    core::{log, Container, LogLevel},
    domain::{DebugStackFrame, Dunder, ToDebugStackFrame},
    parser::{
        static_analysis::{FunctionAnalysisVisitor, YieldDetector},
        types::{Ast, Closure, Expr, Params},
    },
    resolved_args,
    treewalk::{interpreter::TreewalkResult, Interpreter, Scope, TreewalkState},
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

#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    pub args: Params,
    pub body: Ast,
    pub module: Container<Module>,
    pub class_context: Option<Container<Class>>,
    line_number: usize,
    decorators: Vec<Expr>,
    is_async: bool,
    pub captured_env: Container<EnvironmentFrame>,
    scope: Scope,
    closure: Closure,
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
        state: Container<TreewalkState>,
        name: &str,
        args: Params,
        body: Ast,
        decorators: &[Expr],
        is_async: bool,
        line_number: usize,
    ) -> Self {
        let module = state.current_module();
        let class_context = state.current_class();
        let captured_env = state.get_environment_frame();

        let mut visitor = FunctionAnalysisVisitor::new();
        body.accept(&mut visitor);

        Self {
            name: name.to_string(),
            args,
            body,
            module,
            class_context,
            line_number,
            decorators: decorators.to_vec(),
            is_async,
            captured_env,
            scope: Scope::default(),
            closure: visitor.into(),
        }
    }

    pub fn new_lambda(state: Container<TreewalkState>, args: Params, body: Ast) -> Self {
        // TODO add line number
        Self::new(state, "<lambda>", args, body, &[], false, 1)
    }

    pub fn new_anonymous_generator(state: Container<TreewalkState>, body: Ast) -> Self {
        // TODO add line number
        Self::new(
            state,
            "<anonymous_generator>",
            Params::default(),
            body,
            &[],
            false,
            1,
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
        let free_vars = self.closure.free_vars();

        if free_vars.is_empty() {
            return ExprResult::None;
        }

        let mut items = vec![];
        for key in free_vars {
            let value = self.captured_env.borrow().read(key.as_str()).unwrap();
            items.push(ExprResult::Cell(Container::new(Cell::new(value))));
        }

        ExprResult::Tuple(Tuple::new(items))
    }
}

impl MemberReader for Container<Function> {
    /// This is really the same logic as in Container<Object>::get_member. Maybe we can combine
    /// those at some point.
    fn get_member(
        &self,
        interpreter: &Interpreter,
        name: &str,
    ) -> TreewalkResult<Option<ExprResult>> {
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
    ) -> TreewalkResult<()> {
        self.borrow_mut().scope.insert(name, value);
        Ok(())
    }

    fn delete_member(&mut self, _interpreter: &Interpreter, name: &str) -> TreewalkResult<()> {
        self.borrow_mut().scope.delete(name);
        Ok(())
    }
}

impl Container<Function> {
    pub fn apply_decorators(&self, interpreter: &Interpreter) -> TreewalkResult<ExprResult> {
        let mut result = ExprResult::Function(self.clone());
        if self.borrow().decorators.is_empty() {
            return Ok(result);
        }

        let decorators = self.borrow().decorators.clone();
        for decorator in decorators.iter() {
            let function = interpreter
                .evaluate_expr(decorator)?
                .expect_callable(interpreter)?;
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
    ) -> TreewalkResult<ExprResult> {
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
    ) -> TreewalkResult<ExprResult> {
        Ok(match instance {
            Some(instance) => instance
                .expect_function(interpreter)?
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
    ) -> TreewalkResult<ExprResult> {
        Ok(match instance {
            Some(instance) => instance.expect_function(interpreter)?.borrow().get_code(),
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
    ) -> TreewalkResult<()> {
        todo!();
    }

    fn delete_attr(&self, _interpreter: &Interpreter, _instance: ExprResult) -> TreewalkResult<()> {
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
    ) -> TreewalkResult<ExprResult> {
        Ok(match instance {
            Some(instance) => instance
                .expect_function(interpreter)?
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
    ) -> TreewalkResult<ExprResult> {
        Ok(match instance {
            Some(instance) => {
                let name = instance
                    .expect_function(interpreter)?
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
    ) -> TreewalkResult<ExprResult> {
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
    ) -> TreewalkResult<ExprResult> {
        Ok(match instance {
            Some(instance) => {
                let name = instance.expect_function(interpreter)?.borrow().name.clone();
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
    ) -> TreewalkResult<ExprResult> {
        Ok(match instance {
            Some(instance) => {
                let name = instance.expect_function(interpreter)?.borrow().name.clone();
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
    ) -> TreewalkResult<ExprResult> {
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
    ) -> TreewalkResult<ExprResult> {
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
    ) -> TreewalkResult<ExprResult> {
        let scope = match instance {
            Some(i) => i.expect_function(interpreter)?.borrow().scope.clone(),
            None => owner.borrow().scope.clone(),
        };
        Ok(ExprResult::Dict(scope.as_dict(interpreter)))
    }

    fn name(&self) -> String {
        Dunder::Dict.into()
    }
}
