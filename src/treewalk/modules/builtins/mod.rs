use crate::{
    core::Container,
    domain::{Dunder, Source},
    treewalk::{
        protocols::{Callable, Iterable},
        result::{ExecResult, Raise},
        type_system::CloneableCallable,
        types::{List, Module, Str},
        utils::{args, check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue, TypeRegistry,
    },
};

fn get_builtins() -> Vec<Box<dyn CloneableCallable>> {
    vec![
        Box::new(CallableBuiltin),
        Box::new(DirBuiltin),
        Box::new(GetattrBuiltin),
        Box::new(GlobalsBuiltin),
        Box::new(HashBuiltin),
        Box::new(IsinstanceBuiltin),
        Box::new(IssubclassBuiltin),
        Box::new(IterBuiltin),
        Box::new(LenBuiltin),
        Box::new(NextBuiltin),
        Box::new(PrintBuiltin),
    ]
}

pub fn init(registry: &TypeRegistry) -> Module {
    let mut mod_ = Module::new(Source::default());
    for builtin in get_builtins() {
        mod_.insert(&builtin.name(), TreewalkValue::BuiltinFunction(builtin));
    }

    // This is to insert `list()`, `set()`, etc into the builtin scope. We must do it here instead
    // of in `init_builtin_scope()` because we want to use the singleton instances owned by
    // `TypeRegistry`.
    for builtin_class in registry.get_callable_builtin_types() {
        mod_.insert(
            builtin_class.borrow().name(),
            TreewalkValue::Class(builtin_class.clone()),
        );
    }

    mod_
}

#[derive(Clone)]
pub struct CallableBuiltin;
#[derive(Clone)]
pub struct DirBuiltin;
#[derive(Clone)]
pub struct GetattrBuiltin;
#[derive(Clone)]
pub struct GlobalsBuiltin;
#[derive(Clone)]
pub struct HashBuiltin;
#[derive(Clone)]
pub struct IsinstanceBuiltin;
#[derive(Clone)]
pub struct IssubclassBuiltin;
#[derive(Clone)]
pub struct IterBuiltin;
#[derive(Clone)]
pub struct LenBuiltin;
#[derive(Clone)]
pub struct NextBuiltin;
#[derive(Clone)]
pub struct PrintBuiltin;

impl Callable for CallableBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;
        Ok(TreewalkValue::Bool(args.get_arg(0).as_callable().is_ok()))
    }

    fn name(&self) -> String {
        "callable".into()
    }
}

impl Callable for DirBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;
        let dir = args
            .get_arg(0)
            .into_member_reader(interpreter)
            .dir()
            .iter()
            .map(|i| TreewalkValue::Str(Str::new(i)))
            .collect::<Vec<_>>();
        Ok(TreewalkValue::List(Container::new(List::new(dir))))
    }

    fn name(&self) -> String {
        "dir".into()
    }
}

impl Callable for GetattrBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [2, 3].contains(&len), interpreter)?;

        let object = args.get_arg(0);
        let name = args.get_arg(1).as_str().raise(interpreter)?;

        let attr = object
            .clone()
            .into_member_reader(interpreter)
            .get_member(interpreter, name.as_str())?;

        if let Some(attr) = attr {
            Ok(attr)
        } else {
            // Use the default value if provided
            if args.len() == 3 {
                Ok(args.get_arg(2))
            } else {
                Err(interpreter.attribute_error(&object, name))
            }
        }
    }

    fn name(&self) -> String {
        "getattr".into()
    }
}

impl Callable for GlobalsBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0, interpreter)?;
        Ok(TreewalkValue::Dict(
            interpreter.state.read_globals(interpreter),
        ))
    }

    fn name(&self) -> String {
        "globals".into()
    }
}

impl Callable for HashBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let arg = args.get_arg(0);
        if arg.as_class().is_ok() {
            return Ok(TreewalkValue::Int(arg.hash() as i64));
        }

        let result = interpreter.invoke_method(&arg, Dunder::Hash, args![])?;

        if let TreewalkValue::Int(_) = result {
            Ok(result)
        } else {
            Err(interpreter.type_error(format!("{} method should return an integer", Dunder::Hash)))
        }
    }

    fn name(&self) -> String {
        "hash".into()
    }
}

impl Callable for IsinstanceBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 2, interpreter)?;
        let message = "isinstance() arg 2 must be a type, a tuple of types, or a union";

        let instance_class = args.get_arg(0).get_class(interpreter);

        let reference_class = match args.get_arg(1) {
            TreewalkValue::Class(class) => vec![class],
            TreewalkValue::Tuple(tuple) => tuple
                .into_iter()
                .map(|item| item.as_class())
                .collect::<ExecResult<Vec<_>>>()
                .map_err(|_| interpreter.type_error(message))?,
            _ => return Err(interpreter.type_error(message)),
        };

        let isinstance = if args.get_arg(0).as_class().is_ok() {
            has_overlap(&reference_class, &instance_class.borrow().metaclass().mro())
        } else {
            has_overlap(&reference_class, &instance_class.mro())
        };

        Ok(TreewalkValue::Bool(isinstance))
    }

    fn name(&self) -> String {
        "isinstance".into()
    }
}

impl Callable for IssubclassBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 2, interpreter)?;

        let instance_class = args
            .get_arg(0)
            .as_class()
            .map_err(|_| interpreter.type_error("issubclass() arg 1 must be a class"))?;

        let reference_class = args.get_arg(1).as_class().map_err(|_| {
            interpreter
                .type_error("issubclass() arg 2 must be a type, a tuple of types, or a union")
        })?;

        Ok(TreewalkValue::Bool(
            instance_class.mro().contains(&reference_class),
        ))
    }

    fn name(&self) -> String {
        "issubclass".into()
    }
}

impl Callable for PrintBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        println!(
            "{}",
            args.iter_args()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" ")
        );

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "print".into()
    }
}

impl Callable for LenBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;
        let iterator = args.get_arg(0).as_iterator().raise(interpreter)?;
        Ok(TreewalkValue::Int(iterator.count() as i64))
    }

    fn name(&self) -> String {
        "len".into()
    }
}

impl Callable for NextBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;
        let mut iterator = args.get_arg(0).as_iterator_strict().raise(interpreter)?;
        match Iterable::try_next(&mut iterator) {
            Ok(Some(val)) => Ok(val),
            Ok(None) => Err(interpreter.stop_iteration()),
            Err(e) => Err(e),
        }
    }

    fn name(&self) -> String {
        "next".into()
    }
}

impl Callable for IterBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;
        args.get_arg(0).as_iterable().raise(interpreter)
    }

    fn name(&self) -> String {
        "iter".into()
    }
}

fn has_overlap<T: PartialEq>(vec1: &[T], vec2: &[T]) -> bool {
    vec1.iter().any(|item| vec2.contains(item))
}
