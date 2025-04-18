use crate::{
    core::Container,
    domain::Dunder,
    treewalk::{
        protocols::Callable,
        types::{iterators::StringIterator, List, Str},
        utils::{args, check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

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
        Ok(TreewalkValue::Boolean(
            args.get_arg(0).into_callable().is_some(),
        ))
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
            .map(|i| TreewalkValue::String(Str::new(i.to_string())))
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
        let name = args.get_arg(1).expect_string(interpreter)?;

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
        if arg.as_class().is_some() {
            return Ok(TreewalkValue::Integer(arg.hash() as i64));
        }

        let result = interpreter.invoke_method(&arg, Dunder::Hash, args![])?;

        if let TreewalkValue::Integer(_) = result {
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
            TreewalkValue::Tuple(tuple) => {
                let classes: Result<Vec<_>, _> = tuple
                    .into_iter()
                    .map(|item| {
                        item.as_class()
                            .ok_or_else(|| interpreter.type_error(message))
                    })
                    .collect();
                classes?
            }
            _ => return Err(interpreter.type_error(message)),
        };

        let isinstance = if args.get_arg(0).is_class() {
            has_overlap(&reference_class, &instance_class.borrow().metaclass().mro())
        } else {
            has_overlap(&reference_class, &instance_class.mro())
        };

        Ok(TreewalkValue::Boolean(isinstance))
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
            .ok_or_else(|| interpreter.type_error("issubclass() arg 1 must be a class"))?;

        let reference_class = args.get_arg(1).as_class().ok_or_else(|| {
            interpreter
                .type_error("issubclass() arg 2 must be a type, a tuple of types, or a union")
        })?;

        Ok(TreewalkValue::Boolean(
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

        let iterator = args
            .get_arg(0)
            .clone()
            .try_into_iter()
            .ok_or_else(|| interpreter.type_error("Expected an iterator"))?;

        Ok(TreewalkValue::Integer(iterator.count() as i64))
    }

    fn name(&self) -> String {
        "len".into()
    }
}

impl Callable for NextBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let mut generator = args
            .get_arg(0)
            .as_generator()
            .ok_or_else(|| interpreter.type_error("Expected an iterator"))?;

        generator.next().ok_or_else(|| interpreter.stop_iteration())
    }

    fn name(&self) -> String {
        "next".into()
    }
}

impl Callable for IterBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        match args.get_arg(0) {
            TreewalkValue::String(s) => Ok(TreewalkValue::StringIterator(StringIterator::new(s))),
            TreewalkValue::List(list) => Ok(TreewalkValue::ListIterator(list.into_iter())),
            TreewalkValue::ReversedIterator(_) => Ok(args.get_arg(0)),
            TreewalkValue::Set(set) => Ok(TreewalkValue::SetIterator(set.into_iter())),
            TreewalkValue::Zip(_) => Ok(args.get_arg(0)),
            TreewalkValue::Tuple(tuple) => Ok(TreewalkValue::TupleIterator(tuple.into_iter())),
            TreewalkValue::DictItems(dict) => {
                Ok(TreewalkValue::DictItemsIterator(dict.into_iter()))
            }
            TreewalkValue::DictKeys(dict) => Ok(TreewalkValue::DictKeysIterator(dict.into_iter())),
            TreewalkValue::DictValues(dict) => {
                Ok(TreewalkValue::DictValuesIterator(dict.into_iter()))
            }
            TreewalkValue::Bytes(b) => Ok(TreewalkValue::BytesIterator(b)),
            TreewalkValue::ByteArray(b) => {
                Ok(TreewalkValue::ByteArrayIterator(b.borrow().raw().to_vec()))
            }
            TreewalkValue::Range(r) => Ok(TreewalkValue::RangeIterator(r.into_iter())),
            _ => Err(interpreter.type_error("Expected an iterable")),
        }
    }

    fn name(&self) -> String {
        "iter".into()
    }
}

/// This can be used when you need something that implements `Callable` to compile, but you don't
/// plan on ever running with this. A placeholder.
pub struct NoopCallable;

impl Callable for NoopCallable {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!()
    }

    fn name(&self) -> String {
        unimplemented!()
    }
}

pub fn has_overlap<T: PartialEq>(vec1: &[T], vec2: &[T]) -> bool {
    vec1.iter().any(|item| vec2.contains(item))
}
