use crate::{
    core::Container,
    domain::Dunder,
    resolved_args,
    treewalk::{
        interpreter::TreewalkResult,
        types::{
            domain::traits::Callable, iterators::StringIterator, utils::ResolvedArguments,
            ExprResult, List, Str,
        },
        Interpreter,
    },
};

pub struct CallableBuiltin;
pub struct DirBuiltin;
pub struct GetattrBuiltin;
pub struct GlobalsBuiltin;
pub struct HashBuiltin;
pub struct IsinstanceBuiltin;
pub struct IssubclassBuiltin;
pub struct IterBuiltin;
pub struct LenBuiltin;
pub struct NextBuiltin;
pub struct PrintBuiltin;

impl Callable for CallableBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 1, interpreter)?;
        Ok(ExprResult::Boolean(args.get_arg(0).as_callable().is_some()))
    }

    fn name(&self) -> String {
        "callable".into()
    }
}

impl Callable for DirBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 1, interpreter)?;
        let dir = args
            .get_arg(0)
            .as_member_reader(interpreter)
            .dir()
            .iter()
            .map(|i| ExprResult::String(Str::new(i.to_string())))
            .collect::<Vec<_>>();
        Ok(ExprResult::List(Container::new(List::new(dir))))
    }

    fn name(&self) -> String {
        "dir".into()
    }
}

impl Callable for GetattrBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| [2, 3].contains(&len), interpreter)?;

        let object = args.get_arg(0);
        let name = args.get_arg(1).expect_string(interpreter)?;

        let attr = object
            .as_member_reader(interpreter)
            .get_member(interpreter, name.as_str())?;

        if let Some(attr) = attr {
            Ok(attr)
        } else {
            // Use the default value if provided
            if args.len() == 3 {
                Ok(args.get_arg(2))
            } else {
                Err(interpreter.attribute_error(object, name))
            }
        }
    }

    fn name(&self) -> String {
        "getattr".into()
    }
}

impl Callable for GlobalsBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 0, interpreter)?;
        Ok(ExprResult::Dict(
            interpreter.state.read_globals(interpreter),
        ))
    }

    fn name(&self) -> String {
        "globals".into()
    }
}

impl Callable for HashBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 1, interpreter)?;

        let arg = args.get_arg(0);
        if arg.as_class().is_some() {
            return Ok(ExprResult::Integer(arg.hash() as i64));
        }

        let result = interpreter.invoke_method(arg, Dunder::Hash, &resolved_args![])?;

        if let ExprResult::Integer(_) = result {
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
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 2, interpreter)?;
        let message = "isinstance() arg 2 must be a type, a tuple of types, or a union";

        let instance_class = args.get_arg(0).get_class(interpreter);

        let reference_class = match args.get_arg(1) {
            ExprResult::Class(class) => vec![class],
            ExprResult::Tuple(tuple) => {
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
            utils::has_overlap(&reference_class, &instance_class.borrow().metaclass().mro())
        } else {
            utils::has_overlap(&reference_class, &instance_class.mro())
        };

        Ok(ExprResult::Boolean(isinstance))
    }

    fn name(&self) -> String {
        "isinstance".into()
    }
}

impl Callable for IssubclassBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 2, interpreter)?;

        let instance_class = args
            .get_arg(0)
            .as_class()
            .ok_or_else(|| interpreter.type_error("issubclass() arg 1 must be a class"))?;

        let reference_class = args.get_arg(1).as_class().ok_or_else(|| {
            interpreter
                .type_error("issubclass() arg 2 must be a type, a tuple of types, or a union")
        })?;

        Ok(ExprResult::Boolean(
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
        _interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        println!(
            "{}",
            args.iter_args()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" ")
        );

        Ok(ExprResult::None)
    }

    fn name(&self) -> String {
        "print".into()
    }
}

impl Callable for LenBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 1, interpreter)?;

        let iterator = args
            .get_arg(0)
            .clone()
            .try_into_iter()
            .ok_or_else(|| interpreter.type_error("Expected an iterator"))?;

        Ok(ExprResult::Integer(iterator.count() as i64))
    }

    fn name(&self) -> String {
        "len".into()
    }
}

impl Callable for NextBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 1, interpreter)?;

        let generator = args
            .get_arg(0)
            .as_generator()
            .ok_or_else(|| interpreter.type_error("Expected an iterator"))?;

        generator
            .clone()
            .borrow_mut()
            .next()
            .ok_or_else(|| interpreter.stop_iteration())
    }

    fn name(&self) -> String {
        "next".into()
    }
}

impl Callable for IterBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 1, interpreter)?;

        match args.get_arg(0) {
            ExprResult::String(s) => Ok(ExprResult::StringIterator(StringIterator::new(s))),
            ExprResult::List(list) => Ok(ExprResult::ListIterator(list.into_iter())),
            ExprResult::ReversedIterator(_) => Ok(args.get_arg(0)),
            ExprResult::Set(set) => Ok(ExprResult::SetIterator(set.into_iter())),
            ExprResult::Zip(_) => Ok(args.get_arg(0)),
            ExprResult::Tuple(tuple) => Ok(ExprResult::TupleIterator(tuple.into_iter())),
            ExprResult::DictItems(dict) => Ok(ExprResult::DictItemsIterator(dict.into_iter())),
            ExprResult::DictKeys(dict) => Ok(ExprResult::DictKeysIterator(dict.into_iter())),
            ExprResult::DictValues(dict) => Ok(ExprResult::DictValuesIterator(dict.into_iter())),
            ExprResult::Bytes(b) => Ok(ExprResult::BytesIterator(b)),
            ExprResult::ByteArray(b) => {
                Ok(ExprResult::ByteArrayIterator(b.borrow().raw().to_vec()))
            }
            ExprResult::Range(r) => Ok(ExprResult::RangeIterator(r.into_iter())),
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
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        unimplemented!()
    }

    fn name(&self) -> String {
        unimplemented!()
    }
}

pub mod utils {
    use super::*;

    pub fn validate_args<F>(
        args: &ResolvedArguments,
        condition: F,
        interpreter: &Interpreter,
    ) -> TreewalkResult<()>
    where
        F: Fn(usize) -> bool,
    {
        if !condition(args.len()) {
            Err(interpreter.type_error(format!("Found {} args", args.len())))
        } else {
            Ok(())
        }
    }

    pub fn has_overlap<T: PartialEq>(vec1: &[T], vec2: &[T]) -> bool {
        vec1.iter().any(|item| vec2.contains(item))
    }
}
