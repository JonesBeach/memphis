use crate::{
    core::Container,
    treewalk::{
        types::{
            domain::traits::Callable, iterators::StringIterator, utils::ResolvedArguments,
            ExprResult, List, Str,
        },
        Interpreter,
    },
    types::errors::InterpreterError,
};

pub struct CallableBuiltin;
pub struct DirBuiltin;
pub struct GetattrBuiltin;
pub struct GlobalsBuiltin;
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
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;
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
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;
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
    ) -> Result<ExprResult, InterpreterError> {
        if ![2, 3].contains(&args.len()) {
            return Err(InterpreterError::WrongNumberOfArguments(
                2,
                args.len(),
                interpreter.state.call_stack(),
            ));
        }

        let object = args.get_arg(0);
        let name = args
            .get_arg(1)
            .as_string()
            .ok_or(InterpreterError::ExpectedString(
                interpreter.state.call_stack(),
            ))?;

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
                Err(InterpreterError::AttributeError(
                    object.get_class(interpreter).name(),
                    name,
                    interpreter.state.call_stack(),
                ))
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
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 0, interpreter.state.call_stack())?;
        Ok(ExprResult::Dict(
            interpreter.state.read_globals(interpreter.clone()),
        ))
    }

    fn name(&self) -> String {
        "globals".into()
    }
}

fn has_overlap<T: PartialEq>(vec1: &[T], vec2: &[T]) -> bool {
    vec1.iter().any(|item| vec2.contains(item))
}

impl Callable for IsinstanceBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 2, interpreter.state.call_stack())?;
        let msg = Some("isinstance() arg 2 must be a type, a tuple of types, or a union".into());

        let instance_class = args.get_arg(0).get_class(interpreter);

        let reference_class = match args.get_arg(1) {
            ExprResult::Class(class) => vec![class],
            ExprResult::Tuple(tuple) => {
                let classes: Result<Vec<_>, _> = tuple
                    .into_iter()
                    .map(|item| {
                        item.as_class().ok_or(InterpreterError::TypeError(
                            msg.clone(),
                            interpreter.state.call_stack(),
                        ))
                    })
                    .collect();
                classes?
            }
            _ => {
                return Err(InterpreterError::TypeError(
                    msg,
                    interpreter.state.call_stack(),
                ))
            }
        };

        let isinstance = if args.get_arg(0).is_class() {
            has_overlap(&reference_class, &instance_class.borrow().metaclass().mro())
        } else {
            has_overlap(&reference_class, &instance_class.mro())
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
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 2, interpreter.state.call_stack())?;

        let instance_class = args
            .get_arg(0)
            .as_class()
            .ok_or(InterpreterError::TypeError(
                Some("issubclass() arg 1 must be a class".into()),
                interpreter.state.call_stack(),
            ))?;

        let reference_class = args
            .get_arg(1)
            .as_class()
            .ok_or(InterpreterError::TypeError(
                Some("issubclass() arg 2 must be a type, a tuple of types, or a union".into()),
                interpreter.state.call_stack(),
            ))?;

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
    ) -> Result<ExprResult, InterpreterError> {
        println!(
            "{}",
            args.iter_args()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" ")
        );
        Ok(ExprResult::Void)
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
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;

        let iterator =
            args.get_arg(0)
                .clone()
                .try_into_iter()
                .ok_or(InterpreterError::ExpectedIterable(
                    interpreter.state.call_stack(),
                ))?;

        Ok(ExprResult::Integer(Container::new(iterator.count() as i64)))
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
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;

        let generator =
            args.get_arg(0)
                .as_generator()
                .ok_or(InterpreterError::ExpectedIterable(
                    interpreter.state.call_stack(),
                ))?;

        generator
            .clone()
            .borrow_mut()
            .next()
            .ok_or(InterpreterError::StopIteration(
                interpreter.state.call_stack(),
            ))
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
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;

        match args.get_arg(0) {
            ExprResult::String(s) => Ok(ExprResult::StringIterator(StringIterator::new(s.clone()))),
            ExprResult::List(list) => Ok(ExprResult::ListIterator(list.clone().into_iter())),
            ExprResult::ReversedIterator(_) => Ok(args.get_arg(0).clone()),
            ExprResult::Set(set) => Ok(ExprResult::SetIterator(set.clone().into_iter())),
            ExprResult::Zip(_) => Ok(args.get_arg(0).clone()),
            ExprResult::Tuple(tuple) => Ok(ExprResult::TupleIterator(tuple.clone().into_iter())),
            ExprResult::DictItems(dict) => {
                Ok(ExprResult::DictItemsIterator(dict.clone().into_iter()))
            }
            ExprResult::DictKeys(dict) => {
                Ok(ExprResult::DictKeysIterator(dict.clone().into_iter()))
            }
            ExprResult::DictValues(dict) => {
                Ok(ExprResult::DictValuesIterator(dict.clone().into_iter()))
            }
            ExprResult::Bytes(b) => Ok(ExprResult::BytesIterator(b.borrow().0.to_vec())),
            ExprResult::ByteArray(b) => Ok(ExprResult::ByteArrayIterator(b.borrow().0.to_vec())),
            ExprResult::Range(r) => Ok(ExprResult::RangeIterator(r.clone().into_iter())),
            _ => Err(InterpreterError::ExpectedObject(
                interpreter.state.call_stack(),
            )),
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
    ) -> Result<ExprResult, InterpreterError> {
        unimplemented!()
    }

    fn name(&self) -> String {
        unimplemented!()
    }
}

pub mod utils {
    use crate::treewalk::CallStack;

    use super::*;

    pub(crate) fn validate_args(
        args: &ResolvedArguments,
        expected_length: usize,
        call_stack: CallStack,
    ) -> Result<(), InterpreterError> {
        if args.len() != expected_length {
            Err(InterpreterError::WrongNumberOfArguments(
                expected_length,
                args.len(),
                call_stack,
            ))
        } else {
            Ok(())
        }
    }
}
