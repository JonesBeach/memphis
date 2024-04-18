use crate::{core::Container, types::errors::InterpreterError};

use super::{traits::NonDataDescriptor, utils::Dunder, Class, ExprResult};

#[derive(Debug, PartialEq, Clone)]
pub struct Exception;

impl Exception {
    pub fn get_descriptors() -> Vec<Box<dyn NonDataDescriptor>> {
        vec![Box::new(TracebackAttribute)]
    }
}

struct TracebackAttribute;

impl NonDataDescriptor for TracebackAttribute {
    fn get_attr(
        &self,
        _interpreter: &crate::treewalk::Interpreter,
        _instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::Traceback(Traceback))
    }

    fn name(&self) -> String {
        Dunder::Traceback.into()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Traceback;

impl Traceback {
    pub fn get_descriptors() -> Vec<Box<dyn NonDataDescriptor>> {
        vec![Box::new(FrameAttribute)]
    }
}

struct FrameAttribute;

impl NonDataDescriptor for FrameAttribute {
    fn get_attr(
        &self,
        _interpreter: &crate::treewalk::Interpreter,
        _instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::Frame)
    }

    fn name(&self) -> String {
        "tb_frame".into()
    }
}
