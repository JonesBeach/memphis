use pyo3::prelude::Python;
use pyo3::types::{PyAny, PyCFunction, PyDict, PyModule, PyTuple};
use pyo3::{pyclass, IntoPy, Py, PyObject, PyResult, ToPyObject};
use std::fmt::{Display, Error, Formatter};

use crate::core::Container;
use crate::treewalk::{
    types::{
        traits::{Callable, IndexRead, IndexWrite, MemberReader},
        utils::{Dunder, ResolvedArguments},
        ExprResult,
    },
    Interpreter,
};
use crate::types::errors::InterpreterError;

// This is the full list, but that causes the stdlib to take a different path because of blocks
// like this from abc.py:
// ```
// try:
//     from _abc import (...)
// except ImportError:
//     from _py_abc import ...
// ```
// I'm going to keep this disabled for now until we find which ones we absolutely must have.
//
// pub const BUILTIN_MODULE_NAMES: [&'static str; 31] = [
//     "_abc",
//     "_ast",
//     "_codecs",
//     "_collections",
//     "_functools",
//     "_imp",
//     "_io",
//     "_locale",
//     "_operator",
//     "_signal",
//     "_sre",
//     "_stat",
//     "_string",
//     "_symtable",
//     "_thread",
//     "_tokenize",
//     "_tracemalloc",
//     "_typing",
//     "_warnings",
//     "_weakref",
//     "atexit",
//     "builtins",
//     "errno",
//     "faulthandler",
//     "gc",
//     "itertools",
//     "marshal",
//     "posix",
//     "pwd",
//     "sys",
//     "time",
// ];
pub const BUILTIN_MODULE_NAMES: [&'static str; 8] = [
    "builtins",
    "errno",
    "posix",
    "itertools",
    "sys",
    "time",
    "_thread",
    "_weakref",
];

#[derive(Clone)]
pub struct CPythonModule(PyObject);

impl Display for Container<CPythonModule> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "<module '{}' (built-in)>", self.borrow().name())
    }
}

impl CPythonModule {
    pub fn new(name: &str) -> Self {
        pyo3::prepare_freethreaded_python();
        let pymodule = Python::with_gil(|py| PyModule::import(py, name).expect("failed").into());

        Self(pymodule)
    }

    fn name(&self) -> String {
        self.get_item(Dunder::Name.value())
            .unwrap()
            .unwrap()
            .as_string()
            .unwrap()
    }

    fn get_item(&self, name: &str) -> Result<Option<ExprResult>, InterpreterError> {
        Ok(Python::with_gil(|py| {
            match self.0.as_ref(py).getattr(name) {
                Ok(py_attr) => Some(utils::from_pyobject(py, py_attr)),
                Err(_) => None,
            }
        }))
    }
}

impl MemberReader for CPythonModule {
    fn get_member(
        &self,
        _interpreter: &Interpreter,
        name: &str,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        self.get_item(name)
    }

    fn dir(&self) -> Vec<String> {
        Python::with_gil(|py| {
            self.0
                .as_ref(py)
                .dir()
                .iter()
                .map(|item| item.extract::<String>().unwrap())
                .collect()
        })
    }
}

impl Callable for CPythonObject {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        Python::with_gil(|py| {
            let py_attr: &PyAny = self.0.as_ref(py);
            if py_attr.is_callable() {
                if args.is_empty() {
                    let result = py_attr.call0().map_err(|e| {
                        dbg!(&e);
                        panic!()
                    })?;
                    Ok(utils::from_pyobject(py, result))
                } else if args.get_kwargs().is_empty() {
                    let result = py_attr.call1(utils::to_args(py, args));
                    let result = result.map_err(|e| {
                        dbg!(&e);
                        panic!()
                    })?;
                    Ok(utils::from_pyobject(py, result))
                } else {
                    // Need to use py_attr.call() here
                    unimplemented!()
                }
            } else {
                Err(InterpreterError::FunctionNotFound(
                    self.name(),
                    interpreter.state.call_stack(),
                ))
            }
        })
    }

    // I don't think we ever need this, which means we may be modeling something poorly here.
    fn name(&self) -> String {
        "<cpythonobject_NONE>".into()
    }
}

#[pyclass(weakref)]
struct TestClass;

impl ToPyObject for ExprResult {
    fn to_object(&self, py: Python) -> PyObject {
        match self {
            ExprResult::Integer(val) => val.borrow().to_object(py),
            ExprResult::String(s) => s.0.as_str().to_object(py),
            ExprResult::Function(_) => {
                // This still doesn't actually do anything.
                let callback =
                    |_args: &PyTuple, _kwargs: Option<&PyDict>| -> PyResult<_> { Ok(true) };
                let py_cfunc = PyCFunction::new_closure(py, None, None, callback).unwrap();
                py_cfunc.to_object(py)
            }
            ExprResult::Class(_) => Py::new(py, TestClass {}).unwrap().to_object(py),
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone)]
#[allow(dead_code)]
pub struct CPythonClass(PyObject);

#[derive(Clone)]
pub struct CPythonObject(PyObject);

impl CPythonObject {
    pub fn new(py_object: PyObject) -> Self {
        Self(py_object)
    }
}

impl CPythonObject {
    pub fn get_type(&self) -> ExprResult {
        Python::with_gil(|py| {
            let obj_ref: &PyAny = self.0.as_ref(py);
            let obj_type = obj_ref.getattr(Dunder::Class.value()).unwrap();
            ExprResult::CPythonClass(CPythonClass(obj_type.into()))
        })
    }

    pub fn hasattr(&self, attr: &str) -> bool {
        Python::with_gil(|py| utils::hasattr(py, self.0.as_ref(py), attr).unwrap())
    }
}

impl IndexRead for CPythonObject {
    fn getitem(
        &self,
        _interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        Python::with_gil(|py| {
            let key = index.to_object(py);
            let result = self
                .0
                .as_ref(py)
                .call_method1(Dunder::GetItem.value(), (key,))
                .unwrap();
            Ok(Some(utils::from_pyobject(py, result)))
        })
    }
}

impl IndexWrite for CPythonObject {
    fn setitem(
        &mut self,
        _interpreter: &Interpreter,
        index: ExprResult,
        value: ExprResult,
    ) -> Result<(), InterpreterError> {
        Python::with_gil(|py| {
            let key = index.to_object(py);
            let value = value.to_object(py);
            self.0
                .as_ref(py)
                .call_method1(Dunder::SetItem.value(), (key, value))
                .unwrap();
        });

        Ok(())
    }

    fn delitem(
        &mut self,
        _interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<(), InterpreterError> {
        Python::with_gil(|py| {
            let key = index.to_object(py);
            self.0
                .as_ref(py)
                .call_method1(Dunder::DelItem.value(), (key,))
                .unwrap();
        });

        Ok(())
    }
}

pub mod utils {
    use pyo3::types::PyBool;

    use crate::{
        core::Container,
        treewalk::types::{Str, Tuple},
    };

    use super::*;

    pub fn from_pyobject(py: Python, py_obj: &PyAny) -> ExprResult {
        if let Ok(value) = py_obj.extract::<i64>() {
            ExprResult::Integer(Container::new(value))
        } else if let Ok(value) = py_obj.extract::<f64>() {
            ExprResult::FloatingPoint(value)
        } else if let Ok(value) = py_obj.extract::<&str>() {
            ExprResult::String(Str::new(value.to_string()))
        } else if let Ok(tuple) = py_obj.extract::<&PyTuple>() {
            let elements: Vec<ExprResult> =
                tuple.iter().map(|item| from_pyobject(py, item)).collect();
            ExprResult::Tuple(Container::new(Tuple::new(elements)))
        } else {
            ExprResult::CPythonObject(CPythonObject::new(py_obj.into_py(py)))
        }
    }

    pub fn to_args(py: Python, args: ResolvedArguments) -> &PyTuple {
        let args = args
            .iter_args()
            .map(|a| a.to_object(py))
            .collect::<Vec<_>>();
        PyTuple::new(py, args.iter().map(|item| item.as_ref(py)))
    }

    pub fn hasattr(py: Python, obj: &PyAny, attr: &str) -> PyResult<bool> {
        let hasattr = py.eval("hasattr", None, None)?.to_object(py);
        let has_setitem = hasattr.call1(py, (obj, attr))?;
        let has_setitem_bool: &PyBool = has_setitem.extract(py)?;
        Ok(has_setitem_bool.is_true())
    }
}
