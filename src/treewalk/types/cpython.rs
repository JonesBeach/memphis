use pyo3::{
    prelude::Python,
    pyclass,
    types::{PyAny, PyCFunction, PyDict, PyList, PyModule, PyString, PyTuple},
    IntoPy, Py, PyObject, PyResult, ToPyObject,
};
use std::fmt::{Display, Error, Formatter};

use crate::{
    core::{log, Container, LogLevel},
    domain::Dunder,
    parser::types::ImportPath,
    treewalk::{
        types::{
            domain::traits::{Callable, IndexRead, IndexWrite, MemberReader},
            utils::ResolvedArguments,
            ExprResult, Str,
        },
        Interpreter,
    },
    types::errors::InterpreterError,
};

pub fn import_from_cpython(
    interpreter: &Interpreter,
    import_path: &ImportPath,
) -> Option<ExprResult> {
    if BUILTIN_MODULE_NAMES.contains(&import_path.as_str().as_str()) {
        return Some(ExprResult::CPythonModule(
            interpreter.state.import_builtin_module(import_path),
        ));
    }

    if let Some(module) = interpreter.state.read("sys") {
        let import_str = import_path.as_str();
        // TODO this is a hack so that we only take this code path for "os.path"
        if import_str != "os.path" {
            return None;
        }
        let sys_modules = module
            .as_module()
            .expect("Failed to read sys")
            .get_member(interpreter, "modules")
            .expect("Failed to read sys.modules")
            .expect("Failed to read sys.modules");
        let module_result = sys_modules
            .as_index_read(interpreter)
            .expect("Failed to read sys.modules")
            .getitem(interpreter, ExprResult::String(Str::new(import_str)))
            .expect("Failed to find this import path");
        if let Some(module) = module_result {
            return Some(module);
        }
    }

    None
}

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
const BUILTIN_MODULE_NAMES: [&'static str; 8] = [
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
        let pymodule = Python::with_gil(|py| {
            PyModule::import(py, name)
                .expect(&format!("Failed to import CPython module '{}'", name))
                .into()
        });

        Self(pymodule)
    }

    fn name(&self) -> String {
        self.get_item(Dunder::Name)
            .unwrap()
            .unwrap()
            .as_string()
            .unwrap()
    }

    fn get_item<S>(&self, name: S) -> Result<Option<ExprResult>, InterpreterError>
    where
        S: IntoPy<Py<PyString>>,
    {
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

    /// TODO fix this name field for CPythonObject
    /// I don't think we ever need this, which means we may be modeling something poorly here.
    fn name(&self) -> String {
        unreachable!()
    }
}

#[pyclass(weakref)]
struct TestClass;

impl ToPyObject for ExprResult {
    fn to_object(&self, py: Python) -> PyObject {
        match self {
            ExprResult::None => py.None(),
            ExprResult::Boolean(b) => b.to_object(py),
            ExprResult::Integer(i) => i.to_object(py),
            ExprResult::String(s) => s.as_str().to_object(py),
            ExprResult::List(l) => {
                let list = PyList::empty(py);
                for item in l.clone().into_iter() {
                    list.append(item).expect("Failed to append to PyList");
                }
                list.to_object(py)
            }
            ExprResult::Function(_) => {
                // TODO our PyCFunction implementation is a no-op, we need to find a way to pass
                // the interpreter into here.
                let callback = |_args: &PyTuple, _kwargs: Option<&PyDict>| -> PyResult<bool> {
                    log(LogLevel::Warn, || {
                        "Potentially lossy PyCFunction invocation.".to_string()
                    });
                    Ok(true)
                };
                // TODO use real function name
                let py_cfunc =
                    PyCFunction::new_closure(py, Some("memphis_func"), None, callback).unwrap();
                py_cfunc.to_object(py)
            }
            ExprResult::Class(_) => {
                // TODO same here, our PyClass implementation does bring real fields
                Py::new(py, TestClass {}).unwrap().to_object(py)
            }
            ExprResult::Module(module) => {
                let py_module = PyModule::new(py, &module.borrow().name()).unwrap();

                // Flatten all key-value pairs from scope into the module
                for (key, value) in module.borrow().dict() {
                    py_module.add(key, value.to_object(py)).unwrap();
                }

                py_module.to_object(py)
            }
            ExprResult::CPythonModule(module) => module.borrow().0.to_object(py),
            ExprResult::CPythonObject(object) => object.0.to_object(py),
            _ => unimplemented!(
                "Attempting to convert {} to a PyObject, but {} conversion is not implemented!",
                self,
                self.get_type()
            ),
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

impl Display for CPythonObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "<CPythonObject>: {}", self.0)
    }
}

impl CPythonObject {
    pub fn get_type(&self) -> ExprResult {
        Python::with_gil(|py| {
            let obj_ref: &PyAny = self.0.as_ref(py);
            let obj_type = obj_ref.getattr(Dunder::Class).unwrap();
            ExprResult::CPythonClass(CPythonClass(obj_type.into()))
        })
    }

    pub fn hasattr(&self, attr: Dunder) -> bool {
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
                .call_method1(Dunder::GetItem, (key,))
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
                .call_method1(Dunder::SetItem, (key, value))
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
                .call_method1(Dunder::DelItem, (key,))
                .unwrap();
        });

        Ok(())
    }
}

pub mod utils {
    use pyo3::types::{PyBool, PySet};

    use crate::{
        core::Container,
        treewalk::types::{List, Module, Set, Str, Tuple},
    };

    use super::*;

    pub fn from_pyobject(py: Python, py_obj: &PyAny) -> ExprResult {
        if let Ok(value) = py_obj.extract::<i64>() {
            ExprResult::Integer(value)
        } else if let Ok(value) = py_obj.extract::<f64>() {
            ExprResult::FloatingPoint(value)
        } else if let Ok(value) = py_obj.extract::<&str>() {
            ExprResult::String(Str::new(value.to_string()))
        } else if let Ok(py_tuple) = py_obj.extract::<&PyTuple>() {
            let elements = py_tuple
                .iter()
                .map(|item| from_pyobject(py, item))
                .collect();
            ExprResult::Tuple(Tuple::new(elements))
        } else if let Ok(py_module) = py_obj.extract::<&PyModule>() {
            let mut module = Module::default();

            // Get the module's __dict__ to iterate over all attributes
            for (key, value) in py_module.dict() {
                let key_str: String = key.extract().expect("Key is not a string");
                let expr_value = from_pyobject(py, value);
                module.insert(&key_str, expr_value);
            }

            ExprResult::Module(Container::new(module))
        } else if let Ok(py_set) = py_obj.extract::<&PySet>() {
            let elements = py_set.iter().map(|item| from_pyobject(py, item)).collect();
            ExprResult::Set(Container::new(Set::new(elements)))
        } else if let Ok(py_list) = py_obj.extract::<&PyList>() {
            let elements = py_list.iter().map(|item| from_pyobject(py, item)).collect();
            ExprResult::List(Container::new(List::new(elements)))
        } else {
            // TODO think of a way to detect whether this is an object we can convert or not
            // log(LogLevel::Warn, || {
            //     "Potentially ambiguous CPythonObject instance.".to_string()
            // });
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

    pub fn hasattr<S>(py: Python, obj: &PyAny, attr: S) -> PyResult<bool>
    where
        S: IntoPy<Py<PyAny>>,
    {
        let hasattr = py.eval("hasattr", None, None)?.to_object(py);
        let has_setitem = hasattr.call1(py, (obj, attr))?;
        let has_setitem_bool = has_setitem.extract::<&PyBool>(py)?;
        Ok(has_setitem_bool.is_true())
    }
}
