use std::{
    collections::HashMap,
    fmt::{Display, Error, Formatter},
};

use pyo3::{
    prelude::*,
    pyclass,
    types::{PyAny, PyList, PyModule, PyString, PyTuple},
    Bound, BoundObject, PyObject, PyResult,
};

use crate::{
    core::Container,
    domain::Dunder,
    parser::types::ImportPath,
    treewalk::{
        protocols::{Callable, IndexRead, IndexWrite, MemberReader},
        types::Str,
        utils::Arguments,
        Interpreter, TreewalkResult, TreewalkValue,
    },
};

pub struct BuiltinModuleCache {
    builtin_module_cache: HashMap<ImportPath, Container<CPythonModule>>,
}

impl BuiltinModuleCache {
    pub fn new() -> Self {
        Self {
            builtin_module_cache: HashMap::default(),
        }
    }

    pub fn import_builtin_module(&mut self, import_path: &ImportPath) -> Container<CPythonModule> {
        if let Some(module) = self.builtin_module_cache.get(import_path) {
            module.clone()
        } else {
            let module = Container::new(CPythonModule::new(&import_path.as_str()));
            self.builtin_module_cache
                .insert(import_path.to_owned(), module.clone());
            module
        }
    }
}

pub fn import_from_cpython(
    interpreter: &Interpreter,
    import_path: &ImportPath,
) -> Option<TreewalkValue> {
    if BUILTIN_MODULE_NAMES.contains(&import_path.as_str().as_str()) {
        return Some(TreewalkValue::CPythonModule(
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
            .getitem(interpreter, TreewalkValue::String(Str::new(import_str)))
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
const BUILTIN_MODULE_NAMES: [&str; 8] = [
    "builtins",
    "errno",
    "posix",
    "itertools",
    "sys",
    "time",
    "_thread",
    "_weakref",
];

pub struct CPythonModule(PyObject);

impl Clone for CPythonModule {
    fn clone(&self) -> Self {
        Python::with_gil(|py| CPythonModule(self.0.clone_ref(py)))
    }
}

impl Display for Container<CPythonModule> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "<module '{}' (built-in)>", self.borrow().name())
    }
}

impl CPythonModule {
    pub fn new(name: &str) -> Self {
        pyo3::prepare_freethreaded_python();
        let pymodule = Python::with_gil(|py| {
            py.import(name)
                .unwrap_or_else(|_| panic!("Failed to import CPython module '{}'", name))
                .into()
        });

        Self(pymodule)
    }

    fn name(&self) -> String {
        Python::with_gil(|py| {
            self.get_item(Dunder::Name.into_pyobject(py).unwrap())
                .unwrap()
                .unwrap()
                .as_string()
                .unwrap()
        })
    }

    fn get_item(&self, name: Bound<PyString>) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(Python::with_gil(|py| match self.0.bind(py).getattr(name) {
            Ok(py_attr) => Some(utils::from_pyobject(py, py_attr)),
            Err(_) => None,
        }))
    }
}

impl MemberReader for CPythonModule {
    fn get_member(
        &self,
        _interpreter: &Interpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Python::with_gil(|py| self.get_item(name.into_pyobject(py).unwrap()))
    }

    fn dir(&self) -> Vec<String> {
        Python::with_gil(|py| {
            let py_list = self.0.bind(py).dir().unwrap();

            py_list
                .iter()
                .map(|item| item.extract::<String>().unwrap())
                .collect()
        })
    }
}

impl Callable for CPythonObject {
    fn call(&self, interpreter: &Interpreter, args: Arguments) -> TreewalkResult<TreewalkValue> {
        Python::with_gil(|py| {
            let py_attr = self.0.bind(py);
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
                Err(interpreter.name_error(self.name()))
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

impl<'py> IntoPyObject<'py> for TreewalkValue {
    type Target = PyAny;
    type Output = Bound<'py, Self::Target>;
    type Error = std::convert::Infallible;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        let o = match self {
            TreewalkValue::None => py.None().as_any().bind(py).to_owned(),
            TreewalkValue::Boolean(b) => b.into_pyobject(py)?.into_bound().as_any().to_owned(),
            TreewalkValue::String(s) => s.as_str().into_pyobject(py)?.as_any().to_owned(),
            _ => {
                dbg!(&self);
                todo!()
            }
        };

        Ok(o)
    }
}

// This code is left over from pyo3 v0.22. When we upgrade it to use IntoPyObject, we should lock
// this down with unit tests.
//
// impl ToPyObject for TreewalkValue {
//     fn to_object(&self, py: Python) -> PyObject {
//         match self {
//             TreewalkValue::Integer(i) => i.to_object(py),
//             TreewalkValue::List(l) => PyList::new(py, l.clone()).unwrap().to_object(py),
//             TreewalkValue::Function(_) => {
//                 // TODO our PyCFunction implementation is a no-op, we need to find a way to pass
//                 // the interpreter into here.
//                 let callback = |_args: &Bound<'_, PyTuple>,
//                                 _kwargs: Option<&Bound<'_, PyDict>>|
//                  -> PyResult<bool> {
//                     log(LogLevel::Warn, || {
//                         "Potentially lossy PyCFunction invocation.".to_string()
//                     });
//                     Ok(true)
//                 };
//                 // TODO use real function name
//                 let py_cfunc = PyCFunction::new_closure_bound(py, None, None, callback).unwrap();
//                 py_cfunc.to_object(py)
//             }
//             TreewalkValue::Class(_) => {
//                 // TODO same here, our PyClass implementation does bring real fields
//                 Py::new(py, TestClass {}).unwrap().to_object(py)
//             }
//             TreewalkValue::Module(module) => {
//                 let name = PyString::new(py, module.borrow().name());
//                 let types = py.import_bound("types").unwrap();
//                 let module_type = types.getattr("ModuleType").unwrap();
//                 let args = PyTuple::new(py, &[name]).unwrap();
//                 let py_module_obj = module_type.call1(args).unwrap();
//                 let py_module = py_module_obj.downcast().unwrap();
//
//                 // Flatten all key-value pairs from scope into the module
//                 for (key, value) in module.borrow().dict() {
//                     PyModuleMethods::add(py_module, key.as_str(), value.to_object(py)).unwrap();
//                 }
//
//                 py_module.to_object(py)
//             }
//             TreewalkValue::CPythonModule(module) => module.borrow().0.to_object(py),
//             TreewalkValue::CPythonObject(object) => object.0.to_object(py),
//             _ => unimplemented!(
//                 "Attempting to convert {} to a PyObject, but {} conversion is not implemented!",
//                 self,
//                 self.get_type()
//             ),
//         }
//     }
// }

pub struct CPythonClass(PyObject);

impl Clone for CPythonClass {
    fn clone(&self) -> Self {
        Python::with_gil(|py| CPythonClass(self.0.clone_ref(py)))
    }
}

pub struct CPythonObject(PyObject);

impl Clone for CPythonObject {
    fn clone(&self) -> Self {
        Python::with_gil(|py| CPythonObject(self.0.clone_ref(py)))
    }
}

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
    pub fn get_type(&self) -> TreewalkValue {
        Python::with_gil(|py| {
            let obj_ref = self.0.bind(py);
            let obj_type = obj_ref.getattr(Dunder::Class).unwrap();
            TreewalkValue::CPythonClass(CPythonClass(obj_type.into()))
        })
    }

    pub fn hasattr(&self, attr: Dunder) -> bool {
        Python::with_gil(|py| {
            let attr = attr.into_pyobject(py).unwrap();
            utils::hasattr(py, self.0.bind(py), attr).unwrap()
        })
    }
}

impl IndexRead for CPythonObject {
    fn getitem(
        &self,
        _interpreter: &Interpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Python::with_gil(|py| {
            let key = index.into_pyobject(py).unwrap();
            let result = self
                .0
                .bind(py)
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
        index: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        Python::with_gil(|py| {
            let key = index.into_pyobject(py).unwrap();
            let value = value.into_pyobject(py).unwrap();
            self.0
                .bind(py)
                .call_method1(Dunder::SetItem, (key, value))
                .unwrap();
        });

        Ok(())
    }

    fn delitem(&mut self, _interpreter: &Interpreter, index: TreewalkValue) -> TreewalkResult<()> {
        Python::with_gil(|py| {
            let key = index.into_pyobject(py).unwrap();
            self.0
                .bind(py)
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

    pub fn from_pyobject(py: Python, py_obj: Bound<PyAny>) -> TreewalkValue {
        if let Ok(value) = py_obj.extract::<i64>() {
            TreewalkValue::Integer(value)
        } else if let Ok(value) = py_obj.extract::<f64>() {
            TreewalkValue::FloatingPoint(value)
        } else if let Ok(value) = py_obj.extract::<&str>() {
            TreewalkValue::String(Str::new(value.to_string()))
        } else if let Ok(py_tuple) = py_obj.extract::<Bound<PyTuple>>() {
            let elements = py_tuple
                .iter()
                .map(|item| from_pyobject(py, item))
                .collect();
            TreewalkValue::Tuple(Tuple::new(elements))
        } else if let Ok(py_module) = py_obj.extract::<Bound<PyModule>>() {
            let mut module = Module::default();

            // Get the module's __dict__ to iterate over all attributes
            for (key, value) in py_module.dict() {
                let key_str: String = key.extract().expect("Key is not a string");
                let expr_value = from_pyobject(py, value);
                module.insert(&key_str, expr_value);
            }

            TreewalkValue::Module(Container::new(module))
        } else if let Ok(py_set) = py_obj.extract::<Bound<PySet>>() {
            #[allow(clippy::mutable_key_type)]
            let elements = py_set.iter().map(|item| from_pyobject(py, item)).collect();
            TreewalkValue::Set(Container::new(Set::new(elements)))
        } else if let Ok(py_list) = py_obj.extract::<Bound<PyList>>() {
            let elements = py_list.iter().map(|item| from_pyobject(py, item)).collect();
            TreewalkValue::List(Container::new(List::new(elements)))
        } else {
            // TODO think of a way to detect whether this is an object we can convert or not
            // log(LogLevel::Warn, || {
            //     "Potentially ambiguous CPythonObject instance.".to_string()
            // });
            TreewalkValue::CPythonObject(CPythonObject::new(
                py_obj.into_pyobject(py).unwrap().into(),
            ))
        }
    }

    pub fn to_args(py: Python, args: Arguments) -> Bound<PyTuple> {
        let args = args
            .iter_args()
            .map(|a| a.clone().into_pyobject(py).unwrap())
            .collect::<Vec<_>>();
        PyTuple::new(py, args).unwrap()
    }

    pub fn hasattr(py: Python, obj: &Bound<PyAny>, attr: Bound<PyString>) -> PyResult<bool> {
        let builtins = py.import("builtins")?;
        let hasattr = builtins.getattr("hasattr")?.into_pyobject(py)?;

        let has_setitem = hasattr.call1((obj, attr))?;
        let has_setitem_bool = has_setitem.extract::<Bound<PyBool>>()?;
        Ok(has_setitem_bool.is_true())
    }
}
