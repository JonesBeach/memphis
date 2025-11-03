use crate::{
    domain::{DomainResult, Dunder, ExecutionError},
    treewalk::{
        protocols::{IndexRead, IndexWrite, MemberRead, MemberWrite},
        type_system::{
            CloneableCallable, CloneableDataDescriptor, CloneableIterable,
            CloneableNonDataDescriptor,
        },
        utils::BuiltinObject,
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl TreewalkValue {
    pub fn into_member_reader(self, interpreter: &TreewalkInterpreter) -> Box<dyn MemberRead> {
        match self {
            TreewalkValue::Object(i) => Box::new(i),
            TreewalkValue::Class(i) => Box::new(i),
            TreewalkValue::Function(i) => Box::new(i),
            TreewalkValue::Cell(i) => Box::new(i.borrow().clone()),
            TreewalkValue::Module(i) => Box::new(i.borrow().clone()),
            TreewalkValue::Super(i) => Box::new(i),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(i) => Box::new(i.borrow().clone()),
            _ => {
                // We need this fallback case for instances of builtin types.
                // i.e. [].append
                // All attributes fetched off the builtin types not explicitly handled above do not
                // support attribute writes, only reads of builtin attributes.
                let class = interpreter.state.class_of_type(&self.get_type());
                Box::new(BuiltinObject::new(self, class))
            }
        }
    }

    pub fn into_member_writer(self) -> Option<Box<dyn MemberWrite>> {
        match self {
            TreewalkValue::Object(i) => Some(Box::new(i)),
            TreewalkValue::Class(i) => Some(Box::new(i)),
            TreewalkValue::Function(i) => Some(Box::new(i)),
            // #[cfg(feature = "c_stdlib")]
            // TreewalkValue::CPythonModule(i) => Some(Box::new(i.borrow().clone())),
            _ => None,
        }
    }

    pub fn into_index_read(
        self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Option<Box<dyn IndexRead>>> {
        let result: Box<dyn IndexRead> = match self {
            TreewalkValue::List(list) => Box::new(list),
            TreewalkValue::Tuple(tuple) => Box::new(tuple),
            TreewalkValue::Dict(dict) => Box::new(dict),
            TreewalkValue::MappingProxy(proxy) => Box::new(proxy),
            TreewalkValue::Str(s) => Box::new(s),
            TreewalkValue::Object(ref i) => {
                if self.hasattr(interpreter, Dunder::GetItem)? {
                    Box::new(i.clone()) as Box<dyn IndexRead>
                } else {
                    return Ok(None);
                }
            }
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonObject(o) => match o.hasattr(Dunder::GetItem) {
                true => Box::new(o.clone()),
                false => return Ok(None),
            },
            _ => return Ok(None),
        };

        Ok(Some(result))
    }

    pub fn into_index_write(
        self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Option<Box<dyn IndexWrite>>> {
        let result: Box<dyn IndexWrite> = match self {
            TreewalkValue::List(list) => Box::new(list),
            TreewalkValue::Dict(dict) => Box::new(dict),
            TreewalkValue::Object(ref i) => {
                if self.hasattr(interpreter, Dunder::SetItem)? {
                    Box::new(i.clone()) as Box<dyn IndexWrite>
                } else {
                    return Ok(None);
                }
            }
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonObject(o) => match o.hasattr(Dunder::SetItem) {
                true => Box::new(o.clone()),
                false => return Ok(None),
            },
            _ => return Ok(None),
        };

        Ok(Some(result))
    }

    pub fn into_nondata_descriptor(
        self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Option<Box<dyn CloneableNonDataDescriptor>>> {
        let result: Box<dyn CloneableNonDataDescriptor> = match self {
            TreewalkValue::NonDataDescriptor(i) => i,
            TreewalkValue::Object(ref i) => {
                if self.hasattr(interpreter, Dunder::Get)? {
                    Box::new(i.clone()) as Box<dyn CloneableNonDataDescriptor>
                } else {
                    return Ok(None);
                }
            }
            TreewalkValue::Classmethod(i) => Box::new(i),
            TreewalkValue::Staticmethod(i) => Box::new(i),
            TreewalkValue::Property(i) => Box::new(i),
            _ => return Ok(None),
        };

        Ok(Some(result))
    }

    pub fn into_data_descriptor(
        self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Option<Box<dyn CloneableDataDescriptor>>> {
        let result = match self {
            TreewalkValue::Object(ref i) => {
                if self.hasattr(interpreter, Dunder::Set)? {
                    Box::new(i.clone()) as Box<dyn CloneableDataDescriptor>
                } else {
                    return Ok(None);
                }
            }
            TreewalkValue::DataDescriptor(i) => i,
            // TODO handle property here
            _ => return Ok(None),
        };

        Ok(Some(result))
    }

    /// Ensure this value *is already* an iterator (e.g. result of a prior `iter()` call).
    ///
    /// If not, raise a TypeError. This does not attempt to coerce an iterable into an iterator.
    /// Use when `next()` is called directly — `next(x)` must fail if `x` is not an iterator.
    #[allow(clippy::wrong_self_convention)]
    pub fn as_iterator_strict(self) -> DomainResult<Box<dyn CloneableIterable>> {
        let result: Box<dyn CloneableIterable> = match self {
            TreewalkValue::ListIter(i) => Box::new(i),
            TreewalkValue::SetIter(i) => Box::new(i),
            TreewalkValue::TupleIter(i) => Box::new(i),
            TreewalkValue::RangeIter(i) => Box::new(i),
            TreewalkValue::StrIter(i) => Box::new(i),
            TreewalkValue::ReversedIter(i) => Box::new(i),
            TreewalkValue::DictItemsIter(i) => Box::new(i),
            TreewalkValue::DictKeysIter(i) => Box::new(i),
            TreewalkValue::DictValuesIter(i) => Box::new(i),
            TreewalkValue::Generator(i) => Box::new(i),
            TreewalkValue::Zip(i) => Box::new(i),
            _ => {
                return Err(ExecutionError::type_error(format!(
                    "'{}' object is not an iterator",
                    self.get_type()
                )))
            }
        };

        Ok(result)
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn as_callable(self) -> DomainResult<Box<dyn CloneableCallable>> {
        let result: Box<dyn CloneableCallable> = match self {
            TreewalkValue::Function(i) => Box::new(i),
            TreewalkValue::Method(i) => Box::new(i),
            TreewalkValue::BuiltinMethod(i) => i,
            TreewalkValue::BuiltinFunction(i) => i,
            TreewalkValue::Class(i) => Box::new(i),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonObject(i) => Box::new(i),
            _ => return Err(ExecutionError::type_error("Expected a callable")),
        };

        Ok(result)
    }

    fn hasattr(&self, interpreter: &TreewalkInterpreter, attr: Dunder) -> TreewalkResult<bool> {
        let result = self
            .clone()
            .into_member_reader(interpreter)
            .get_member(interpreter, &attr)?
            .is_some();
        Ok(result)
    }
}
