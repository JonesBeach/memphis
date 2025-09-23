use std::{
    fmt::{Display, Error, Formatter},
    ops::Deref,
};

/// Represents Python's "dunder" (double underscore) methods and attributes.
///
/// This enum provides a convenient way to reference and work with Python's special
/// methods and attributes, commonly referred to as "magic methods" or "dunder methods."
#[derive(PartialEq, Debug, Clone)]
pub enum Dunder {
    // Special methods
    Main,
    New,
    Init,
    Contains,
    Add,
    Sub,
    Mul,
    Truediv,
    Floordiv,
    Mod,
    And,
    Or,
    Xor,
    Lshift,
    Rshift,
    Pow,
    Le,
    Lt,
    Ge,
    Gt,
    Eq,
    Ne,
    Hash,
    Enter,
    Exit,
    Get,
    Set,
    Delete,
    GetItem,
    SetItem,
    DelItem,
    // Special attributes
    Code,
    Globals,
    Closure,
    Module,
    Doc,
    Name,
    Qualname,
    Annotations,
    TypeParams,
    Dict,
    Mro,
    Str,
    Traceback,
    Class,
    Builtins,
}

impl Dunder {
    /// Returns the string representation of the dunder name (e.g., `__init__`).
    fn value(&self) -> &'static str {
        match self {
            Dunder::Main => "__main__",
            Dunder::New => "__new__",
            Dunder::Init => "__init__",
            Dunder::Contains => "__contains__",
            Dunder::Eq => "__eq__",
            Dunder::Ne => "__ne__",
            Dunder::Add => "__add__",
            Dunder::Sub => "__sub__",
            Dunder::Mul => "__mul__",
            Dunder::Truediv => "__truediv__",
            Dunder::Floordiv => "__floordiv__",
            Dunder::Mod => "__mod__",
            Dunder::And => "__and__",
            Dunder::Or => "__or__",
            Dunder::Xor => "__xor__",
            Dunder::Lshift => "__lshift__",
            Dunder::Rshift => "__rshift__",
            Dunder::Pow => "__pow__",
            Dunder::Lt => "__lt__",
            Dunder::Le => "__le__",
            Dunder::Gt => "__gt__",
            Dunder::Ge => "__ge__",
            Dunder::Hash => "__hash__",
            Dunder::Enter => "__enter__",
            Dunder::Exit => "__exit__",
            Dunder::Get => "__get__",
            Dunder::Set => "__set__",
            Dunder::Delete => "__delete__",
            Dunder::GetItem => "__getitem__",
            Dunder::SetItem => "__setitem__",
            Dunder::DelItem => "__delitem__",
            Dunder::Code => "__code__",
            Dunder::Globals => "__globals__",
            Dunder::Closure => "__closure__",
            Dunder::Module => "__module__",
            Dunder::Doc => "__doc__",
            Dunder::Name => "__name__",
            Dunder::Qualname => "__qualname__",
            Dunder::Annotations => "__annotations__",
            Dunder::TypeParams => "__type_params__",
            Dunder::Dict => "__dict__",
            Dunder::Mro => "__mro__",
            Dunder::Str => "__str__",
            Dunder::Traceback => "__traceback__",
            Dunder::Class => "__class__",
            Dunder::Builtins => "__builtins__",
        }
    }
}

impl Display for Dunder {
    /// Formats the dunder value for display.
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.value())
    }
}

impl Deref for Dunder {
    type Target = str;

    /// Allows dereferencing to the string representation of the dunder name.
    fn deref(&self) -> &Self::Target {
        self.value()
    }
}

impl AsRef<str> for Dunder {
    fn as_ref(&self) -> &str {
        self.value()
    }
}

#[cfg(feature = "c_stdlib")]
mod cpython {
    use super::*;

    use pyo3::{types::PyString, Bound, IntoPyObject, Python};

    impl<'py> IntoPyObject<'py> for Dunder {
        type Target = PyString;
        type Output = Bound<'py, Self::Target>;
        type Error = std::convert::Infallible;

        fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
            self.value().into_pyobject(py)
        }
    }
}

impl From<Dunder> for String {
    /// Converts a `Dunder` value into its string representation.
    fn from(value: Dunder) -> Self {
        value.value().to_string()
    }
}

impl From<Dunder> for &str {
    /// Converts a `Dunder` value into a static string slice.
    fn from(value: Dunder) -> Self {
        value.value()
    }
}

impl PartialEq<&str> for Dunder {
    fn eq(&self, other: &&str) -> bool {
        // this is a bit cryptic but avoid an extra allocation
        **self == **other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dunder_values() {
        assert_eq!(Dunder::Init.value(), "__init__");
        assert_eq!(Dunder::Main.to_string(), "__main__");
    }

    #[test]
    fn test_dunder_display() {
        let dunder = Dunder::New;
        assert_eq!(format!("{}", dunder), "__new__");
    }

    #[test]
    fn test_deref_trait() {
        let dunder = Dunder::Init;
        // Use deref to access the underlying string
        assert_eq!(&*dunder, "__init__");
        assert_eq!(dunder.deref(), "__init__");
        // Ensure deref works in string-like operations
        assert!(dunder.starts_with("__"));
        assert!(dunder.ends_with("__"));
    }

    #[test]
    fn test_as_ref_trait() {
        let dunder = Dunder::Eq;
        assert_eq!(dunder.as_ref(), "__eq__");
    }

    #[test]
    fn test_as_ref_in_generic_context() {
        fn use_as_ref<'a, S: AsRef<str> + 'a>(s: &'a S) -> &'a str {
            s.as_ref()
        }

        let dunder = Dunder::Contains;
        assert_eq!(use_as_ref(&dunder), "__contains__");
    }

    #[test]
    fn test_from_dunder_to_string() {
        let dunder = Dunder::Main;
        let converted: String = dunder.clone().into();
        assert_eq!(converted, "__main__");

        // Ensure the original `Dunder` instance is unaffected
        assert_eq!(dunder.value(), "__main__");
    }

    #[test]
    fn test_from_dunder_to_str() {
        let dunder = Dunder::Contains;
        let converted: &str = dunder.into();
        assert_eq!(converted, "__contains__");
    }
}
