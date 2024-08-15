use std::{
    fmt::{Display, Error, Formatter},
    ops::Deref,
};

#[derive(PartialEq, Clone)]
pub enum Dunder {
    Main,
    // Methods
    New,
    Init,
    Contains,
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
    // Attributes
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
    #[cfg(feature = "c_stdlib")]
    Class,
}

impl Dunder {
    fn value(&self) -> &'static str {
        match self {
            Dunder::Main => "__main__",
            Dunder::New => "__new__",
            Dunder::Init => "__init__",
            Dunder::Contains => "__contains__",
            Dunder::Eq => "__eq__",
            Dunder::Ne => "__ne__",
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
            #[cfg(feature = "c_stdlib")]
            Dunder::Class => "__class__",
        }
    }
}

impl Display for Dunder {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.value())
    }
}

impl Deref for Dunder {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.value()
    }
}

impl From<Dunder> for String {
    fn from(value: Dunder) -> Self {
        value.value().to_string()
    }
}
