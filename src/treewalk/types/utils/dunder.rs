use std::fmt::{Display, Error, Formatter};

#[derive(PartialEq, Clone)]
pub enum Dunder {
    Main,
    // Methods
    New,
    Init,
    Contains,
    Eq,
    Ne,
    Enter,
    Exit,
    Get,
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

static DUNDER_MAPPINGS: &[(Dunder, &str)] = &[
    (Dunder::Main, "__main__"),
    (Dunder::New, "__new__"),
    (Dunder::Init, "__init__"),
    (Dunder::Contains, "__contains__"),
    (Dunder::Eq, "__eq__"),
    (Dunder::Ne, "__ne__"),
    (Dunder::Enter, "__enter__"),
    (Dunder::Exit, "__exit__"),
    (Dunder::Get, "__get__"),
    (Dunder::GetItem, "__getitem__"),
    (Dunder::SetItem, "__setitem__"),
    (Dunder::DelItem, "__delitem__"),
    (Dunder::Code, "__code__"),
    (Dunder::Globals, "__globals__"),
    (Dunder::Closure, "__closure__"),
    (Dunder::Module, "__module__"),
    (Dunder::Doc, "__doc__"),
    (Dunder::Name, "__name__"),
    (Dunder::Qualname, "__qualname__"),
    (Dunder::Annotations, "__annotations__"),
    (Dunder::TypeParams, "__type_params__"),
    (Dunder::Dict, "__dict__"),
    (Dunder::Mro, "__mro__"),
    (Dunder::Str, "__str__"),
    (Dunder::Traceback, "__traceback__"),
    #[cfg(feature = "c_stdlib")]
    (Dunder::Class, "__class__"),
];

impl Dunder {
    pub fn value(&self) -> &'static str {
        DUNDER_MAPPINGS
            .iter()
            .find_map(
                |(variant, name)| {
                    if variant == self {
                        Some(name)
                    } else {
                        None
                    }
                },
            )
            .expect("Invalid Dunder variant")
    }
}

impl Display for Dunder {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.value())
    }
}

impl From<&str> for Dunder {
    fn from(s: &str) -> Self {
        DUNDER_MAPPINGS
            .iter()
            .find_map(|(variant, name)| {
                if *name == s {
                    Some(variant.to_owned())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("Unknown dunder method: {}", s))
    }
}

impl From<Dunder> for String {
    fn from(value: Dunder) -> Self {
        value.value().to_string()
    }
}
