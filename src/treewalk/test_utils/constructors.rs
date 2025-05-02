macro_rules! none {
    () => {
        crate::treewalk::TreewalkValue::None
    };
}

macro_rules! str {
    ($input:expr) => {
        crate::treewalk::TreewalkValue::Str(crate::treewalk::types::Str::new($input.to_string()))
    };
}

macro_rules! int {
    ($val:expr) => {
        crate::treewalk::TreewalkValue::Integer($val)
    };
}

macro_rules! float {
    ($val:expr) => {
        crate::treewalk::TreewalkValue::Float($val)
    };
}

macro_rules! complex {
    ($real:expr, $imag:expr) => {
        crate::treewalk::TreewalkValue::Complex(crate::treewalk::types::Complex::new($real, $imag))
    };
}

macro_rules! bool {
    ($val:expr) => {
        crate::treewalk::TreewalkValue::Boolean($val)
    };
}

macro_rules! list {
    ($($expr:expr),* $(,)?) => {
        crate::treewalk::TreewalkValue::List(crate::core::Container::new(crate::treewalk::types::List::new(vec![
            $($expr),*
        ])))
    };
}

macro_rules! tuple {
    ($($expr:expr),* $(,)?) => {
        crate::treewalk::TreewalkValue::Tuple(crate::treewalk::types::Tuple::new(vec![$($expr),*]))
    };
}

macro_rules! set {
    ($($expr:expr),* $(,)?) => {
        crate::treewalk::TreewalkValue::Set(crate::core::Container::new(crate::treewalk::types::Set::new(std::collections::HashSet::from([
            $($expr),*
        ]))))
    };
}

macro_rules! frozenset {
    ($($expr:expr),* $(,)?) => {
        crate::treewalk::TreewalkValue::FrozenSet(crate::treewalk::types::FrozenSet::new(std::collections::HashSet::from([
            $($expr),*
        ])))
    };
}

macro_rules! bytearray {
    () => {
        crate::treewalk::TreewalkValue::ByteArray(crate::core::Container::new(
            crate::treewalk::types::ByteArray::new("".into()),
        ))
    };
    ($input:expr) => {
        crate::treewalk::TreewalkValue::ByteArray(crate::core::Container::new(
            crate::treewalk::types::ByteArray::new($input.into()),
        ))
    };
}

macro_rules! bytes {
    () => {
        crate::treewalk::TreewalkValue::Bytes("".into())
    };
    ($input:expr) => {
        crate::treewalk::TreewalkValue::Bytes($input.into())
    };
}

macro_rules! dict {
    ($interp:expr, { $($key:expr => $value:expr),* $(,)? }) => {
        crate::treewalk::TreewalkValue::Dict(crate::core::Container::new(crate::treewalk::types::Dict::new(
            $interp,
            vec![
                $(($key, $value)),*
            ].into_iter().collect()
        )))
    };
}

macro_rules! dict_items {
    () => {
        crate::treewalk::TreewalkValue::DictItems(crate::treewalk::types::DictItems::default())
    };
    ($interp:expr, $expr:expr) => {
        crate::treewalk::TreewalkValue::DictItems(crate::treewalk::types::DictItems::new(
            $interp, $expr,
        ))
    };
}

macro_rules! dict_keys {
    ($($expr:expr),* $(,)?) => {
        crate::treewalk::TreewalkValue::DictKeys(crate::treewalk::types::DictKeys::new(vec![
            $($expr),*
        ]))
    };
}

macro_rules! dict_values {
    ($($expr:expr),* $(,)?) => {
        crate::treewalk::TreewalkValue::DictValues(crate::treewalk::types::DictValues::new(vec![
            $($expr),*
        ]))
    };
}

pub(crate) use bool;
pub(crate) use bytearray;
pub(crate) use bytes;
pub(crate) use complex;
pub(crate) use dict;
pub(crate) use dict_items;
pub(crate) use dict_keys;
pub(crate) use dict_values;
pub(crate) use float;
pub(crate) use frozenset;
pub(crate) use int;
pub(crate) use list;
pub(crate) use none;
pub(crate) use set;
pub(crate) use str;
pub(crate) use tuple;
