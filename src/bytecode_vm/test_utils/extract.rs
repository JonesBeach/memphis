macro_rules! extract {
    ($ctx:expr, $input:expr, $variant:ident) => {{
        match read(&mut $ctx, $input) {
            VmValue::$variant(v) => v,
            _ => panic!("Expected {}: {}", stringify!($variant), $input),
        }
    }};
}

macro_rules! extract_member {
    ($ctx:expr, $obj:expr, $field:expr) => {{
        read_attr(&mut $ctx, $obj, $field)
    }};
}

pub(crate) use extract;
pub(crate) use extract_member;
