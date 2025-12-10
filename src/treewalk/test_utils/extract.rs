macro_rules! extract {
    ($ctx:expr, $input:expr, $variant:ident) => {{
        match read(&$ctx, $input) {
            crate::treewalk::TreewalkValue::$variant(v) => v,
            _ => panic!("Expected {}: {}", stringify!($variant), $input),
        }
    }};
}

macro_rules! extract_member {
    ($ctx:expr, $input:expr, $field:expr) => {{
        use crate::treewalk::protocols::MemberRead;
        extract!($ctx, $input, Object)
            .get_member($ctx.interpreter(), $field)
            .expect(&format!("Failed to get field: {}", $field))
            .expect(&format!("Failed to get field: {}", $field))
    }};
    ($ctx:expr, $input:expr, $field:expr, $pattern:ident) => {{
        use crate::treewalk::protocols::MemberRead;
        extract!($ctx, $input, $pattern)
            .get_member($ctx.interpreter(), $field)
            .expect(&format!("Failed to get field: {}", $field))
            .expect(&format!("Failed to get field: {}", $field))
    }};
}

pub(crate) use extract;
pub(crate) use extract_member;
