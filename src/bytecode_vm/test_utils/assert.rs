macro_rules! assert_eval_eq {
    ($input:expr, $expected:expr) => {
        assert_eq!(eval($input), $expected);
    };
}

macro_rules! assert_read_eq {
    ($ctx:expr, $input:expr, $expected:expr) => {
        assert_eq!(read(&mut $ctx, $input), $expected);
    };
}

macro_rules! assert_member_eq {
    ($ctx:expr, $obj:expr, $field:expr, $expected:expr) => {
        assert_eq!(extract_member!($ctx, $obj, $field), $expected);
    };
}

pub(crate) use assert_eval_eq;
pub(crate) use assert_member_eq;
pub(crate) use assert_read_eq;
