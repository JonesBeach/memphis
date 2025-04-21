macro_rules! assert_eval_eq {
    ($input:expr, $expected:expr) => {
        assert_eq!(eval($input), $expected);
    };
}

macro_rules! assert_eval_variant {
    ($input:expr, $pattern:ident) => {
        assert!(matches!(
            eval($input),
            crate::treewalk::TreewalkValue::$pattern(_)
        ));
    };
}

macro_rules! assert_read_eq {
    ($ctx:expr, $input:expr, $expected:expr) => {
        assert_eq!(read(&$ctx, $input), $expected);
    };
}

macro_rules! assert_type_eq {
    ($ctx:expr, $name:expr, $type:expr) => {
        assert_eq!(
            extract!(&$ctx, $name, Class).borrow().builtin_type(),
            &$type
        );
    };
}

macro_rules! assert_variant {
    ($ctx:expr, $input:expr, $pattern:ident) => {
        assert!(matches!(
            read(&$ctx, $input),
            crate::treewalk::TreewalkValue::$pattern(_)
        ));
    };
}

macro_rules! assert_member_eq {
    ($ctx:expr, $input:expr, $field:expr, $expected:expr) => {
        assert_eq!(extract_member!($ctx, $input, $field), $expected);
    };
}

pub(crate) use assert_eval_eq;
pub(crate) use assert_eval_variant;
pub(crate) use assert_member_eq;
pub(crate) use assert_read_eq;
pub(crate) use assert_type_eq;
pub(crate) use assert_variant;
