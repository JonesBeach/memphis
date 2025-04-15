macro_rules! crosscheck_eval {
    ($src:expr) => {{
        $crate::crosscheck::CrosscheckSession::new($crate::domain::Source::from_text($src))
            .run()
            .expect("Crosscheck session failed")
    }};
}

macro_rules! crosscheck_expect_error {
    ($src:expr) => {{
        match $crate::crosscheck::CrosscheckSession::new($crate::domain::Source::from_text($src))
            .run_expect_error()
        {
            $crate::MemphisError::Execution(e) => e,
            _ => panic!("Expected an execution error."),
        }
    }};
}

macro_rules! assert_crosscheck_eq {
    ($session:expr, $name:expr, $expected:expr) => {{
        let actual = $session.read($name).expect("Symbol not found");
        assert_eq!(actual, $expected);
    }};
}

macro_rules! assert_crosscheck_return {
    ($src:expr, $expected:expr) => {{
        let mut session =
            $crate::crosscheck::CrosscheckSession::new($crate::domain::Source::from_text($src));
        let actual = session.eval_expect_val();
        assert_eq!(actual, $expected, "Return value did not match expected");
    }};
}

pub(crate) use assert_crosscheck_eq;
pub(crate) use assert_crosscheck_return;
pub(crate) use crosscheck_eval;
pub(crate) use crosscheck_expect_error;
