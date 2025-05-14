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
            $crate::errors::MemphisError::Execution(e) => e,
            _ => panic!("Expected an execution error."),
        }
    }};
}

macro_rules! assert_crosscheck_eq {
    ($session:expr, $name:expr, $expected:expr) => {{
        let actual = $session
            .read($name)
            .expect(&format!("Variable not found: {}", $name));
        assert_eq!(actual, $expected);
    }};
}

macro_rules! assert_crosscheck_return {
    ($src:expr, $expected:expr) => {{
        let mut session =
            $crate::crosscheck::CrosscheckSession::new($crate::domain::Source::from_text($src));
        let (tw_val, vm_val) = session.eval();
        assert_eq!(
            tw_val, $expected,
            "Treewalk return value did not match expected"
        );
        assert_eq!(
            vm_val, $expected,
            "Bytecode VM return value did not match expected"
        );
    }};
}

pub(crate) use assert_crosscheck_eq;
pub(crate) use assert_crosscheck_return;
pub(crate) use crosscheck_eval;
pub(crate) use crosscheck_expect_error;
