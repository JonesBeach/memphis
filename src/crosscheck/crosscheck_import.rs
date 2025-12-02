use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn regular_import_not_found() {
    let e = crosscheck_expect_error!(
        r#"
import unknown
"#
    );

    assert_import_error!(e, "No module named unknown");
}
