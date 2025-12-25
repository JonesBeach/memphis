use crate::{
    core::Container,
    domain::ModuleName,
    treewalk::{
        types::{Exception, Module},
        DomainResult, TreewalkValue,
    },
};

/// Construct a module chain given a `ModuleName` and a leaf `TreewalkValue`.
///
/// For example:
/// ```python
/// import mypackage.myothermodule
/// ```
///
/// must be used as
/// ```python
/// mypackage.myothermodule.add('1', '1')
/// ```
/// `myothermodule` must become a submodule of `mypackage`.
pub fn build_module_chain(
    full_name: &ModuleName,
    leaf_value: TreewalkValue,
) -> DomainResult<TreewalkValue> {
    let mut inner = leaf_value;
    let mut child_key = full_name
        .tail()
        .ok_or_else(Exception::runtime_error)?
        .to_owned(); // last segment, e.g. "d"

    // parents() yields:
    // ["a.b.c", "a.b", "a"]
    for parent_name in full_name.parents() {
        let mut outer = Module::new_empty(parent_name.clone());

        // insert inner module under the child's name
        outer.insert(&child_key, inner);

        inner = TreewalkValue::Module(Container::new(outer));

        // Next iteration the child key becomes this parent's last segment
        child_key = parent_name
            .tail()
            .ok_or_else(Exception::runtime_error)?
            .to_owned();
    }

    Ok(inner)
}
