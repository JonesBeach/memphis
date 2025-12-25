use crate::{
    parser::types::{Expr, LoopIndex},
    treewalk::{
        result::Raise, types::Exception, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    /// Assignment functionality shared by traditional assignment such as `a = 1` and compound
    /// assignment such as `a += 1`.
    pub fn execute_assignment(&self, name: &Expr, value: TreewalkValue) -> TreewalkResult<()> {
        match name {
            Expr::Variable(name) => {
                self.state.write(name.as_str(), value);
            }
            Expr::IndexAccess { object, index } => {
                let index_result = self.evaluate_expr(index)?;
                let object_result = self.evaluate_expr(object)?;
                object_result
                    .clone()
                    .into_index_write(self)?
                    .ok_or_else(|| {
                        Exception::type_error(format!(
                            "'{}' object does not support item assignment",
                            object_result.get_type()
                        ))
                    })
                    .raise(self)?
                    .setitem(self, index_result, value)?;
            }
            Expr::MemberAccess { object, field } => {
                let result = self.evaluate_expr(object)?;
                result
                    .clone()
                    .into_member_writer()
                    .ok_or_else(|| {
                        Exception::attribute_error(result.class_name(self), field.as_str())
                    })
                    .raise(self)?
                    .set_member(self, field.as_str(), value)?;
            }
            _ => return Exception::type_error("cannot assign").raise(self),
        }

        Ok(())
    }

    pub fn execute_loop_index_assignment(
        &self,
        index: &LoopIndex,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        match index {
            LoopIndex::Variable(var) => {
                self.state.write(var.as_str(), value);
            }
            LoopIndex::Tuple(tuple_index) => {
                for (key, value) in tuple_index.iter().zip(value.as_iterable().raise(self)?) {
                    self.state.write(key.as_str(), value);
                }
            }
        };

        Ok(())
    }
}
