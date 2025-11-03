use crate::domain::ExecutionError;

// local semantic errors, used in lower levels of the code
pub type DomainResult<T> = Result<T, ExecutionError>;
