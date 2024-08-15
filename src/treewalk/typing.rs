use crate::parser::types::TypeNode;

/// Represents the optional type hints found in some Python programs.
#[derive(Debug, Clone)]
pub enum TypeExpr {
    Generic {
        base: Box<TypeExpr>,
        parameters: Vec<TypeExpr>,
    },
    Union(Vec<TypeExpr>),
    Ellipsis,
    Int,
    List,
    Str,
}

/// Convert from `TypeNode`, which is used by the parser, to `TypeExpr`, which is used by the
/// interpreter.
fn convert_to_type_expr(type_node: &TypeNode) -> TypeExpr {
    match type_node {
        TypeNode::Generic {
            base_type,
            parameters,
        } => {
            let base_expr = match base_type.as_str() {
                "list" => TypeExpr::List,
                _ => unimplemented!(),
            };

            let param_exprs = parameters.iter().map(convert_to_type_expr).collect();

            TypeExpr::Generic {
                base: Box::new(base_expr),
                parameters: param_exprs,
            }
        }
        TypeNode::Union(parameters) => {
            let param_exprs = parameters.iter().map(convert_to_type_expr).collect();

            TypeExpr::Union(param_exprs)
        }
        TypeNode::Basic(type_str) => match type_str.as_str() {
            "int" => TypeExpr::Int,
            "str" => TypeExpr::Str,
            "..." => TypeExpr::Ellipsis,
            _ => unimplemented!(),
        },
    }
}

impl From<&TypeNode> for TypeExpr {
    fn from(type_node: &TypeNode) -> TypeExpr {
        convert_to_type_expr(type_node)
    }
}
