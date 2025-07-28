use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinopType {
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Eq,     // ==
    Neq,    // !=
    Lt,     // <
    Gt,     // >
    LtEq,   // <=
    GtEq,   // >=
    And,    // &&
    Or,     // ||
    BitAnd, // &
    BitOr,  // |
}

#[ast_node(NodeType::Binop)]
pub struct BinopNode {
    pub op_type: BinopType,
    pub left_node: NodeHandle,
    pub right_node: NodeHandle,
}
