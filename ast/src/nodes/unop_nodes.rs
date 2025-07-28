use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnopType {
    Neg,    // -
    Not,    // !
    BitNot, // ~
}

#[ast_node(NodeType::Unop)]
pub struct UnopNode {
    pub op_type: UnopType,
    pub operand_node: NodeHandle,
}
