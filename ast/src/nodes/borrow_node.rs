use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType};

#[ast_node(NodeType::Borrow)]
pub struct BorrowNode {
    pub is_mutable: bool,
    pub path_node: NodeHandle,
}
