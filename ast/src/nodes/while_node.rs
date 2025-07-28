use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType, TypedNodeHandle, nodes::BlockNode};

#[ast_node(NodeType::While)]
pub struct WhileNode {
    pub is_inline: bool,
    pub cond_node: NodeHandle,
    pub body_node: TypedNodeHandle<BlockNode>,
}
