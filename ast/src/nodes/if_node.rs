use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType, TypedNodeHandle, nodes::BlockNode};

#[ast_node(NodeType::If)]
pub struct IfNode {
    pub is_inline: bool,
    pub cond_node: NodeHandle,
    pub then_node: TypedNodeHandle<BlockNode>,
    pub else_node: NodeHandle,
}
