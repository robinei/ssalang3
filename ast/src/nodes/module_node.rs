use macros::ast_node;

use crate::{AstNode, NodeType, TypedNodeHandle, nodes::BlockNode};

#[ast_node(NodeType::Module)]
pub struct ModuleNode {
    pub body_node: TypedNodeHandle<BlockNode>,
}
