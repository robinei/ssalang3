use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType};

#[ast_node(NodeType::Return)]
pub struct ReturnNode {
    pub value_node: NodeHandle,
}
