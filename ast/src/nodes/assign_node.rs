use common::Symbol;
use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType};

#[ast_node(NodeType::Assign)]
pub struct AssignNode {
    pub name: Symbol,
    pub value_node: NodeHandle,
}
