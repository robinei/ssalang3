use common::Symbol;
use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType};

#[ast_node(NodeType::Let)]
pub struct LetNode {
    pub name: Symbol,
    pub is_mutable: bool,
    pub type_node: Option<NodeHandle>,
    pub value_node: Option<NodeHandle>,
}
