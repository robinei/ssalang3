use common::Symbol;
use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType};

#[ast_node(NodeType::Break)]
pub struct BreakNode {
    pub label: Option<Symbol>,
    pub value_node: NodeHandle,
}
