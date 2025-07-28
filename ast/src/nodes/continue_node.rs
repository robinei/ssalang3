use common::Symbol;
use macros::ast_node;

use crate::{AstNode, NodeType};

#[ast_node(NodeType::Continue)]
pub struct ContinueNode {
    pub label: Option<Symbol>,
}
