use common::Symbol;
use macros::ast_node;

use crate::{AstNode, NodeType};

#[ast_node(NodeType::Ident)]
pub struct IdentNode {
    pub name: Symbol,
}
