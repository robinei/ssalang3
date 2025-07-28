use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType};

#[ast_node(NodeType::Block, array = NodeHandle)]
pub struct BlockNode {}
