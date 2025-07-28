use common::Symbol;
use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType, TypedNodeHandle, nodes::BlockNode};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct FnParam {
    pub name: Symbol,
    pub type_node: NodeHandle,
}

#[ast_node(NodeType::Fn, array = FnParam)]
pub struct FnNode {
    pub is_inline: bool,
    pub body_node: TypedNodeHandle<BlockNode>,
    pub retval_node: Option<NodeHandle>,
}
