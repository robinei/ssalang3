use common::Symbol;
use macros::ast_node;

use crate::{AstNode, NodeHandle, NodeType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct StructField {
    pub name: Symbol,
    pub type_node: NodeHandle,
}

#[ast_node(NodeType::Struct, array = StructField)]
pub struct StructNode {}
