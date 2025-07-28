use macros::ast_node;

use crate::{AstNode, NodeType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeAtom {
    Unit,
    Bool,
    I32,
    String,
}

#[ast_node(NodeType::TypeAtom)]
pub struct TypeAtomNode {
    pub atom: TypeAtom,
}
