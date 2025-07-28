use common::Symbol;
use macros::ast_node;

use crate::{AstNode, NodeType, TypedNodeHandle, nodes::FnNode};

#[ast_node(NodeType::LetFn)]
pub struct LetFnNode {
    pub name: Symbol,
    pub fn_node: TypedNodeHandle<FnNode>,
}
