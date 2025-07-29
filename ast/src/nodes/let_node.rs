use common::Symbol;

use crate::{AstNode, CompileContext, NodeHandle, NodeType};

pub struct LetNode {
    pub name: Symbol,
    pub is_mutable: bool,
    pub type_node: Option<NodeHandle>,
    pub value_node: Option<NodeHandle>,
}

impl AstNode for LetNode {
    const NODE_TYPE: NodeType = NodeType::Let;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
