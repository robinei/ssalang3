use crate::{AstNode, CompileContext, NodeHandle, NodeType};

pub struct BorrowNode {
    pub is_mutable: bool,
    pub path_node: NodeHandle,
}

impl AstNode for BorrowNode {
    const NODE_TYPE: NodeType = NodeType::Borrow;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
