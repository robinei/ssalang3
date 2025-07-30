use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

pub struct BorrowNode {
    pub is_mutable: bool,
    pub path_node: NodeHandle,
}

impl AstNode for BorrowNode {
    const NODE_TYPE: NodeType = NodeType::Borrow;
    type LengthType = ();
    type ElementType = ();

    fn compile(
        &self,
        _context: &mut CompileContext,
        _handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        todo!()
    }
}
