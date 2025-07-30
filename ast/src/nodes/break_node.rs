use common::Symbol;

use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

pub struct BreakNode {
    pub label: Option<Symbol>,
    pub value_node: NodeHandle,
}

impl AstNode for BreakNode {
    const NODE_TYPE: NodeType = NodeType::Break;
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
