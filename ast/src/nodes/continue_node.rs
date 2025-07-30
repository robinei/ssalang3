use common::Symbol;

use crate::{AstNode, CompileContext, CompileResult, NodeType, TypedNodeHandle};

pub struct ContinueNode {
    pub label: Option<Symbol>,
}

impl AstNode for ContinueNode {
    const NODE_TYPE: NodeType = NodeType::Continue;
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
