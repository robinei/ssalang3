use common::Symbol;

use crate::{AstNode, CompileContext, CompileResult, NodeType, TypedNodeHandle};

pub struct IdentNode {
    pub name: Symbol,
}

impl AstNode for IdentNode {
    const NODE_TYPE: NodeType = NodeType::Ident;
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
