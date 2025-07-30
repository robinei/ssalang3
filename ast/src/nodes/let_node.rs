use common::Symbol;

use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

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

    #[inline]
    fn compile(
        &self,
        context: &mut CompileContext,
        _handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        self.compile_let(context)
    }
}

impl LetNode {
    pub fn compile_let(&self, _context: &mut CompileContext) -> CompileResult {
        todo!()
    }
}
