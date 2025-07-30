use crate::{AstNode, CompileContext, CompileResult, NodeType, TypedNodeHandle, nodes::BlockNode};

pub struct ModuleNode {
    pub body_node: TypedNodeHandle<BlockNode>,
}

impl AstNode for ModuleNode {
    const NODE_TYPE: NodeType = NodeType::Module;
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
