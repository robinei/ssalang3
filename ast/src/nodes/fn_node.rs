use common::Symbol;

use crate::{AstNode, CompileContext, NodeHandle, NodeType, TypedNodeHandle, nodes::BlockNode};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct FnParam {
    pub name: Symbol,
    pub type_node: NodeHandle,
}

pub struct FnNode {
    pub is_inline: bool,
    pub body_node: TypedNodeHandle<BlockNode>,
    pub retval_node: Option<NodeHandle>,
}

impl AstNode for FnNode {
    const NODE_TYPE: NodeType = NodeType::Fn;
    type LengthType = u32;
    type ElementType = FnParam;

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
