use common::Symbol;

use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct StructField {
    pub name: Symbol,
    pub type_node: NodeHandle,
}

pub struct StructNode {}

impl AstNode for StructNode {
    const NODE_TYPE: NodeType = NodeType::Struct;
    type LengthType = u32;
    type ElementType = StructField;

    fn compile(
        &self,
        _context: &mut CompileContext,
        _handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        todo!()
    }
}
