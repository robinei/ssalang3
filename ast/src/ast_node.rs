use crate::{CompileContext, CompileResult, NodeHandle, TypedNodeHandle, node_type::NodeType};

// Trait that lets us abstract over u32 and () as lengths.
// () is unit, and zero-sized, and is used for nodes with no attached array
pub trait ArrayLen: Default + Copy {
    fn create_len(value: u32) -> Self;
    fn get_len(self) -> u32;
}

impl ArrayLen for u32 {
    #[inline]
    fn create_len(value: u32) -> Self {
        value
    }

    #[inline]
    fn get_len(self) -> u32 {
        self
    }
}

impl ArrayLen for () {
    #[inline]
    fn create_len(_: u32) -> Self {
        ()
    }

    #[inline]
    fn get_len(self) -> u32 {
        0
    }
}

// Trait that all node types must implement
pub trait AstNode: Sized {
    const NODE_TYPE: NodeType;
    type LengthType: ArrayLen;
    type ElementType;

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult;

    #[inline]
    fn get_handle(&self, context: &CompileContext) -> Option<TypedNodeHandle<Self>> {
        context.module.ast.handle_from_ref(self)
    }

    #[inline]
    fn get_untyped_handle(&self, context: &CompileContext) -> Option<NodeHandle> {
        self.get_handle(context).map(|t| t.untyped())
    }
}
