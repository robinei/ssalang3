use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle, Value};

pub struct BlockNode {
    is_static: bool,
}

impl AstNode for BlockNode {
    const NODE_TYPE: NodeType = NodeType::Block;
    type LengthType = u32;
    type ElementType = NodeHandle;

    fn compile(
        &self,
        context: &mut CompileContext,
        handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        context.with_static_eval(self.is_static, |context| {
            for stmt in context.ast.get_array(handle) {
                stmt.compile(context)?;
            }
            Ok(Value::unit())
        })
    }
}
