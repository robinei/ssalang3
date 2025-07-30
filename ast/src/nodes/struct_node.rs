use common::{StructField, Symbol, Type};
use ir::Instr;

use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct StructNodeField {
    pub name: Symbol,
    pub type_node: NodeHandle,
}

pub struct StructNode {}

impl AstNode for StructNode {
    const NODE_TYPE: NodeType = NodeType::Struct;
    type LengthType = u32;
    type ElementType = StructNodeField;

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult {
        let mut fields = Vec::new();

        for f in context.module.ast.get_array(handle) {
            let type_id = context.compile_type(f.type_node)?;
            fields.push(StructField { name: f.name, type_id });
        }

        let type_id = Type::Struct(fields).intern();
        Ok(Instr::const_type(type_id))
    }
}
