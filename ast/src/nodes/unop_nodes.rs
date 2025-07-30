use common::TypeId;

use crate::{
    AstNode, CompileContext, CompileError, CompileResult, NodeHandle, NodeType, TypedNodeHandle,
    Value,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnopType {
    Neg, // -
    Not, // !
         //BitNot, // ~
}

pub struct UnopNode {
    pub op_type: UnopType,
    pub operand_node: NodeHandle,
}

impl AstNode for UnopNode {
    const NODE_TYPE: NodeType = NodeType::Unop;
    type LengthType = ();
    type ElementType = ();

    fn compile(
        &self,
        context: &mut CompileContext,
        _handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        let operand = self.operand_node.compile(context)?;

        match self.op_type {
            UnopType::Neg => {
                if operand.get_type_id() != TypeId::i32_id() {
                    return Err(CompileError::type_error(
                        Some(self.operand_node),
                        TypeId::i32_id(),
                        operand.get_type_id(),
                        "unary negation".to_string(),
                    ));
                }
                let result_instr = context.irbuilder.emit_neg(operand.instr);
                Ok(Value::new(result_instr))
            }
            UnopType::Not => {
                if operand.get_type_id() != TypeId::bool_id() {
                    return Err(CompileError::type_error(
                        Some(self.operand_node),
                        TypeId::bool_id(),
                        operand.get_type_id(),
                        "logical not".to_string(),
                    ));
                }
                let result_instr = context.irbuilder.emit_logi_not(operand.instr);
                Ok(Value::new(result_instr))
            }
        }
    }
}
