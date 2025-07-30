use common::TypeId;
use ir::Instr;

use crate::{
    AstNode, CompileContext, CompileError, CompileResult, NodeHandle, NodeType, TypedNodeHandle,
    Value, nodes::BlockNode,
};

pub struct IfNode {
    pub is_inline: bool,
    pub cond_node: NodeHandle,
    pub then_node: TypedNodeHandle<BlockNode>,
    pub else_node: NodeHandle,
}

impl AstNode for IfNode {
    const NODE_TYPE: NodeType = NodeType::If;
    type LengthType = ();
    type ElementType = ();

    fn compile(
        &self,
        context: &mut CompileContext,
        _handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        let cond =
            context.with_static_eval(self.is_inline, |context| self.cond_node.compile(context))?;

        if cond.get_type_id() != TypeId::bool_id() {
            return Err(CompileError::type_error(
                Some(self.cond_node),
                TypeId::bool_id(),
                cond.get_type_id(),
                "if condition".to_string(),
            ));
        }

        if let Instr::ConstBool(_, val) = cond.instr {
            return if val {
                self.then_node.compile(context)
            } else {
                self.else_node.compile(context)
            };
        }

        let then_block = context.irbuilder.create_block();
        let else_block = context.irbuilder.create_block();
        let merge_block = context.irbuilder.create_block();

        context
            .irbuilder
            .emit_branch(cond.instr, then_block, else_block);
        context.irbuilder.seal_block(then_block);
        context.irbuilder.seal_block(else_block);

        // Compile then branch
        context.irbuilder.emit_label(then_block);
        let then = self.then_node.compile(context)?;

        // Check if we need a phi for the result
        let needs_phi = then.get_type_id() != TypeId::unit_id();
        let phi_ref = if needs_phi {
            Some(context.irbuilder.create_phi())
        } else {
            None
        };

        // Add upsilon for then branch if needed and jump to merge
        if let Some(phi) = phi_ref {
            context.irbuilder.emit_upsilon(then_block, phi, then.instr);
        }
        context.irbuilder.emit_jump(merge_block);

        // Compile else branch
        context.irbuilder.emit_label(else_block);
        let els = self.else_node.compile(context)?;

        // Type check: both branches must have same type
        if then.get_type_id() != els.get_type_id() {
            return Err(CompileError::type_error(
                Some(self.cond_node),
                then.get_type_id(),
                els.get_type_id(),
                "else body (must have same type as then body)".to_string(),
            ));
        }

        // Add upsilon for else branch if needed and jump to merge
        if let Some(phi) = phi_ref {
            context.irbuilder.emit_upsilon(else_block, phi, els.instr);
        }
        context.irbuilder.emit_jump(merge_block);
        context.irbuilder.seal_block(merge_block);

        // Generate merge block with phi if needed
        context.irbuilder.emit_label(merge_block);

        if let Some(phi) = phi_ref {
            let phi_instr = context.irbuilder.emit_phi(phi, then.get_type_id());
            Ok(Value::new(phi_instr))
        } else {
            // Unit type - no meaningful value
            Ok(Value::unit())
        }
    }
}
