use common::TypeId;
use ir::Instr;

use crate::{
    AstNode, CompileContext, CompileError, CompileResult, NodeHandle, NodeType, TypedNodeHandle,
    Value, nodes::BlockNode,
};

pub struct WhileNode {
    pub is_inline: bool,
    pub cond_node: NodeHandle,
    pub body_node: TypedNodeHandle<BlockNode>,
}

impl AstNode for WhileNode {
    const NODE_TYPE: NodeType = NodeType::While;
    type LengthType = ();
    type ElementType = ();

    fn compile(
        &self,
        context: &mut CompileContext,
        _handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        if self.is_inline || context.static_eval {
            self.compile_inline_while(context)
        } else {
            self.compile_runtime_while(context)
        }
    }
}

impl WhileNode {
    // inline while loop where the condition is always statically evaluated, and the body is either static
    // (if we are in a static context, or compiled repeatedly with the IR for all iterations laid out linearly)
    #[inline]
    fn compile_inline_while(&self, context: &mut CompileContext) -> CompileResult {
        loop {
            let cond = context.with_static_eval(true, |context| self.cond_node.compile(context))?;
            self.typecheck_cond(&cond)?;

            let Instr::ConstBool(_, cond_val) = cond.instr else {
                return Err(CompileError::Error(
                    Some(self.cond_node),
                    "expected static expression".to_string(),
                ));
            };

            if !cond_val {
                return Ok(Value::unit());
            }

            let _ = self.body_node.compile(context)?;
        }
    }

    // while loop with runtime control flow, with body being compiled only once
    #[inline]
    fn compile_runtime_while(&self, context: &mut CompileContext) -> CompileResult {
        let cond_block = context.irbuilder.create_block();
        let body_block = context.irbuilder.create_block();
        let exit_block = context.irbuilder.create_block();

        context.irbuilder.emit_jump(cond_block);
        context.irbuilder.emit_label(cond_block);
        let cond = self.cond_node.compile(context)?;
        self.typecheck_cond(&cond)?;
        context
            .irbuilder
            .emit_branch(cond.instr, body_block, exit_block);
        context.irbuilder.seal_block(body_block);
        context.irbuilder.seal_block(exit_block);

        context.irbuilder.emit_label(body_block);
        let _ = self.body_node.compile(context)?;
        context.irbuilder.emit_jump(cond_block);
        context.irbuilder.seal_block(cond_block);

        context.irbuilder.emit_label(exit_block);

        Ok(Value::unit())
    }

    fn typecheck_cond(&self, cond: &Value) -> Result<(), CompileError> {
        if cond.get_type_id() != TypeId::bool_id() {
            return Err(CompileError::type_error(
                Some(self.cond_node),
                TypeId::bool_id(),
                cond.get_type_id(),
                "while condition".to_string(),
            ));
        }
        Ok(())
    }
}
