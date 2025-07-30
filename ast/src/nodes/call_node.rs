use common::{FunctionId, TemplateId, TypeId};
use ir::{Instr, IrBuilderError};

use crate::{AstNode, Bindings, CompileContext, CompileError, CompileResult, Environment, NodeHandle, NodeType, TypedNodeHandle};

pub struct CallNode {
    pub func_node: NodeHandle,
}

impl AstNode for CallNode {
    const NODE_TYPE: NodeType = NodeType::Call;
    type LengthType = u32;
    type ElementType = NodeHandle;

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult {
        // Compile the function target of the call
        let call_target = self.func_node.compile(context)?;

        // Compile the arguments
        let arg_nodes = context.module.ast.get_array(handle);
        let mut orig_args = Vec::new();
        for arg_node in arg_nodes {
            orig_args.push(arg_node.compile(context)?);
        }

        // Get the function
        let mut call_args_storage = Vec::new();
        let (func_type_id, func_id, call_args) = match call_target {
            // Concrete function can be used directly with original args
            Instr::ConstFunction(func_meta, func_id) => (func_meta.get_type_id(), func_id, &orig_args),

            // Template function must be specialized with the arguments which correspond with static paramaters
            Instr::ConstTemplate(_, template_id) => {
                let (func_type_id, func_id) =
                    self.get_or_create_specialized_func(context, template_id, &orig_args, &mut call_args_storage)?;
                (func_type_id, func_id, &call_args_storage)
            }

            _ => return Err(CompileError::Error(Some(self.func_node), "expected function in call".to_string())),
        };

        // Emit function call
        context
            .func
            .irbuilder
            .emit_call(Instr::const_function(func_type_id, func_id), &call_args)
            .map_err(|err: IrBuilderError| {
                CompileError::Error(
                    Some(match err.component {
                        0 => handle.untyped(),
                        1 => self.func_node,
                        _ => {
                            let err_arg = call_args[err.component as usize - 2];
                            arg_nodes[orig_args.iter().position(|arg| *arg == err_arg).unwrap()]
                        }
                    }),
                    err.message,
                )
            })
    }
}

impl CallNode {
    fn get_or_create_specialized_func(
        &self,
        context: &mut CompileContext,
        template_id: TemplateId,
        orig_args: &Vec<Instr>,         // the entire original argument list
        func_args_out: &mut Vec<Instr>, // will hold the args that should go to the actual function (and not the ones used by specialization)
    ) -> Result<(TypeId, FunctionId), CompileError> {
        let func_module = context.global.get_module(template_id.module);
        let template = func_module.get_template(template_id);
        let fn_node = func_module.ast.get(template.fn_node);
        let fn_params = func_module.ast.get_array(template.fn_node);

        if fn_params.len() != orig_args.len() {
            return Err(CompileError::Error(
                Some(self.func_node),
                format!("function expected {} arguments, but received {}", fn_params.len(), orig_args.len()),
            ));
        }

        // collect arguments into static_args for specialization, and into func_args_out as actual args for the resulting function
        let mut static_args = Bindings::new();
        for (param, arg) in fn_params.iter().zip(orig_args.iter()) {
            if param.is_static {
                if !arg.is_const() {
                    return Err(CompileError::Error(
                        Some(self.func_node),
                        format!("parameter '{}' is static, but non-static value was passed", param.name),
                    ));
                }
                static_args.push((param.name, *arg));
            } else {
                func_args_out.push(*arg);
            }
        }

        // check if this specialization has been cached before
        let specialization_key = (template.id, static_args);
        if let Some(func) = func_module.get_specialization(&specialization_key) {
            return Ok((func.type_id, func.id));
        }

        // create fresh env populated only with the values captured from the definition environment
        let mut fresh_env = Environment::new();
        for (name, instr) in template.static_captures.iter() {
            fresh_env.push_slot(*name, false, Some(instr.get_type_id()), Some(*instr));
        }

        // create new context with the new environment, and the module the template is owned by (a function must be compiled in the context of its own module)
        let mut new_context = CompileContext {
            global: context.global,
            module: &func_module,
            func: context.func, // compile_function overrides this
            env: &mut fresh_env,
            static_eval: false,
        };

        // compile the function in the new context
        let (func_type_id, code) = fn_node.compile_function(&mut new_context, fn_params, Some(&specialization_key.1))?;

        // cache specialization
        let func_id = func_module.create_specialized_function(template_id, specialization_key.1, func_type_id, code);

        Ok((func_type_id, func_id))
    }
}
