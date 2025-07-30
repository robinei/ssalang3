use common::{FuncParam, Symbol, Type, TypeId};
use ir::{Code, Instr, IrBuilder};

use crate::{
    AstNode, AstVisitor, Bindings, CompileContext, CompileError, CompileResult, FunctionContext, NodeHandle, NodeType, Template,
    TypedNodeHandle,
    nodes::{BlockNode, IdentNode, LetNode},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct FnParam {
    pub name: Symbol,
    pub is_static: bool,
    pub type_node: NodeHandle,
}

pub struct FnNode {
    pub is_inline: bool,
    pub return_type_node: Option<NodeHandle>,
    pub body_node: TypedNodeHandle<BlockNode>,
}

impl AstNode for FnNode {
    const NODE_TYPE: NodeType = NodeType::Fn;
    type LengthType = u32;
    type ElementType = FnParam;

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult {
        let params = context.module.ast.get_array(handle);
        let has_any_static_params = params.iter().any(|p| p.is_static);

        if has_any_static_params {
            let mut free_var_finder = FreeVarFinder::new();
            free_var_finder.visit(handle.untyped(), &context.module.ast);

            let mut static_captures = Bindings::new();
            for name in free_var_finder.free_vars {
                let Some(slot_index) = context.env.get_slot_index(name) else {
                    return Err(CompileError::Error(None, "use of undeclared variable".to_string()));
                };
                let Some(value) = &context.env.slots[slot_index].value else {
                    return Err(CompileError::Error(None, "use of undefined variable".to_string()));
                };
                static_captures.push((name, *value));
            }

            let template_id = context.module.create_template(handle, static_captures);
            Ok(Instr::const_template(template_id))
        } else {
            let (func_type_id, code) = self.compile_function(context, params, None)?;
            let func_id = context.module.create_function(func_type_id, code);
            Ok(Instr::const_function(func_type_id, func_id))
        }
    }
}

impl FnNode {
    pub fn compile_function(
        &self,
        context: &mut CompileContext,
        params: &[FnParam],
        static_args: Option<&Bindings>,
    ) -> Result<(TypeId, Code), CompileError> {
        // create new function context, because we are compiling a new function
        let mut function_context = FunctionContext {
            is_in_body: false,
            return_type: None,
            irbuilder: IrBuilder::new(),
        };

        // set up a new compile context with the created function context
        let mut new_context = CompileContext {
            global: &context.global,
            module: &context.module,
            func: &mut function_context,
            env: &mut context.env,
            static_eval: false,
        };

        let (return_type_id, type_params) = new_context.with_push_scope(|context| {
            let mut type_params = Vec::new();

            // push arguments onto the environment stack
            for param in params.iter() {
                let type_id = context.compile_type(param.type_node)?;
                let value = if param.is_static {
                    // it the parameter is marked static, we require the arg to be present in static_args
                    let Some(static_args) = &static_args else {
                        unreachable!("can't specialize template function without specialization args");
                    };
                    let Some((_, instr)) = static_args.iter().find(|a| a.0 == param.name) else {
                        unreachable!("could not find specialization argument for parameter '{}'", param.name);
                    };
                    if type_id != instr.get_type_id() {
                        return Err(CompileError::type_error(
                            None,
                            type_id,
                            instr.get_type_id(),
                            "argument for static function paramater".to_string(),
                        ));
                    }
                    assert!(instr.is_const());
                    *instr
                } else {
                    // if it is not marked as static, we generate an Arg instruction to represent it
                    // (and add the param to the parameter list of the function type we will generate)
                    type_params.push(FuncParam::new(param.name, type_id));
                    Instr::arg(type_params.len() as i32 - 1, type_id)
                };
                context.env.push_slot(param.name, false, Some(type_id), Some(value));
            }

            // compile the return type
            context.func.return_type = match self.return_type_node {
                Some(return_type_node) => Some(context.compile_type(return_type_node)?),
                None => None,
            };

            // compile the body
            context.func.is_in_body = true;
            let body_result = self.body_node.compile(context)?;
            let body_type_id = body_result.get_type_id();

            // validate return type, comoparing and the body result type with context.func.return_type, which is set when compiling return nodes
            let return_type_id = if let Some(return_type_id) = context.func.return_type {
                if body_type_id != TypeId::NEVER && return_type_id != body_type_id {
                    return Err(CompileError::type_error(
                        Some(self.body_node.untyped()),
                        return_type_id,
                        body_type_id,
                        "body result type".to_string(),
                    ));
                }
                return_type_id
            } else {
                body_type_id
            };

            Ok((return_type_id, type_params))
        })?;

        let code = function_context.irbuilder.into_code();
        let func_type_id = Type::Function(false, return_type_id, type_params).intern();
        Ok((func_type_id, code))
    }
}

struct FreeVarFinder {
    env: Vec<Symbol>,
    free_vars: Vec<Symbol>,
}

impl FreeVarFinder {
    fn new() -> Self {
        Self {
            env: Vec::new(),
            free_vars: Vec::new(),
        }
    }
}

impl AstVisitor for FreeVarFinder {
    fn visit_fn(&mut self, _node: &FnNode, _handle: TypedNodeHandle<FnNode>, visit_children: impl FnOnce(&mut Self)) {
        let prev_len = self.env.len();
        visit_children(self);
        self.env.truncate(prev_len);
    }

    fn visit_fn_param(&mut self, param: &FnParam, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
        self.env.push(param.name);
    }

    fn visit_block(&mut self, _node: &BlockNode, _handle: TypedNodeHandle<BlockNode>, visit_children: impl FnOnce(&mut Self)) {
        let prev_len = self.env.len();
        visit_children(self);
        self.env.truncate(prev_len);
    }

    fn visit_let(&mut self, node: &LetNode, _handle: TypedNodeHandle<LetNode>, visit_children: impl FnOnce(&mut Self)) {
        self.env.push(node.name);
        visit_children(self);
    }

    fn visit_ident(&mut self, node: &IdentNode, _handle: TypedNodeHandle<IdentNode>, _visit_children: impl FnOnce(&mut Self)) {
        if !self.env.contains(&node.name) {
            self.free_vars.push(node.name);
        }
    }
}
