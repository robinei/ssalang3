use crate::{AstArena, NodeHandle, NodeType, TypedNodeHandle, nodes::*};

pub trait AstVisitor {
    fn visit(&mut self, handle: NodeHandle, ast: &AstArena) {
        match handle.node_type() {
            NodeType::Assign => {
                let handle = handle.typed::<AssignNode>().unwrap();
                let node = ast.get(handle);
                self.visit_assign(node, handle, |visitor| {
                    visitor.visit(node.value_node, ast);
                })
            }
            NodeType::Binop => {
                let handle = handle.typed::<BinopNode>().unwrap();
                let node = ast.get(handle);
                self.visit_binop(node, handle, |visitor| {
                    visitor.visit(node.left_node, ast);
                    visitor.visit(node.right_node, ast);
                })
            }
            NodeType::Block => {
                let handle = handle.typed::<BlockNode>().unwrap();
                let node = ast.get(handle);
                self.visit_block(node, handle, |visitor| {
                    for stmt in ast.get_array(handle) {
                        visitor.visit(*stmt, ast);
                    }
                })
            }
            NodeType::Borrow => {
                let handle = handle.typed::<BorrowNode>().unwrap();
                let node = ast.get(handle);
                self.visit_borrow(node, handle, |visitor| {
                    visitor.visit(node.path_node, ast);
                })
            }
            NodeType::Break => {
                let handle = handle.typed::<BreakNode>().unwrap();
                let node = ast.get(handle);
                self.visit_break(node, handle, |visitor| {
                    visitor.visit(node.value_node, ast);
                })
            }
            NodeType::Call => {
                let handle = handle.typed::<CallNode>().unwrap();
                let node = ast.get(handle);
                self.visit_call(node, handle, |visitor| {
                    visitor.visit(node.func_node, ast);
                    for arg in ast.get_array(handle) {
                        visitor.visit(*arg, ast);
                    }
                })
            }
            NodeType::ConstUnit => {
                let handle = handle.typed::<ConstUnitNode>().unwrap();
                let node = ast.get(handle);
                self.visit_const_unit(node, handle)
            }
            NodeType::ConstBool => {
                let handle = handle.typed::<ConstBoolNode>().unwrap();
                let node = ast.get(handle);
                self.visit_const_bool(node, handle)
            }
            NodeType::ConstInt => {
                let handle = handle.typed::<ConstIntNode>().unwrap();
                let node = ast.get(handle);
                self.visit_const_int(node, handle)
            }
            NodeType::ConstUint => {
                let handle = handle.typed::<ConstUintNode>().unwrap();
                let node = ast.get(handle);
                self.visit_const_uint(node, handle)
            }
            NodeType::ConstFloat => {
                let handle = handle.typed::<ConstFloatNode>().unwrap();
                let node = ast.get(handle);
                self.visit_const_float(node, handle)
            }
            NodeType::ConstString => {
                let handle = handle.typed::<ConstStringNode>().unwrap();
                let node = ast.get(handle);
                self.visit_const_string(node, handle)
            }
            NodeType::ConstTypeId => {
                let handle = handle.typed::<ConstTypeIdNode>().unwrap();
                let node = ast.get(handle);
                self.visit_const_type_id(node, handle)
            }
            NodeType::Continue => {
                let handle = handle.typed::<ContinueNode>().unwrap();
                let node = ast.get(handle);
                self.visit_continue(node, handle, |_visitor| {})
            }
            NodeType::Fn => {
                let handle = handle.typed::<FnNode>().unwrap();
                let node = ast.get(handle);
                self.visit_fn(node, handle, |visitor| {
                    for param in ast.get_array(handle) {
                        visitor.visit_fn_param(param, |visitor| {
                            visitor.visit(param.type_node, ast);
                        });
                    }
                    if let Some(return_type_node) = node.return_type_node {
                        visitor.visit(return_type_node, ast);
                    }
                    visitor.visit(node.body_node.untyped(), ast);
                })
            }
            NodeType::Ident => {
                let handle = handle.typed::<IdentNode>().unwrap();
                let node = ast.get(handle);
                self.visit_ident(node, handle, |_visitor| {})
            }
            NodeType::If => {
                let handle = handle.typed::<IfNode>().unwrap();
                let node = ast.get(handle);
                self.visit_if(node, handle, |visitor| {
                    visitor.visit(node.cond_node, ast);
                    visitor.visit(node.then_node.untyped(), ast);
                    visitor.visit(node.else_node, ast);
                })
            }
            NodeType::Let => {
                let handle = handle.typed::<LetNode>().unwrap();
                let node = ast.get(handle);
                self.visit_let(node, handle, |visitor| {
                    if let Some(type_node) = node.type_node {
                        visitor.visit(type_node, ast);
                    }
                    if let Some(value_node) = node.value_node {
                        visitor.visit(value_node, ast);
                    }
                })
            }
            NodeType::Module => {
                let handle = handle.typed::<ModuleNode>().unwrap();
                let node = ast.get(handle);
                self.visit_module(node, handle, |visitor| {
                    visitor.visit(node.body_node.untyped(), ast);
                })
            }
            NodeType::Return => {
                let handle = handle.typed::<ReturnNode>().unwrap();
                let node = ast.get(handle);
                self.visit_return(node, handle, |visitor| {
                    visitor.visit(node.value_node, ast);
                })
            }
            NodeType::Struct => {
                let handle = handle.typed::<StructNode>().unwrap();
                let node = ast.get(handle);
                self.visit_struct(node, handle, |visitor| {
                    for field in ast.get_array(handle) {
                        visitor.visit_struct_field(field, |visitor| {
                            visitor.visit(field.type_node, ast);
                        });
                    }
                })
            }
            NodeType::Unop => {
                let handle = handle.typed::<UnopNode>().unwrap();
                let node = ast.get(handle);
                self.visit_unop(node, handle, |visitor| {
                    visitor.visit(node.operand_node, ast);
                })
            }
            NodeType::While => {
                let handle = handle.typed::<WhileNode>().unwrap();
                let node = ast.get(handle);
                self.visit_while(node, handle, |visitor| {
                    visitor.visit(node.cond_node, ast);
                    visitor.visit(node.body_node.untyped(), ast);
                })
            }
        }
    }

    fn visit_assign(&mut self, _node: &AssignNode, _handle: TypedNodeHandle<AssignNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_binop(&mut self, _node: &BinopNode, _handle: TypedNodeHandle<BinopNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_block(&mut self, _node: &BlockNode, _handle: TypedNodeHandle<BlockNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_borrow(&mut self, _node: &BorrowNode, _handle: TypedNodeHandle<BorrowNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_break(&mut self, _node: &BreakNode, _handle: TypedNodeHandle<BreakNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_call(&mut self, _node: &CallNode, _handle: TypedNodeHandle<CallNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_const_unit(&mut self, _node: &ConstUnitNode, _handle: TypedNodeHandle<ConstUnitNode>) {}

    fn visit_const_bool(&mut self, _node: &ConstBoolNode, _handle: TypedNodeHandle<ConstBoolNode>) {}

    fn visit_const_int(&mut self, _node: &ConstIntNode, _handle: TypedNodeHandle<ConstIntNode>) {}

    fn visit_const_uint(&mut self, _node: &ConstUintNode, _handle: TypedNodeHandle<ConstUintNode>) {}

    fn visit_const_float(&mut self, _node: &ConstFloatNode, _handle: TypedNodeHandle<ConstFloatNode>) {}

    fn visit_const_string(&mut self, _node: &ConstStringNode, _handle: TypedNodeHandle<ConstStringNode>) {}

    fn visit_const_type_id(&mut self, _node: &ConstTypeIdNode, _handle: TypedNodeHandle<ConstTypeIdNode>) {}

    fn visit_continue(&mut self, _node: &ContinueNode, _handle: TypedNodeHandle<ContinueNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_fn(&mut self, _node: &FnNode, _handle: TypedNodeHandle<FnNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_fn_param(&mut self, _param: &FnParam, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_ident(&mut self, _node: &IdentNode, _handle: TypedNodeHandle<IdentNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_if(&mut self, _node: &IfNode, _handle: TypedNodeHandle<IfNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_let(&mut self, _node: &LetNode, _handle: TypedNodeHandle<LetNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_module(&mut self, _node: &ModuleNode, _handle: TypedNodeHandle<ModuleNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_return(&mut self, _node: &ReturnNode, _handle: TypedNodeHandle<ReturnNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_struct(&mut self, _node: &StructNode, _handle: TypedNodeHandle<StructNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_struct_field(&mut self, _field: &StructNodeField, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_unop(&mut self, _node: &UnopNode, _handle: TypedNodeHandle<UnopNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }

    fn visit_while(&mut self, _node: &WhileNode, _handle: TypedNodeHandle<WhileNode>, visit_children: impl FnOnce(&mut Self)) {
        visit_children(self);
    }
}
