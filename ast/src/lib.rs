use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

mod ast_arena;
mod ast_node;
mod ast_visitor;
mod node_type;
pub mod nodes;

pub use ast_arena::*;
pub use ast_node::*;
pub use ast_visitor::*;
use common::{FunctionId, ModuleId, Symbol, TemplateId, TypeId};
use ir::{Code, Instr, IrBuilder};
pub use node_type::*;
use smallvec::SmallVec;

use crate::nodes::FnNode;

#[must_use]
#[derive(Debug, Eq, PartialEq)]
pub enum CompileError {
    Return(Instr),
    Break(Option<Symbol>, Instr),
    Continue(Option<Symbol>),
    Error(Option<NodeHandle>, String),
}

impl CompileError {
    fn type_error(node: Option<NodeHandle>, expected: TypeId, found: TypeId, errcontext: String) -> CompileError {
        CompileError::Error(
            node,
            format!("expected type {}, but found type {} in {}", expected, found, errcontext),
        )
    }
}

type CompileResult = Result<Instr, CompileError>;

pub struct EnvironmentSlot {
    name: Symbol,
    is_mutable: bool,
    type_id: Option<TypeId>,
    value: Option<Instr>,
    shadowing_slot: i32, // this slot shadows the specified slot (unless negative)
}

pub struct Environment {
    slots: Vec<EnvironmentSlot>,
    symbol_map: HashMap<Symbol, i32>,
}

impl Environment {
    fn new() -> Self {
        Self {
            slots: Vec::new(),
            symbol_map: HashMap::new(),
        }
    }

    fn push_slot(&mut self, name: Symbol, is_mutable: bool, type_id: Option<TypeId>, value: Option<Instr>) -> usize {
        let shadowing_slot = *self.symbol_map.get(&name).unwrap_or(&-1);
        let slot_index = self.slots.len();
        self.symbol_map.insert(name, slot_index as i32);
        self.slots.push(EnvironmentSlot {
            name,
            is_mutable,
            type_id,
            value,
            shadowing_slot,
        });
        slot_index
    }

    fn pop_slot(&mut self) {
        let slot = self.slots.pop().expect("can't pop variable");
        if slot.shadowing_slot >= 0 {
            self.symbol_map.insert(slot.name, slot.shadowing_slot);
        } else {
            self.symbol_map.remove(&slot.name);
        }
    }

    fn get_slot_index(&self, name: Symbol) -> Option<usize> {
        self.symbol_map.get(&name).map(|i| *i as usize)
    }
}

struct FunctionContext {
    is_in_body: bool,
    return_type: Option<TypeId>,
    irbuilder: IrBuilder,
}

pub struct CompileContext<'a> {
    global: &'a GlobalContext,
    module: &'a Module,
    func: &'a mut FunctionContext,
    env: &'a mut Environment,
    static_eval: bool,
}

impl<'a> CompileContext<'a> {
    #[inline]
    fn with_static_eval<R>(&mut self, static_eval: bool, f: impl FnOnce(&mut Self) -> R) -> R {
        let prev = self.static_eval;
        self.static_eval = self.static_eval || static_eval;
        let result = f(self);
        self.static_eval = prev;
        result
    }

    #[inline]
    fn with_push_scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let prev_slots_len = self.env.slots.len();
        let result = f(self);
        while self.env.slots.len() > prev_slots_len {
            self.env.pop_slot();
        }
        result
    }

    fn compile_type(&mut self, node: NodeHandle) -> Result<TypeId, CompileError> {
        let instr = self.with_static_eval(true, |context| node.compile(context))?;
        if instr.get_type_id() != TypeId::TYPE {
            return Err(CompileError::type_error(
                Some(node),
                TypeId::TYPE,
                instr.get_type_id(),
                "type expression".to_string(),
            ));
        }
        match instr {
            Instr::ConstType(_, type_id) => Ok(type_id),
            _ => unreachable!("expected instruction to be ConstType, since its type was Type"),
        }
    }
}

pub type Bindings = SmallVec<[(Symbol, Instr); 4]>;

pub struct Template {
    id: TemplateId,
    fn_node: TypedNodeHandle<FnNode>,
    static_captures: Bindings, // generic functions need to capture their free vars
}

pub struct Function {
    id: FunctionId,
    type_id: TypeId,
    code: Code,
}

pub struct FunctionStore {
    pub template: Vec<Arc<Template>>,
    pub concrete: Vec<Arc<Function>>,
    pub specializations: HashMap<(TemplateId, Bindings), FunctionId>,
}

pub struct Module {
    id: ModuleId,
    ast: AstArena,
    functions: RwLock<FunctionStore>,
}

impl Module {
    pub fn get_template(&self, id: TemplateId) -> Arc<Template> {
        assert_eq!(id.module, self.id);
        let functions = self.functions.read().unwrap();
        functions.template[id.index as usize].clone()
    }

    pub fn get_function(&self, id: FunctionId) -> Arc<Function> {
        assert_eq!(id.module, self.id);
        let functions = self.functions.read().unwrap();
        functions.concrete[id.index as usize].clone()
    }

    pub fn get_specialization(&self, specialization_key: &(TemplateId, Bindings)) -> Option<Arc<Function>> {
        let functions = self.functions.read().unwrap();
        let id = functions.specializations.get(specialization_key).copied();
        match id {
            Some(func_id) => Some(functions.concrete[func_id.index as usize].clone()),
            None => None,
        }
    }

    pub fn create_template(&self, fn_node: TypedNodeHandle<FnNode>, static_captures: Bindings) -> TemplateId {
        let mut functions = self.functions.write().unwrap();
        let id = TemplateId::new(self.id, functions.concrete.len() as u32);
        functions.template.push(Arc::new(Template {
            id,
            fn_node,
            static_captures,
        }));
        id
    }

    pub fn create_function(&self, type_id: TypeId, code: Code) -> FunctionId {
        let mut functions = self.functions.write().unwrap();
        let id = FunctionId::new(self.id, functions.concrete.len() as u32);
        functions.concrete.push(Arc::new(Function { id, type_id, code }));
        id
    }

    pub fn create_specialized_function(
        &self,
        template_id: TemplateId,
        specialization_args: Bindings,
        type_id: TypeId,
        code: Code,
    ) -> FunctionId {
        let mut functions = self.functions.write().unwrap();
        let concrete_id = FunctionId::new(self.id, functions.concrete.len() as u32);
        functions.concrete.push(Arc::new(Function {
            id: concrete_id,
            type_id,
            code,
        }));
        functions.specializations.insert((template_id, specialization_args), concrete_id);
        concrete_id
    }
}

pub struct ModuleStore {
    all: Vec<Arc<Module>>,
}

pub struct GlobalContext {
    modules: RwLock<ModuleStore>,
}

impl GlobalContext {
    pub fn get_module(&self, module_id: ModuleId) -> Arc<Module> {
        let modules = self.modules.read().unwrap();
        modules.all[module_id.index as usize].clone()
    }
}
