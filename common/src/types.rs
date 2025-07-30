use std::collections::HashMap;
use std::num::NonZeroU32;
use std::sync::{Arc, LazyLock, RwLock};

use crate::{NoCompare, Symbol};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum FloatPrecision {
    Single,
    Double,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructField {
    pub name: Symbol,
    pub type_id: TypeId,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FuncParam {
    pub name: NoCompare<Symbol>, // should not participate in comparisons or hashing (function types are structural, and names are advisory)
    pub type_id: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    Int(i64, i64),
    UInt(u64, u64),
    Float(FloatPrecision),
    String,
    Struct(Vec<StructField>),
    Function(bool, TypeId, Vec<FuncParam>), // is_pure, return_type_id, params -- values are FunctionId of function matching description

    // values of the following types can cannot exist at run-time
    Template, // values are TemplateId (identifies template which will be specialized for all unique sets of static values) - detailed type (Function) can't be established yet
    Type,     // values are TypeId
    Cons,     // type of Cons instructions, used to chain arguments in IR
    Unit, // type of expressions which return, but without a useful value. more precisely it is the type which has a single value inhabiting it
    Never, // type of instructions/expressions which never return (so there is no value)
}

/// Deterministic identifier for types (indexes into type table)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct TypeId(NonZeroU32);

impl TypeId {
    #[inline]
    pub const fn new(id: u32) -> Self {
        Self(NonZeroU32::new(id).unwrap())
    }

    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0.get()
    }

    pub fn get_type(self) -> Arc<Type> {
        TYPES.read().unwrap().get_type(self).unwrap()
    }

    pub const BOOL: TypeId = TypeId::new(1);
    pub const I32: TypeId = TypeId::new(2);
    pub const I64: TypeId = TypeId::new(3);
    pub const U32: TypeId = TypeId::new(4);
    pub const U64: TypeId = TypeId::new(5);
    pub const F32: TypeId = TypeId::new(6);
    pub const F64: TypeId = TypeId::new(7);
    pub const STRING: TypeId = TypeId::new(8);
    pub const TEMPLATE: TypeId = TypeId::new(9);
    pub const TYPE: TypeId = TypeId::new(10);
    pub const CONS: TypeId = TypeId::new(11);
    pub const UNIT: TypeId = TypeId::new(12);
    pub const NEVER: TypeId = TypeId::new(13);
}

struct TypeStore {
    type_table: Vec<Arc<Type>>,             // TypeId indexes into this
    type_to_id: HashMap<Arc<Type>, TypeId>, // For deduplication/interning
}

impl TypeStore {
    pub fn new() -> Self {
        let mut store = Self {
            type_table: vec![Arc::new(Type::Never)],
            type_to_id: HashMap::new(),
        };
        store.intern_known(TypeId::BOOL, Type::Bool);
        store.intern_known(TypeId::I32, Type::Int(i32::MIN as i64, i32::MAX as i64));
        store.intern_known(TypeId::I64, Type::Int(i64::MIN, i64::MAX));
        store.intern_known(TypeId::U32, Type::UInt(u32::MIN as u64, u32::MAX as u64));
        store.intern_known(TypeId::U64, Type::UInt(u64::MIN, u64::MAX));
        store.intern_known(TypeId::F32, Type::Float(FloatPrecision::Single));
        store.intern_known(TypeId::F64, Type::Float(FloatPrecision::Double));
        store.intern_known(TypeId::STRING, Type::String);
        store.intern_known(TypeId::TEMPLATE, Type::Template);
        store.intern_known(TypeId::TYPE, Type::Type);
        store.intern_known(TypeId::CONS, Type::Cons);
        store.intern_known(TypeId::UNIT, Type::Unit);
        store.intern_known(TypeId::NEVER, Type::Never);
        store
    }

    fn intern_known(&mut self, expected_type_id: TypeId, ty: Type) {
        let type_id = self.intern_type(ty);
        assert_eq!(type_id, expected_type_id);
    }

    /// Intern a type and return its TypeId (deduplicates identical types)
    pub fn intern_type(&mut self, mut ty: Type) -> TypeId {
        if let Type::Struct(fields) = &mut ty {
            fields.sort();
        }
        if let Some(&type_id) = self.type_to_id.get(&ty) {
            type_id
        } else {
            let type_id = TypeId::new(self.type_table.len() as u32);
            let tyref = Arc::new(ty);
            self.type_table.push(tyref.clone());
            self.type_to_id.insert(tyref, type_id);
            type_id
        }
    }

    /// Get a type by its TypeId
    pub fn get_type(&self, type_id: TypeId) -> Option<Arc<Type>> {
        self.type_table.get(type_id.as_u32() as usize).cloned()
    }
}

static TYPES: LazyLock<RwLock<TypeStore>> = LazyLock::new(|| RwLock::new(TypeStore::new()));

impl StructField {
    pub fn new(name: Symbol, type_id: TypeId) -> Self {
        Self { name, type_id }
    }
}

impl FuncParam {
    pub fn new(name: Symbol, type_id: TypeId) -> Self {
        Self {
            name: NoCompare { value: name },
            type_id,
        }
    }
}

impl Type {
    pub fn intern(self) -> TypeId {
        TYPES.write().unwrap().intern_type(self)
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            Self::Int(_, _) => true,
            Self::UInt(_, _) => true,
            Self::Float(_) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Int(_, _) => write!(f, "int"), // TODO: format integer types properly (taking range into account)
            Type::UInt(_, _) => write!(f, "uint"),
            Type::Float(_) => write!(f, "float"), // TODO: format float type properly
            Type::String => write!(f, "string"),
            Type::Struct(..) => write!(f, "struct"),
            Type::Function(is_pure, ret, params) => {
                if *is_pure {
                    write!(f, "pure ")?;
                }
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    p.type_id.fmt(f)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                if *ret == TypeId::UNIT {
                    write!(f, ")")?;
                } else {
                    write!(f, ") -> ")?;
                    ret.fmt(f)?;
                }
                Ok(())
            }
            Type::Template => write!(f, "generic_func_id"),
            Type::Type => write!(f, "type_id"),
            Type::Cons => write!(f, "cons"),
            Type::Unit => write!(f, "unit"),
            Type::Never => write!(f, "never"),
        }
    }
}

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.get_type().fmt(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId {
    pub index: u32,
}

impl ModuleId {
    #[inline]
    pub const fn new(id: u32) -> Self {
        Self { index: id }
    }
}

impl std::fmt::Display for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ModuleId({})", self.index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId {
    pub module: ModuleId,
    pub index: u32,
}

impl FunctionId {
    #[inline]
    pub const fn new(module: ModuleId, id: u32) -> Self {
        Self { module, index: id }
    }
}

impl std::fmt::Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ConcreteFuncId({}, module={})", self.index, self.module.index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TemplateId {
    pub module: ModuleId,
    pub index: u32,
}

impl TemplateId {
    #[inline]
    pub const fn new(module: ModuleId, id: u32) -> Self {
        Self { module, index: id }
    }
}

impl std::fmt::Display for TemplateId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GenericFuncId({}, module={})", self.index, self.module.index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_size() {
        assert_eq!(32, size_of::<Type>());
    }
}
