use std::{num::NonZeroI16, sync::Arc};

use common::{FunctionId, TemplateId, Type, TypeId};
use ordered_float::OrderedFloat;

pub type RefType = i16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstrRef(NonZeroI16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockRef(NonZeroI16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhiRef(NonZeroI16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarRef(NonZeroI16);

impl InstrRef {
    #[inline]
    pub fn new(value: RefType) -> Option<Self> {
        NonZeroI16::new(value).map(Self)
    }
    #[inline]
    pub fn get(self) -> RefType {
        self.0.get()
    }
}
impl BlockRef {
    #[inline]
    pub fn new(value: RefType) -> Option<Self> {
        NonZeroI16::new(value).map(Self)
    }
    #[inline]
    pub fn get(self) -> RefType {
        self.0.get()
    }
}
impl PhiRef {
    #[inline]
    pub fn new(value: RefType) -> Option<Self> {
        NonZeroI16::new(value).map(Self)
    }
    #[inline]
    pub fn get(self) -> RefType {
        self.0.get()
    }
}
impl VarRef {
    #[inline]
    pub fn new(value: RefType) -> Option<Self> {
        NonZeroI16::new(value).map(Self)
    }
    #[inline]
    pub fn get(self) -> RefType {
        self.0.get()
    }
}

impl From<InstrRef> for RefType {
    #[inline]
    fn from(value: InstrRef) -> Self {
        value.get()
    }
}
impl From<BlockRef> for RefType {
    #[inline]
    fn from(value: BlockRef) -> Self {
        value.get()
    }
}
impl From<PhiRef> for RefType {
    #[inline]
    fn from(value: PhiRef) -> Self {
        value.get()
    }
}
impl From<VarRef> for RefType {
    #[inline]
    fn from(value: VarRef) -> Self {
        value.get()
    }
}

impl From<RefType> for InstrRef {
    #[inline]
    fn from(value: RefType) -> Self {
        Self(std::num::NonZeroI16::new(value).unwrap())
    }
}
impl From<RefType> for BlockRef {
    #[inline]
    fn from(value: RefType) -> Self {
        Self(std::num::NonZeroI16::new(value).unwrap())
    }
}
impl From<RefType> for PhiRef {
    #[inline]
    fn from(value: RefType) -> Self {
        Self(std::num::NonZeroI16::new(value).unwrap())
    }
}
impl From<RefType> for VarRef {
    #[inline]
    fn from(value: RefType) -> Self {
        Self(std::num::NonZeroI16::new(value).unwrap())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Meta(u32);

impl Meta {
    #[inline]
    pub fn new(ty: TypeId) -> Self {
        Self(ty.as_u32())
    }

    #[inline]
    pub fn get_type_id(self) -> TypeId {
        TypeId::new(self.0 & 0x7FFFFFFF)
    }

    #[inline]
    pub fn get_type(self) -> Arc<Type> {
        self.get_type_id().get_type()
    }

    #[inline]
    pub fn is_marked(self) -> bool {
        (self.0 & 0x80000000) != 0
    }

    #[inline]
    pub fn set_marked(&mut self, marked: bool) {
        if marked {
            self.0 |= 0x80000000;
        } else {
            self.0 &= 0x7FFFFFFF;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Instr {
    Nop(Meta),
    Never(Meta),
    Identity(Meta, InstrRef),
    Print(Meta, InstrRef),
    Call(Meta, InstrRef, Option<InstrRef>),

    // CFG instructions
    Label(Meta, BlockRef),
    Jump(Meta, BlockRef),
    Branch(Meta, InstrRef, BlockRef, BlockRef),
    Ret(Meta, InstrRef),

    StackAlloc(Meta, u32),
    Load(Meta, InstrRef),
    Store(Meta, InstrRef),

    // SSA annotations
    Upsilon(Meta, PhiRef, InstrRef),
    Phi(Meta, PhiRef),

    // Pure instructions after this

    // Constant values
    ConstUnit(Meta),
    ConstBool(Meta, bool),
    ConstInt(Meta, i64),
    ConstUInt(Meta, u64),
    ConstFloat(Meta, OrderedFloat<f64>),
    ConstTemplate(Meta, TemplateId),
    ConstFunction(Meta, FunctionId),
    ConstType(Meta, TypeId),

    Arg(Meta, i32), // Function argument reference
    PureCall(Meta, InstrRef, Option<InstrRef>),
    Cons(Meta, InstrRef, InstrRef), // used to chain arguments to PureCall / Call

    // Binary operations
    Add(Meta, InstrRef, InstrRef),
    Sub(Meta, InstrRef, InstrRef),
    Mul(Meta, InstrRef, InstrRef),
    Div(Meta, InstrRef, InstrRef),
    Eq(Meta, InstrRef, InstrRef),
    Neq(Meta, InstrRef, InstrRef),
    Lt(Meta, InstrRef, InstrRef),
    Gt(Meta, InstrRef, InstrRef),
    LtEq(Meta, InstrRef, InstrRef),
    GtEq(Meta, InstrRef, InstrRef),
    And(Meta, InstrRef, InstrRef),
    Or(Meta, InstrRef, InstrRef),

    // Unary operations
    Neg(Meta, InstrRef),
    Not(Meta, InstrRef),
    CheckedCast(Meta, InstrRef),
    UncheckedCast(Meta, InstrRef),
    BitCast(Meta, InstrRef),
}

impl Instr {
    #[inline]
    pub fn nop() -> Self {
        Self::Nop(Meta::new(TypeId::UNIT))
    }

    #[inline]
    pub fn never() -> Self {
        Self::Never(Meta::new(TypeId::NEVER))
    }

    #[inline]
    pub fn const_unit() -> Self {
        Self::ConstUnit(Meta::new(TypeId::UNIT))
    }

    #[inline]
    pub fn const_bool(value: bool) -> Self {
        Self::ConstBool(Meta::new(TypeId::BOOL), value)
    }

    #[inline]
    pub fn const_int(value: i64) -> Self {
        Self::ConstInt(Meta::new(TypeId::I64), value)
    }

    #[inline]
    pub fn const_uint(value: u64) -> Self {
        Self::ConstUInt(Meta::new(TypeId::U64), value)
    }

    #[inline]
    pub fn const_float(value: f64) -> Self {
        Self::ConstFloat(Meta::new(TypeId::F64), value.into())
    }

    #[inline]
    pub fn const_template(id: TemplateId) -> Self {
        Self::ConstTemplate(Meta::new(TypeId::TEMPLATE), id)
    }

    #[inline]
    pub fn const_function(type_id: TypeId, id: FunctionId) -> Self {
        Self::ConstFunction(Meta::new(type_id), id)
    }

    #[inline]
    pub fn const_type(id: TypeId) -> Self {
        Self::ConstType(Meta::new(TypeId::TYPE), id)
    }

    pub fn arg(arg: i32, ty: TypeId) -> Instr {
        assert!(arg >= 0);
        assert_ne!(ty, TypeId::UNIT);
        Instr::Arg(Meta::new(ty), arg)
    }

    #[inline]
    pub fn is_const(&self) -> bool {
        matches!(
            self,
            Self::ConstBool(..)
                | Self::ConstInt(..)
                | Self::ConstUInt(..)
                | Self::ConstFloat(..)
                | Self::ConstTemplate(..)
                | Self::ConstFunction(..)
                | Self::ConstType(..)
        )
    }

    #[inline]
    pub fn is_const_num(&self) -> bool {
        match self {
            Self::ConstInt(_, _) => true,
            Self::ConstUInt(_, _) => true,
            Self::ConstFloat(_, _) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_const_zero(&self) -> bool {
        match self {
            Self::ConstInt(_, val) => *val == 0,
            Self::ConstUInt(_, val) => *val == 0,
            Self::ConstFloat(_, val) => *val == 0.0,
            _ => false,
        }
    }

    pub fn const_zero(type_id: TypeId) -> Instr {
        match type_id {
            TypeId::I64 => Self::const_int(0),
            TypeId::U64 => Self::const_uint(0),
            TypeId::F64 => Self::const_float(0.0),
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn is_const_one(&self) -> bool {
        match self {
            Self::ConstInt(_, val) => *val == 1,
            Self::ConstUInt(_, val) => *val == 1,
            Self::ConstFloat(_, val) => *val == 1.0,
            _ => false,
        }
    }

    pub fn is_pure(&self) -> bool {
        matches!(
            self,
            Self::ConstBool(..)
                | Self::ConstInt(..)
                | Self::ConstUInt(..)
                | Self::ConstFloat(..)
                | Self::ConstTemplate(..)
                | Self::ConstFunction(..)
                | Self::ConstType(..)
                | Self::Arg(..)
                | Self::PureCall(..)
                | Self::Add(..)
                | Self::Sub(..)
                | Self::Mul(..)
                | Self::Div(..)
                | Self::Eq(..)
                | Self::Neq(..)
                | Self::Lt(..)
                | Self::Gt(..)
                | Self::LtEq(..)
                | Self::GtEq(..)
                | Self::And(..)
                | Self::Or(..)
                | Self::Neg(..)
                | Self::Not(..)
        )
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self, Self::Jump(..) | Self::Branch(..) | Self::Ret(..))
    }

    pub fn get_meta(&self) -> Meta {
        match self {
            Self::Nop(meta) => *meta,
            Self::Never(meta) => *meta,
            Self::Identity(meta, ..) => *meta,
            Self::Print(meta, ..) => *meta,
            Self::Call(meta, ..) => *meta,
            Self::Label(meta, ..) => *meta,
            Self::Jump(meta, ..) => *meta,
            Self::Branch(meta, ..) => *meta,
            Self::Ret(meta, ..) => *meta,
            Self::Upsilon(meta, ..) => *meta,
            Self::Phi(meta, ..) => *meta,
            Self::ConstUnit(meta, ..) => *meta,
            Self::ConstBool(meta, ..) => *meta,
            Self::ConstInt(meta, ..) => *meta,
            Self::ConstUInt(meta, ..) => *meta,
            Self::ConstFloat(meta, ..) => *meta,
            Self::ConstTemplate(meta, ..) => *meta,
            Self::ConstFunction(meta, ..) => *meta,
            Self::ConstType(meta, ..) => *meta,
            Self::Arg(meta, ..) => *meta,
            Self::PureCall(meta, ..) => *meta,
            Self::Cons(meta, ..) => *meta,
            Self::Add(meta, ..) => *meta,
            Self::Sub(meta, ..) => *meta,
            Self::Mul(meta, ..) => *meta,
            Self::Div(meta, ..) => *meta,
            Self::Eq(meta, ..) => *meta,
            Self::Neq(meta, ..) => *meta,
            Self::Lt(meta, ..) => *meta,
            Self::Gt(meta, ..) => *meta,
            Self::LtEq(meta, ..) => *meta,
            Self::GtEq(meta, ..) => *meta,
            Self::And(meta, ..) => *meta,
            Self::Or(meta, ..) => *meta,
            Self::Neg(meta, ..) => *meta,
            Self::Not(meta, ..) => *meta,
            Self::CheckedCast(meta, ..) => *meta,
            Self::UncheckedCast(meta, ..) => *meta,
            Self::BitCast(meta, ..) => *meta,
            Self::StackAlloc(meta, ..) => *meta,
            Self::Load(meta, ..) => *meta,
            Self::Store(meta, ..) => *meta,
        }
    }

    #[inline]
    pub fn get_type_id(&self) -> TypeId {
        self.get_meta().get_type_id()
    }

    #[inline]
    pub fn get_type(&self) -> Arc<Type> {
        self.get_type_id().get_type()
    }

    #[inline]
    pub fn is_marked(&self) -> bool {
        self.get_meta().is_marked()
    }
}

impl Default for Instr {
    fn default() -> Self {
        Self::nop()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instr_size() {
        // With packed Meta (1 byte), the largest variant (Branch) needs:
        // tag(1) + meta(1) + 3*RefType(6) = 8 bytes
        assert_eq!(std::mem::size_of::<Instr>(), 16);
        assert_eq!(std::mem::size_of::<Meta>(), 4);
        assert_eq!(std::mem::size_of::<RefType>(), 2);
    }

    #[test]
    fn test_meta() {
        let meta1 = Meta::new(TypeId::I32);
        assert_eq!(meta1.get_type_id(), TypeId::I32);
        assert!(!meta1.is_marked());

        let mut meta2 = Meta::new(TypeId::BOOL);
        meta2.set_marked(true);
        assert_eq!(meta2.get_type_id(), TypeId::BOOL);
        assert!(meta2.is_marked());

        let mut meta3 = Meta::new(TypeId::UNIT);
        assert!(!meta3.is_marked());
        meta3.set_marked(true);
        assert!(meta3.is_marked());
        meta3.set_marked(false);
        assert!(!meta3.is_marked());
    }

    #[test]
    fn test_bool_const() {
        let instr_true = Instr::ConstBool(Meta::new(TypeId::BOOL), true);

        let instr_false = Instr::ConstBool(Meta::new(TypeId::BOOL), false);

        if let Instr::ConstBool(_, value) = instr_true {
            assert_eq!(value, true);
        } else {
            panic!("Expected ConstBool variant");
        }

        if let Instr::ConstBool(_, value) = instr_false {
            assert_eq!(value, false);
        } else {
            panic!("Expected ConstBool variant");
        }
    }

    #[test]
    fn test_i32_const() {
        let instr = Instr::ConstInt(Meta::new(TypeId::I32), 0x12345678);

        if let Instr::ConstInt(_, value) = instr {
            assert_eq!(value, 0x12345678);
        } else {
            panic!("Expected ConstI32 variant");
        }

        let instr_neg = Instr::ConstInt(Meta::new(TypeId::I32), -1);

        if let Instr::ConstInt(_, value) = instr_neg {
            assert_eq!(value, -1);
        } else {
            panic!("Expected ConstI32 variant");
        }
    }

    #[test]
    fn test_instr_methods() {
        let mut meta = Meta::new(TypeId::UNIT);
        meta.set_marked(true);
        let nop = Instr::Nop(meta);

        assert_eq!(nop.get_type_id(), TypeId::UNIT);
        assert!(nop.is_marked());
        assert!(!nop.is_pure());
        assert!(!nop.is_terminal());

        // Test get_meta method
        let meta = nop.get_meta();
        assert_eq!(meta.get_type_id(), TypeId::UNIT);
        assert!(meta.is_marked());

        let add = Instr::Add(Meta::new(TypeId::I32), InstrRef::new(1).unwrap(), InstrRef::new(2).unwrap());

        assert!(add.is_pure());
        assert!(!add.is_terminal());
        assert!(!add.is_marked());
        assert_eq!(add.get_meta().get_type_id(), TypeId::I32);

        let jump = Instr::Jump(Meta::new(TypeId::UNIT), BlockRef::new(5).unwrap());

        assert!(!jump.is_pure());
        assert!(jump.is_terminal());
    }

    #[test]
    fn test_niche_optimization() {
        // Test that Option<Ref> has the same size as Ref due to niche optimization
        assert_eq!(std::mem::size_of::<InstrRef>(), std::mem::size_of::<Option<InstrRef>>());
        assert_eq!(std::mem::size_of::<BlockRef>(), std::mem::size_of::<Option<BlockRef>>());
        assert_eq!(std::mem::size_of::<PhiRef>(), std::mem::size_of::<Option<PhiRef>>());
        assert_eq!(std::mem::size_of::<VarRef>(), std::mem::size_of::<Option<VarRef>>());

        // All should be exactly 16 bits (2 bytes)
        assert_eq!(std::mem::size_of::<InstrRef>(), 2);
        assert_eq!(std::mem::size_of::<Option<InstrRef>>(), 2);
        assert_eq!(std::mem::size_of::<BlockRef>(), 2);
        assert_eq!(std::mem::size_of::<Option<BlockRef>>(), 2);
        assert_eq!(std::mem::size_of::<PhiRef>(), 2);
        assert_eq!(std::mem::size_of::<Option<PhiRef>>(), 2);
        assert_eq!(std::mem::size_of::<VarRef>(), 2);
        assert_eq!(std::mem::size_of::<Option<VarRef>>(), 2);
    }

    #[test]
    fn test_ref_types() {
        let instr_ref = InstrRef::new(42).unwrap();
        assert_eq!(RefType::from(instr_ref), 42);
        assert_eq!(instr_ref.get(), 42);

        let block_ref = BlockRef::new(-1).unwrap();
        assert_eq!(RefType::from(block_ref), -1);
        assert_eq!(block_ref.get(), -1);

        let phi_ref = PhiRef::new(100).unwrap();
        assert_eq!(RefType::from(phi_ref), 100);
        assert_eq!(phi_ref.get(), 100);

        // Test equality and hashing work
        assert_eq!(InstrRef::new(42).unwrap(), InstrRef::new(42).unwrap());
        assert_ne!(InstrRef::new(42).unwrap(), InstrRef::new(43).unwrap());
    }

    #[test]
    fn test_nop_constructor() {
        let nop = Instr::nop();
        assert_eq!(nop.get_type_id(), TypeId::UNIT);
        assert!(!nop.is_pure());
        assert!(!nop.is_terminal());

        // Test that Default trait uses nop constructor
        let default_instr = Instr::default();
        assert_eq!(default_instr.get_type_id(), TypeId::UNIT);

        // Verify they're the same
        assert_eq!(nop, default_instr);
    }

    #[test]
    fn test_instr_is_const() {
        // Test constant instructions
        assert!(Instr::const_bool(true).is_const());
        assert!(Instr::const_bool(false).is_const());
        assert!(Instr::const_int(42).is_const());
        assert!(Instr::const_int(-1).is_const());
        assert!(Instr::const_int(0).is_const());

        // Test non-constant instructions
        let instr_ref = InstrRef::new(1).unwrap();
        assert!(!Instr::nop().is_const());
        assert!(!Instr::Identity(Meta::new(TypeId::I32), instr_ref).is_const());
        assert!(!Instr::Add(Meta::new(TypeId::I32), instr_ref, instr_ref).is_const());
        assert!(!Instr::Arg(Meta::new(TypeId::I32), 0).is_const());
        assert!(!Instr::Print(Meta::new(TypeId::UNIT), instr_ref).is_const());

        // Test control flow instructions
        let block_ref = BlockRef::new(1).unwrap();
        assert!(!Instr::Jump(Meta::new(TypeId::UNIT), block_ref).is_const());
        assert!(!Instr::Branch(Meta::new(TypeId::UNIT), instr_ref, block_ref, block_ref).is_const());
        assert!(!Instr::Ret(Meta::new(TypeId::UNIT), instr_ref).is_const());

        // Test SSA instructions
        let phi_ref = PhiRef::new(1).unwrap();
        assert!(!Instr::Phi(Meta::new(TypeId::I32), phi_ref).is_const());
        assert!(!Instr::Upsilon(Meta::new(TypeId::UNIT), phi_ref, instr_ref).is_const());
    }
}
