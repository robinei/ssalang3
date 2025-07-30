use std::num::NonZeroI16;

use common::TypeId;

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
    Identity(Meta, InstrRef),
    Print(Meta, InstrRef),

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

    // Pure instructions
    ConstBool(Meta, bool),
    ConstI32(Meta, i32),
    Arg(Meta, i32),
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
    Neg(Meta, InstrRef),
    Not(Meta, InstrRef),
}

impl Instr {
    pub fn nop() -> Self {
        Self::Nop(Meta::new(TypeId::unit_id()))
    }

    pub fn const_bool(value: bool) -> Self {
        Self::ConstBool(Meta::new(TypeId::bool_id()), value)
    }

    pub fn const_i32(value: i32) -> Self {
        Self::ConstI32(Meta::new(TypeId::i32_id()), value)
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::ConstBool(..) | Self::ConstI32(..))
    }

    pub fn is_pure(&self) -> bool {
        matches!(
            self,
            Self::ConstBool(..)
                | Self::ConstI32(..)
                | Self::Arg(..)
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
            Self::Identity(meta, ..) => *meta,
            Self::Print(meta, ..) => *meta,
            Self::Label(meta, ..) => *meta,
            Self::Jump(meta, ..) => *meta,
            Self::Branch(meta, ..) => *meta,
            Self::Ret(meta, ..) => *meta,
            Self::Upsilon(meta, ..) => *meta,
            Self::Phi(meta, ..) => *meta,
            Self::ConstBool(meta, ..) => *meta,
            Self::ConstI32(meta, ..) => *meta,
            Self::Arg(meta, ..) => *meta,
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
        let meta1 = Meta::new(TypeId::i32_id());
        assert_eq!(meta1.get_type_id(), TypeId::i32_id());
        assert!(!meta1.is_marked());

        let mut meta2 = Meta::new(TypeId::bool_id());
        meta2.set_marked(true);
        assert_eq!(meta2.get_type_id(), TypeId::bool_id());
        assert!(meta2.is_marked());

        let mut meta3 = Meta::new(TypeId::unit_id());
        assert!(!meta3.is_marked());
        meta3.set_marked(true);
        assert!(meta3.is_marked());
        meta3.set_marked(false);
        assert!(!meta3.is_marked());
    }

    #[test]
    fn test_bool_const() {
        let instr_true = Instr::ConstBool(Meta::new(TypeId::bool_id()), true);

        let instr_false = Instr::ConstBool(Meta::new(TypeId::bool_id()), false);

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
        let instr = Instr::ConstI32(Meta::new(TypeId::i32_id()), 0x12345678);

        if let Instr::ConstI32(_, value) = instr {
            assert_eq!(value, 0x12345678);
        } else {
            panic!("Expected ConstI32 variant");
        }

        let instr_neg = Instr::ConstI32(Meta::new(TypeId::i32_id()), -1);

        if let Instr::ConstI32(_, value) = instr_neg {
            assert_eq!(value, -1);
        } else {
            panic!("Expected ConstI32 variant");
        }
    }

    #[test]
    fn test_instr_methods() {
        let mut meta = Meta::new(TypeId::unit_id());
        meta.set_marked(true);
        let nop = Instr::Nop(meta);

        assert_eq!(nop.get_type_id(), TypeId::unit_id());
        assert!(nop.is_marked());
        assert!(!nop.is_pure());
        assert!(!nop.is_terminal());

        // Test get_meta method
        let meta = nop.get_meta();
        assert_eq!(meta.get_type_id(), TypeId::unit_id());
        assert!(meta.is_marked());

        let add = Instr::Add(
            Meta::new(TypeId::i32_id()),
            InstrRef::new(1).unwrap(),
            InstrRef::new(2).unwrap(),
        );

        assert!(add.is_pure());
        assert!(!add.is_terminal());
        assert!(!add.is_marked());
        assert_eq!(add.get_meta().get_type_id(), TypeId::i32_id());

        let jump = Instr::Jump(Meta::new(TypeId::unit_id()), BlockRef::new(5).unwrap());

        assert!(!jump.is_pure());
        assert!(jump.is_terminal());
    }

    #[test]
    fn test_niche_optimization() {
        // Test that Option<Ref> has the same size as Ref due to niche optimization
        assert_eq!(
            std::mem::size_of::<InstrRef>(),
            std::mem::size_of::<Option<InstrRef>>()
        );
        assert_eq!(
            std::mem::size_of::<BlockRef>(),
            std::mem::size_of::<Option<BlockRef>>()
        );
        assert_eq!(
            std::mem::size_of::<PhiRef>(),
            std::mem::size_of::<Option<PhiRef>>()
        );
        assert_eq!(
            std::mem::size_of::<VarRef>(),
            std::mem::size_of::<Option<VarRef>>()
        );

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
        assert_eq!(nop.get_type_id(), TypeId::unit_id());
        assert!(!nop.is_pure());
        assert!(!nop.is_terminal());

        // Test that Default trait uses nop constructor
        let default_instr = Instr::default();
        assert_eq!(default_instr.get_type_id(), TypeId::unit_id());

        // Verify they're the same
        assert_eq!(nop, default_instr);
    }

    #[test]
    fn test_instr_is_const() {
        // Test constant instructions
        assert!(Instr::const_bool(true).is_const());
        assert!(Instr::const_bool(false).is_const());
        assert!(Instr::const_i32(42).is_const());
        assert!(Instr::const_i32(-1).is_const());
        assert!(Instr::const_i32(0).is_const());

        // Test non-constant instructions
        let instr_ref = InstrRef::new(1).unwrap();
        assert!(!Instr::nop().is_const());
        assert!(!Instr::Identity(Meta::new(TypeId::i32_id()), instr_ref).is_const());
        assert!(!Instr::Add(Meta::new(TypeId::i32_id()), instr_ref, instr_ref).is_const());
        assert!(!Instr::Arg(Meta::new(TypeId::i32_id()), 0).is_const());
        assert!(!Instr::Print(Meta::new(TypeId::unit_id()), instr_ref).is_const());

        // Test control flow instructions
        let block_ref = BlockRef::new(1).unwrap();
        assert!(!Instr::Jump(Meta::new(TypeId::unit_id()), block_ref).is_const());
        assert!(
            !Instr::Branch(
                Meta::new(TypeId::unit_id()),
                instr_ref,
                block_ref,
                block_ref
            )
            .is_const()
        );
        assert!(!Instr::Ret(Meta::new(TypeId::unit_id()), instr_ref).is_const());

        // Test SSA instructions
        let phi_ref = PhiRef::new(1).unwrap();
        assert!(!Instr::Phi(Meta::new(TypeId::i32_id()), phi_ref).is_const());
        assert!(!Instr::Upsilon(Meta::new(TypeId::unit_id()), phi_ref, instr_ref).is_const());
    }
}
