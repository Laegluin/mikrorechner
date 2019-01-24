use crate::ast::*;
use crate::codegen::{
    CodegenError, FRAME_PTR_REG, LOAD_IMMEDIATE_MAX, STORE_IMMEDIATE_MAX, TMP_OP_REG, TMP_REG,
};
use crate::emit::{Label, Reg};
use crate::typecheck::{Type, TypeDesc, TypeRef};
use derive_more::{Add, AddAssign, Sub};
use fnv::FnvHashMap;
use std::iter;
use std::ops::Mul;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use strum::IntoEnumIterator;

impl Reg {
    fn is_general_purpose(&self) -> bool {
        match *self {
            Reg::AddrOffset | Reg::Null | FRAME_PTR_REG | TMP_REG | TMP_OP_REG => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, Copy, Sub, Add, AddAssign, Ord, PartialOrd, Eq, PartialEq)]
pub struct StackOffset(pub u32);

impl Mul<u32> for StackOffset {
    type Output = StackOffset;

    fn mul(self, rhs: u32) -> StackOffset {
        StackOffset(self.0 * rhs)
    }
}

#[derive(Debug, Clone, Copy, Sub, Add, AddAssign, Ord, PartialOrd, Eq, PartialEq)]
pub struct RegOffset(pub u32);

impl Mul<u32> for RegOffset {
    type Output = RegOffset;

    fn mul(self, rhs: u32) -> RegOffset {
        RegOffset(self.0 * rhs)
    }
}

#[derive(PartialEq, Eq)]
pub enum FieldIdent<'a> {
    Idx(usize),
    Name(&'a Ident),
}

impl<'a> From<usize> for FieldIdent<'a> {
    fn from(idx: usize) -> FieldIdent<'a> {
        FieldIdent::Idx(idx)
    }
}

impl<'a> From<&'a Ident> for FieldIdent<'a> {
    fn from(name: &'a Ident) -> FieldIdent<'a> {
        FieldIdent::Name(name)
    }
}

pub struct LayoutCache {
    layouts: FnvHashMap<TypeRef, Option<Rc<Layout>>>,
}

impl LayoutCache {
    pub fn new() -> LayoutCache {
        LayoutCache {
            layouts: FnvHashMap::default(),
        }
    }

    pub fn get_or_gen(&mut self, ty: TypeRef, ast: &TypedAst) -> Result<Rc<Layout>, CodegenError> {
        match self.layouts.get(&ty) {
            Some(Some(layout)) => Ok(Rc::clone(layout)),
            Some(None) => Err(CodegenError::InfiniteSize(ty.desc())),
            None => {
                // insert `None` to detect cyclic layouts (layouts with infinite size)
                self.layouts.insert(ty.clone(), None);

                // generate the actual layout and update the value
                let layout = Rc::new(Layout::from_type(&ast.types[&ty], self, ast)?);
                self.layouts
                    .get_mut(&ty)
                    .map(|layout_ref| *layout_ref = Some(Rc::clone(&layout)));

                Ok(layout)
            }
        }
    }
}

/// Stores the size and layout of a value for both its stack and its register representation.
///
/// Data may have fields that can be indexed by index and name (if the field has one).
pub struct Layout {
    // offsets for each field are stored as the total offset relative to the
    // values start offset
    reg_size: RegOffset,
    reg_field_layout: Vec<(Option<Ident>, RegOffset)>,
    stack_size: StackOffset,
    stack_field_layout: Vec<(Option<Ident>, StackOffset)>,
}

impl Layout {
    pub fn zero_sized() -> Layout {
        Layout {
            reg_size: RegOffset(0),
            reg_field_layout: Vec::new(),
            stack_size: StackOffset(0),
            stack_field_layout: Vec::new(),
        }
    }

    pub fn word() -> Layout {
        Layout {
            reg_size: RegOffset(1),
            reg_field_layout: Vec::new(),
            stack_size: StackOffset(4),
            stack_field_layout: Vec::new(),
        }
    }

    fn array(elem_layout: &Layout, len: u32) -> Layout {
        let reg_field_layout = iter::repeat((None, elem_layout.reg_size()))
            .scan(RegOffset(0), |offset, (name, next_offset)| {
                let field = (name, *offset);
                *offset += next_offset;
                Some(field)
            })
            .take(len as usize)
            .collect();

        let stack_field_layout = iter::repeat((None, elem_layout.stack_size()))
            .scan(StackOffset(0), |offset, (name, next_offset)| {
                let field = (name, *offset);
                *offset += next_offset;
                Some(field)
            })
            .take(len as usize)
            .collect();

        Layout {
            reg_size: elem_layout.reg_size() * len,
            reg_field_layout,
            stack_size: elem_layout.stack_size() * len,
            stack_field_layout,
        }
    }

    fn composite(elems: Vec<(Option<Ident>, Rc<Layout>)>) -> Layout {
        let mut reg_size = RegOffset(0);

        let reg_field_layout = elems
            .iter()
            .map(|(name, layout)| {
                let field = (name.clone(), reg_size);
                reg_size += layout.reg_size();
                field
            })
            .collect();

        let mut stack_size = StackOffset(0);

        let stack_field_layout = elems
            .into_iter()
            .map(|(name, layout)| {
                let field = (name, stack_size);
                stack_size += layout.stack_size();
                field
            })
            .collect();

        Layout {
            reg_size,
            reg_field_layout,
            stack_size,
            stack_field_layout,
        }
    }

    fn from_type(
        ty: &Type,
        layouts: &mut LayoutCache,
        ast: &TypedAst,
    ) -> Result<Layout, CodegenError> {
        let layout = match *ty {
            Type::Never => Layout::zero_sized(),
            Type::Bool => Layout::word(),
            Type::I32 | Type::U32 => Layout::word(),
            Type::ConstPtr(_) | Type::MutPtr(_) => Layout::word(),
            Type::Array(ref elem_ty, len) => {
                let elem_layout = layouts.get_or_gen(elem_ty.clone(), ast)?;
                Layout::array(&*elem_layout, len)
            }
            Type::Tuple(ref elem_tys) => {
                let layouts = elem_tys
                    .iter()
                    .map(|ty| {
                        let layout = layouts.get_or_gen(ty.clone(), ast)?;
                        Ok((None, layout))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Layout::composite(layouts)
            }
            Type::Record(ref record) => {
                let layouts = record
                    .fields
                    .iter()
                    .map(|field| {
                        let layout = layouts.get_or_gen(field.ty.clone(), ast)?;
                        Ok((Some(field.name.clone()), layout))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Layout::composite(layouts)
            }
            Type::Str | Type::Function(_) => {
                return Err(CodegenError::UnsizedType(Rc::new(TypeDesc::from_type(ty))));
            }
            Type::Var | Type::Int | Type::Ptr(_) | Type::PartialRecord(_) => unreachable!(),
        };

        Ok(layout)
    }

    pub fn stack_size(&self) -> StackOffset {
        self.stack_size
    }

    fn stack_field_offset<'a>(&self, field: impl Into<FieldIdent<'a>>) -> StackOffset {
        Layout::find_field_offset(field.into(), &self.stack_field_layout)
    }

    pub fn reg_size(&self) -> RegOffset {
        self.reg_size
    }

    fn reg_field_offset<'a>(&self, field: impl Into<FieldIdent<'a>>) -> RegOffset {
        Layout::find_field_offset(field.into(), &self.reg_field_layout)
    }

    fn find_field_offset<O: Copy>(field: FieldIdent<'_>, offsets: &[(Option<Ident>, O)]) -> O {
        offsets
            .iter()
            .enumerate()
            .find_map(|(idx, (ident, offset))| {
                let field_ident = ident
                    .as_ref()
                    .map(FieldIdent::Name)
                    .unwrap_or(FieldIdent::Idx(idx));

                if field == field_ident {
                    Some(*offset)
                } else {
                    None
                }
            })
            .expect("valid field for layout")
    }

    pub fn is_zero_sized(&self) -> bool {
        self.stack_size() == StackOffset(0) && self.reg_size() == RegOffset(0)
    }

    pub fn is_uniform(&self) -> bool {
        self.stack_size().0 == self.reg_size().0 * 4
    }
}

#[derive(Debug)]
pub enum Value {
    Label(LabelValue),
    Reg(RegValue),
    Stack(StackValue),
    /// A dynamically allocated value that lives at an address pointed to by the nested value.
    /// Effectively, this acts like an lvalue: it is a pointer, but refers to the value inside,
    /// not to the pointer itself. Thus, the layout used with this value needs to be the layout
    /// of the pointed at value.
    Ptr(Box<Value>),
}

impl Value {
    pub fn reg(reg: Reg) -> Value {
        Value::Reg(RegValue { regs: vec![reg] })
    }

    pub fn ptr(ptr: Value) -> Value {
        Value::Ptr(Box::new(ptr))
    }

    pub fn zero_sized() -> Value {
        Value::Reg(RegValue { regs: Vec::new() })
    }

    /// Gets the first register of this value or `None`, if the value is not
    /// stored in a register or is zero-sized.
    pub fn try_get_reg(&self) -> Option<Reg> {
        match *self {
            Value::Reg(ref value) => value.try_reg(),
            _ => None,
        }
    }

    pub fn unwrap_field<'a>(&self, field: impl Into<FieldIdent<'a>>, layout: &Layout) -> Value {
        match *self {
            Value::Reg(ref value) => Value::Reg(value.field(field, layout)),
            Value::Stack(ref value) => Value::Stack(value.field(field, layout)),
            _ => panic!("cannot access field for lvalue or label"),
        }
    }

    pub fn unwrap_label(&self) -> &LabelValue {
        match *self {
            Value::Label(ref value) => value,
            _ => panic!("called unwrap_label on a value that is not a label"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LabelValue {
    label: Label,
}

impl LabelValue {
    pub fn new(desc: impl AsRef<str>) -> LabelValue {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

        let id = NEXT_ID.fetch_add(1, Ordering::AcqRel);

        if id == usize::max_value() {
            panic!("overflow for label ids");
        }

        LabelValue {
            label: Label::new(format!("l{}_{}", id, desc.as_ref())),
        }
    }

    pub fn label(&self) -> &Label {
        &self.label
    }
}

#[derive(Debug)]
pub struct StackValue {
    start: StackOffset,
}

impl StackValue {
    pub fn offset(&self) -> StackOffset {
        self.start
    }

    fn field<'a>(&self, field: impl Into<FieldIdent<'a>>, layout: &Layout) -> StackValue {
        let offset = layout.stack_field_offset(field);

        StackValue {
            start: self.start + offset,
        }
    }
}

pub struct StackAllocator {
    frame_offset: StackOffset,
    scopes: Vec<StackOffset>,
}

impl StackAllocator {
    pub fn new() -> StackAllocator {
        StackAllocator {
            frame_offset: StackOffset(0),
            scopes: Vec::new(),
        }
    }

    pub fn alloc(&mut self, layout: &Layout) -> StackValue {
        let value = StackValue {
            start: self.frame_offset,
        };

        self.frame_offset += layout.stack_size();

        // make sure the value is still addressable by using offsets in load/store
        assert!(self.frame_offset.0 <= LOAD_IMMEDIATE_MAX);
        assert!(self.frame_offset.0 <= STORE_IMMEDIATE_MAX);

        value
    }

    /// Returns the frame offset for the allocator. This is the offset at which the
    /// next allocated value would start.
    pub fn frame_offset(&self) -> StackOffset {
        self.frame_offset
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(self.frame_offset);
    }

    pub fn exit_scope(&mut self) {
        self.frame_offset = self
            .scopes
            .pop()
            .unwrap_or_else(|| panic!("cannot exit global scope"));
    }
}

#[derive(Debug)]
pub struct RegValue {
    regs: Vec<Reg>,
}

impl RegValue {
    pub fn reg(&self) -> Reg {
        assert!(!self.regs.is_empty(), "not a zero-sized value");
        self.regs[0]
    }

    fn try_reg(&self) -> Option<Reg> {
        self.regs.get(0).cloned()
    }

    pub fn regs(&self) -> &[Reg] {
        &self.regs
    }

    fn field<'a>(&self, field: impl Into<FieldIdent<'a>>, layout: &Layout) -> RegValue {
        let offset = layout.reg_field_offset(field);

        RegValue {
            regs: self.regs[offset.0 as usize..].iter().cloned().collect(),
        }
    }
}

pub struct RegAllocator {
    free_regs: Vec<Reg>,
    scopes: Vec<Vec<Reg>>,
}

impl RegAllocator {
    pub fn new() -> RegAllocator {
        RegAllocator {
            free_regs: Reg::iter().filter(Reg::is_general_purpose).collect(),
            scopes: Vec::new(),
        }
    }

    pub fn alloc(&mut self, layout: &Layout) -> Option<RegValue> {
        if self.free_regs.len() < layout.reg_size().0 as usize {
            None
        } else {
            let regs = self
                .free_regs
                .drain(0..layout.reg_size().0 as usize)
                .collect();

            Some(RegValue { regs })
        }
    }

    pub fn allocated_regs(&self) -> impl '_ + Iterator<Item = Reg> {
        Reg::iter()
            .filter(Reg::is_general_purpose)
            .filter(move |reg| !self.free_regs.contains(reg))
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(self.free_regs.clone());
    }

    pub fn exit_scope(&mut self) {
        self.free_regs = self
            .scopes
            .pop()
            .unwrap_or_else(|| panic!("cannot exit global scope"));
    }
}
