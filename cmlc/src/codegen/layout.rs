use crate::ast::*;
use crate::codegen::{CodegenError, STACK_FRAME_PTR_REG, TMP_OPERAND_REG, TMP_RESULT_REG};
use crate::typecheck::{Type, TypeDesc, TypeRef};
use derive_more::{Add, AddAssign};
use fnv::FnvHashMap;
use std::iter;
use std::ops::Mul;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
pub enum Reg {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
    R18,
    R19,
    R20,
    R21,
    R22,
    R23,
    R24,
    R25,
    R26,
    R27,
    R28,
    R29,
    R30,
    R31,
    AddrOffset,
    Null,
}

impl Reg {
    fn is_general_purpose(&self) -> bool {
        match *self {
            Reg::AddrOffset
            | Reg::Null
            | STACK_FRAME_PTR_REG
            | TMP_RESULT_REG
            | TMP_OPERAND_REG => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, Copy, Add, AddAssign, Ord, PartialOrd, Eq, PartialEq)]
pub struct StackOffset(pub u32);

impl Mul<u32> for StackOffset {
    type Output = StackOffset;

    fn mul(self, rhs: u32) -> StackOffset {
        StackOffset(self.0 * rhs)
    }
}

#[derive(Debug, Clone, Copy, Add, AddAssign, Ord, PartialOrd, Eq, PartialEq)]
pub struct RegOffset(pub u32);

impl Mul<u32> for RegOffset {
    type Output = RegOffset;

    fn mul(self, rhs: u32) -> RegOffset {
        RegOffset(self.0 * rhs)
    }
}

#[derive(PartialEq, Eq)]
enum FieldIdent<'a> {
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
    fn zero_sized() -> Layout {
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

    fn variants(variant_layouts: Vec<Layout>) -> Layout {
        // reserve one word for the tag, for the rest use as much space as the
        // largest variant requires
        let reg_data_start = RegOffset(1);
        let reg_field_layout = vec![(None, RegOffset(0)), (None, reg_data_start)];

        let reg_size = reg_data_start
            + variant_layouts
                .iter()
                .map(|layout| layout.reg_size())
                .max()
                .unwrap_or(RegOffset(0));

        let stack_data_start = StackOffset(4);
        let stack_field_layout = vec![(None, StackOffset(0)), (None, stack_data_start)];

        let stack_size = stack_data_start
            + variant_layouts
                .iter()
                .map(|layout| layout.stack_size())
                .max()
                .unwrap_or(StackOffset(0));

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
            Type::Variants(ref variants) => {
                let layouts = variants
                    .variants
                    .iter()
                    .map(|variant| {
                        let data_layouts = variant
                            .params
                            .iter()
                            .map(|ty| {
                                let layout = layouts.get_or_gen(ty.clone(), ast)?;
                                Ok((None, layout))
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        Ok(Layout::composite(data_layouts))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Layout::variants(layouts)
            }
            Type::Str | Type::Function(_) => {
                return Err(CodegenError::UnsizedType(Rc::new(TypeDesc::from_type(ty))));
            }
            Type::Var | Type::Int | Type::Ptr(_) | Type::PartialRecord(_) => unreachable!(),
        };

        Ok(layout)
    }

    fn stack_size(&self) -> StackOffset {
        self.stack_size
    }

    fn stack_field_offset<'a>(&self, field: impl Into<FieldIdent<'a>>) -> StackOffset {
        Layout::find_field_offset(field.into(), &self.stack_field_layout)
    }

    fn reg_size(&self) -> RegOffset {
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
}

#[derive(Debug)]
pub enum Value {
    Label(LabelValue),
    Reg(RegValue),
    Stack(StackValue),
}

#[derive(Debug)]
pub struct LabelValue {
    pub label: Ident,
}

impl LabelValue {
    pub fn new(desc: impl AsRef<str>) -> LabelValue {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

        let id = NEXT_ID.fetch_add(1, Ordering::AcqRel);

        if id == usize::max_value() {
            panic!("overflow for label ids");
        }

        LabelValue {
            label: Ident::new(format!("label_{}_{}", id, desc.as_ref())),
        }
    }
}

#[derive(Debug)]
pub struct RegValue {
    regs: Vec<Reg>,
}

impl RegValue {
    fn reg(&self) -> Reg {
        self.regs[0]
    }

    fn field<'a>(&self, field: impl Into<FieldIdent<'a>>, layout: &Layout) -> RegValue {
        let offset = layout.reg_field_offset(field);

        RegValue {
            regs: self.regs[offset.0 as usize..].iter().cloned().collect(),
        }
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

struct RegAllocator {
    free_regs: Vec<Reg>,
}

impl RegAllocator {
    fn new() -> RegAllocator {
        RegAllocator {
            free_regs: Reg::iter().filter(Reg::is_general_purpose).collect(),
        }
    }

    fn alloc(&mut self, layout: &Layout) -> Option<RegValue> {
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
}

pub struct StackAllocator {
    frame_start: StackOffset,
}

impl StackAllocator {
    pub fn new() -> StackAllocator {
        StackAllocator {
            frame_start: StackOffset(0),
        }
    }

    pub fn alloc(&mut self, layout: &Layout) -> StackValue {
        let value = StackValue {
            start: self.frame_start,
        };

        self.frame_start += layout.stack_size();
        value
    }
}
