use crate::ast::*;
use crate::typecheck::{Type, TypeDesc, TypeRef};
use derive_more::{Add, AddAssign};
use fnv::FnvHashMap;
use std::iter;
use std::ops::Mul;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

const STACK_PTR_REG: Reg = Reg::R31;

enum CodegenError {
    UnsizedType(Rc<TypeDesc>),
    InfiniteSize(Rc<TypeDesc>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
enum Reg {
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
            Reg::AddrOffset | Reg::Null | STACK_PTR_REG => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, Copy, Add, AddAssign)]
struct StackOffset(u32);

impl Mul<u32> for StackOffset {
    type Output = StackOffset;

    fn mul(self, rhs: u32) -> StackOffset {
        StackOffset(self.0 * rhs)
    }
}

#[derive(Debug, Clone, Copy, Add, AddAssign)]
struct RegOffset(u32);

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

struct LayoutCache {
    generation: u32,
    layouts: FnvHashMap<TypeRef, (u32, Layout)>,
}

impl LayoutCache {
    fn new() -> LayoutCache {
        LayoutCache {
            generation: 0,
            layouts: FnvHashMap::default(),
        }
    }

    fn new_generation(&mut self) {
        self.generation += 1;
    }

    fn get_or_insert(&mut self, ty: TypeRef, ast: &TypedAst) -> Result<&Layout, CodegenError> {
        if self.layouts.contains_key(&ty) {
            let (last_gen, ref layout) = self.layouts[&ty];

            if last_gen < self.generation {
                return Ok(layout);
            } else {
                return Err(CodegenError::InfiniteSize(ty.desc()));
            }
        }

        // insert dummy layout to detect cyclic layouts (layouts with infinite size)
        self.layouts
            .insert(ty.clone(), (self.generation, Layout::zero_sized()));

        // generate the actual layout and update the dummy value
        let layout = Layout::from_type(&ast.types[&ty], self, ast)?;
        let layout_ref = self.layouts.get_mut(&ty).unwrap();
        *layout_ref = (self.generation, layout);

        Ok(&layout_ref.1)
    }
}

struct Layout {
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

    fn word() -> Layout {
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

    // FIXME: start a new generation somewhere
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
                let elem_layout = layouts.get_or_insert(elem_ty.clone(), ast)?;
                Layout::array(elem_layout, len)
            }
            Type::Tuple(_) => unimplemented!(),
            Type::Record(_) => unimplemented!(),
            Type::Variants(_) => unimplemented!(),
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
struct RegValue {
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
enum Value {
    Label(LabelValue),
    Reg(RegValue),
    Stack(StackValue),
}

#[derive(Debug)]
struct StackValue {
    start: StackOffset,
}

impl StackValue {
    fn offset(&self) -> StackOffset {
        self.start
    }

    fn field<'a>(&self, field: impl Into<FieldIdent<'a>>, layout: &Layout) -> StackValue {
        let offset = layout.stack_field_offset(field);

        StackValue {
            start: self.start + offset,
        }
    }
}

#[derive(Debug)]
struct LabelValue {
    label: Ident,
}

impl LabelValue {
    fn new() -> LabelValue {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

        let id = NEXT_ID.fetch_add(1, Ordering::AcqRel);

        if id == usize::max_value() {
            panic!("overflow for label ids");
        }

        LabelValue {
            label: Ident::new(format!("_label_{}", id)),
        }
    }
}

struct StackAllocator {
    start: StackOffset,
}

impl StackAllocator {
    fn new() -> StackAllocator {
        StackAllocator {
            start: StackOffset(0),
        }
    }

    fn alloc(&mut self, layout: &Layout) -> StackValue {
        let value = StackValue { start: self.start };
        self.start += layout.stack_size();
        value
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
