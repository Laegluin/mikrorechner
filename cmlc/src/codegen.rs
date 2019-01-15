mod layout;

use crate::codegen::layout::Reg;
use crate::typecheck::TypeDesc;
use std::rc::Rc;

const STACK_FRAME_PTR_REG: Reg = Reg::R31;
const TMP_RESULT_REG: Reg = Reg::R0;
const TMP_OPERAND_REG: Reg = Reg::R1;

enum CodegenError {
    UnsizedType(Rc<TypeDesc>),
    InfiniteSize(Rc<TypeDesc>),
}
