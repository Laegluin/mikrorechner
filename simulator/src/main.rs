extern crate byteorder;
extern crate rand;
extern crate structopt;
extern crate num_enum;

mod simulator;
mod memory;

use memory::Word;

enum Error {
    IllegalInstruction(Word),
    IllegalRegister(u8),
    UninitializedMemoryAccess(Word),
}

fn main() {
    println!("Hello, world!");
}
