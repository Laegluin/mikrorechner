extern crate byteorder;
extern crate rand;
extern crate structopt;
extern crate num_enum;

mod simulator;

use simulator::Word;

enum Error {
    IllegalInstruction(Word),
    IllegalRegister(u8),
}

fn main() {
    println!("Hello, world!");
}
