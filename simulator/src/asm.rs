use crate::memory::Word;
use byteorder::{ByteOrder, LittleEndian};
use std::io::{self, BufRead, Read, Write};
use std::num::ParseIntError;

#[derive(Debug)]
pub enum AsmError {
    Io(io::Error),
    InvalidWord(ParseIntError),
}

/// Converts a simple text file into a binary image that can be loaded by the vm.
/// Each line is either empty, start with a `#` and is ignored, or contains a word
/// formatted in binary. Whitespace is ignored.
pub fn assemble<R, W>(src: R, mut dst: W) -> Result<(), AsmError>
where
    R: Read + BufRead,
    W: Write,
{
    for line in src.lines() {
        let line = line.map_err(AsmError::Io)?;

        // strip line comments
        let line = &line[..line.find("#").unwrap_or(line.len())];
        let line = line.trim().replace(char::is_whitespace, "");

        if line.is_empty() {
            continue;
        }

        let word = Word::from_str_radix(&line, 2).map_err(AsmError::InvalidWord)?;
        let mut bytes = [0; 4];
        LittleEndian::write_u32(&mut bytes, word);
        dst.write_all(&bytes).map_err(AsmError::Io)?;
    }

    Ok(())
}
