use memory::{Word, WORD_BYTES};

pub fn to_hex(word: Word) -> String {
    let width = WORD_BYTES as usize * 2 + 2;
    format!("{:#0width$x}", word, width = width)
}

pub fn to_hex_octets(bytes: &[u8]) -> String {
    let mut string: String = bytes.iter().map(|byte| format!("{:02x} ", byte)).collect();
    string.pop();
    string
}
