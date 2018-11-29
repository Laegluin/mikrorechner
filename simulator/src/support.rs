use memory::{Word, WORD_BYTES};

pub fn to_hex(word: Word) -> String {
    let width = WORD_BYTES as usize * 2 + 2;
    format!("{:#0width$x}", word, width = width)
}
