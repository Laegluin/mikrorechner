use byteorder::{ByteOrder, LittleEndian};
use rand;
use std::cmp::min;
use std::mem;
use std::ptr;
use vm;

pub type Word = u32;
pub const WORD_BYTES: Word = mem::size_of::<Word>() as Word;
pub const WORD_BITS: Word = WORD_BYTES * 8;
pub const OP_CODE_BITS: Word = 5;
pub const REG_REF_BITS: Word = 6;

const PAGE_LEN: usize = 4096;
const LEVEL_1_TABLE_LEN: usize = 1024;
const LEVEL_2_TABLE_LEN: usize = 1024;

/// Paged memory for a 32-bit address space, using a 2-level page table.
///
/// The first 10 bits are used as an index into the top level table, the next
/// 10 bits as an index for the first level page table, and the last 12 bits
/// as the address within a page.
pub struct Memory {
    init_value: u8,
    page_table: Level2Table,
}

impl Memory {
    /// Initializes the top level page table. Note that no memory will be initialized.
    /// When a new page is allocated, it is filled with a random value that is generated
    /// in this function.
    pub fn new() -> Memory {
        Memory {
            init_value: rand::random(),
            page_table: Level2Table::empty(),
        }
    }

    pub fn store_word(&mut self, addr: Word, value: Word) {
        let mut bytes = [0; WORD_BYTES as usize];
        LittleEndian::write_u32(&mut bytes, value);
        self.store(addr, &bytes);
    }

    /// Stores the contents of `buf` into memory, starting at `addr`.
    ///
    /// ## Panics
    /// Panics if `addr + buf.len()` would cause an overflow.
    pub fn store(&mut self, addr: Word, buf: &[u8]) {
        // make sure the address does not overflow
        assert!(buf.len() <= (Word::max_value() - addr) as usize);

        let mut ptr = addr;
        let mut remaining = buf;

        while !remaining.is_empty() {
            let (memory, _) = self.mem_ref(ptr, remaining.len() as Word);
            let memory_len = memory.len();
            memory[..].copy_from_slice(&remaining[..memory_len]);

            ptr += memory_len as Word;
            remaining = &remaining[memory_len..];
        }
    }

    pub fn load_word(&mut self, addr: Word) -> Result<Word, vm::ErrorKind> {
        let mut bytes = [0; WORD_BYTES as usize];
        self.load(addr, &mut bytes)?;
        Ok(LittleEndian::read_u32(&bytes))
    }

    /// Load all bytes starting from `addr` into `buf`.
    ///
    /// ## Errors
    /// Returns an error if the load resulted in an access of unintialized memory. The error
    /// contains the invalid address. Note that an error is not guaranteed and will only occur
    /// if the load accesses an unallocated page.
    ///
    /// ## Panics
    /// Panics if `addr + buf.len()` would cause an overflow.
    pub fn load(&mut self, addr: Word, buf: &mut [u8]) -> Result<(), vm::ErrorKind> {
        // make sure the address does not overflow
        assert!(buf.len() <= (Word::max_value() - addr) as usize);

        let buf_len = buf.len() as Word;
        let mut ptr = addr;
        let mut bytes_read = 0;

        while bytes_read < buf_len as usize {
            match self.mem_ref(ptr, buf_len - bytes_read as Word) {
                (_, true) => return Err(vm::ErrorKind::UninitializedMemoryAccess(ptr)),
                (memory, _) => {
                    let memory_len = memory.len();
                    buf[bytes_read..bytes_read + memory_len].copy_from_slice(&memory);

                    ptr += memory_len as Word;
                    bytes_read += memory_len;
                }
            }
        }

        Ok(())
    }

    /// Returns a mutable reference to the memory starting at `addr`. The returned
    /// slice is either equal to `len` or less, if a page boundary was hit. The
    /// returned bool indicates if a page fault was encountered,
    fn mem_ref(&mut self, addr: Word, len: Word) -> (&mut [u8], bool) {
        let lvl_2_idx = addr >> 22;
        let lvl_1_idx = (addr >> 12) & (Word::max_value() >> 22);
        let page_idx = addr & (Word::max_value() >> 20);

        let init_value = self.init_value;
        let mut is_page_fault = false;

        let page = self.page_table.tables[lvl_2_idx as usize]
            .get_or_insert_with(|| Box::new(Level1Table::empty()))
            .pages[lvl_1_idx as usize]
            .get_or_insert_with(|| {
                is_page_fault = true;
                Box::new([init_value; PAGE_LEN])
            });

        let start = page_idx as usize;
        let end = min(page.len(), (page_idx + len) as usize);
        (&mut page[start..end], is_page_fault)
    }
}

struct Level2Table {
    tables: [Option<Box<Level1Table>>; LEVEL_2_TABLE_LEN],
}

impl Level2Table {
    fn empty() -> Level2Table {
        unsafe {
            let mut tables: [Option<Box<Level1Table>>; LEVEL_2_TABLE_LEN] = mem::uninitialized();

            for table in &mut tables[..] {
                ptr::write(table, None);
            }

            Level2Table { tables }
        }
    }
}

struct Level1Table {
    pages: [Option<Box<[u8; PAGE_LEN]>>; LEVEL_1_TABLE_LEN],
}

impl Level1Table {
    fn empty() -> Level1Table {
        unsafe {
            let mut pages: [Option<Box<[u8; PAGE_LEN]>>; LEVEL_1_TABLE_LEN] = mem::uninitialized();

            for page in &mut pages[..] {
                ptr::write(page, None);
            }

            Level1Table { pages }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn mem_ref() {
        // first access always page fault
        let mut mem = Memory::new();
        assert!(mem.mem_ref(0, 100).1);

        let mut mem = Memory::new();
        mem.store(0, &[0; 10]);
        mem.store(10, &[1; 10]);
        assert_eq!(mem.mem_ref(0, 10), (&mut *vec![0; 10], false));
        assert_eq!(mem.mem_ref(10, 10), (&mut *vec![1; 10], false));

        assert_eq!(
            mem.mem_ref(0, 20),
            (
                &mut *vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                false
            )
        );
    }

    #[test]
    #[should_panic]
    fn store_panic_on_overflow() {
        let mut mem = Memory::new();
        mem.store(Word::max_value(), &[1, 2, 3, 4]);
    }

    #[test]
    #[should_panic]
    fn load_panic_on_overflow() {
        let mut mem = Memory::new();
        mem.load(Word::max_value(), &mut vec![0; 11]).unwrap();
    }

    #[test]
    fn load_and_store() {
        // page fault
        let mut mem = Memory::new();
        mem.load(0, &mut vec![0; 42]).unwrap_err();

        // simple load and store
        let mut mem = Memory::new();
        mem.store(0, &[1, 2, 3, 4]);

        let mut buf = vec![0; 4];
        mem.load(0, &mut buf).unwrap();
        assert_eq!(vec![1, 2, 3, 4], buf);
        let mut buf = vec![0; 2];
        mem.load(2, &mut buf).unwrap();
        assert_eq!(vec![3, 4], buf);

        // load and store across page boundaries
        let mut mem = Memory::new();
        mem.store(PAGE_LEN as Word - 3, &[1, 2, 3, 4, 5, 6]);
        let mut buf = vec![0; 6];
        mem.load(PAGE_LEN as Word - 3, &mut buf).unwrap();
        assert_eq!(vec![1, 2, 3, 4, 5, 6], buf);
    }
}
