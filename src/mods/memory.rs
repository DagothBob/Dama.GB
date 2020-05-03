
pub struct MemMap {
    mem:[u8;0x10000]
}

impl MemMap {
    pub fn init() -> MemMap {
        MemMap {
            mem:[0; 0x10000]
        }
    }

    // Memory addresses from the game code or registers
    // needs to be endian-swapped for indexing
    pub fn set_memory(&mut self, addr:usize, val:u8) {
        self.mem[addr] = val
    }

    pub fn get_byte(&mut self, addr:usize) -> u8 {
        self.mem[addr]
    }

    pub fn get_word(&mut self, addr:usize) -> u16 {
        ((self.mem[addr] as u16) << 8) | self.mem[addr + 1] as u16
    }
}
