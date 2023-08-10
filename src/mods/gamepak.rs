pub const VBLNK_IV:u16 = 0x0040; // V-blank interrupt vector
pub const LSTAT_IV:u16 = 0x0048; // LCD Stat interrupt vector
pub const TIMER_IV:u16 = 0x0050; // Timer interrupt vector
pub const SRIAL_IV:u16 = 0x0058; // Serial interrupt vector
pub const JYPAD_IV:u16 = 0x0060; // Joypad interrupt vector

pub struct GamePak {
    pub rom: [u8; 0x8000],
    pub ram: [u8; 0x2000]
}

impl GamePak {
    pub fn init() -> Self {
        GamePak { 
            rom: [0; 0x8000], // TODO: Put a real rom here
            ram: [0; 0x2000]
        }
    }
}