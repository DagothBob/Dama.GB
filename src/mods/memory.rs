#![allow(non_camel_case_types)]

pub struct MemMap {
    pub mem:[u8;0x10000]
}

pub const VBLN:u16 = 0x0040; // V-blank interrupt vector
pub const LSTT:u16 = 0x0048; // LCD Stat interrupt vector
pub const TIMR:u16 = 0x0050; // Timer interrupt vector
pub const SRAL:u16 = 0x0058; // Serial interrupt vector
pub const JPAD:u16 = 0x0060; // Joypad interrupt vector
pub const P1  :u16 = 0xFF00; // Pad IO
    pub const P1_RIGHT:u8 = 0b0001_0001; // Joypad right
    pub const P1_LEFT :u8 = 0b0001_0010; // Joypad left
    pub const P1_UP   :u8 = 0b0001_0100; // Joypad up
    pub const P1_DOWN :u8 = 0b0001_1000; // Joypad down
    pub const P1_ABUTT:u8 = 0b0010_0001; // Joypad A button
    pub const P1_BBUTT:u8 = 0b0010_0010; // Joypad B button
    pub const P1_SELEC:u8 = 0b0010_0100; // Joypad Select button
    pub const P1_START:u8 = 0b0010_1000; // Joypad Start button
pub const SB  :u16 = 0xFF01; // Serial transfer data
pub const SC  :u16 = 0xFF02; // Serial IO control
pub const DIV :u16 = 0xFF04; // Divider
pub const TIMA:u16 = 0xFF05; // Timer counter
pub const TMA :u16 = 0xFF06; // Timer modulo
pub const TAC :u16 = 0xFF07; // Timer control
pub const IF  :u16 = 0xFF0F; // Interrupt flag
    pub const IF_VBLNK:u8 = 0b0000_0001; // V-Blank start
    pub const IF_LSTAT:u8 = 0b0000_0010; // LCD status
    pub const IF_TIMER:u8 = 0b0000_0100; // Timer
    pub const IF_SRIAL:u8 = 0b0000_1000; // Serial complete
    pub const IF_JYPAD:u8 = 0b0001_0000; // Joypad event
    pub const IF_ALL  :u8 = 0b0001_1111;
pub const NR10:u16 = 0xFF10; // Sound 1 (sweep)
pub const NR11:u16 = 0xFF11; // Sound 1 (wave pattern duty)
pub const NR12:u16 = 0xFF12; // Sound 1 (envelope)
pub const NR13:u16 = 0xFF13; // Sound 1 (freq low)
pub const NR14:u16 = 0xFF14; // Sound 1 (freq high)
pub const NR21:u16 = 0xFF16; // Sound 2 (wave pattern duty)
pub const NR22:u16 = 0xFF17; // Sound 2 (envelope)
pub const NR23:u16 = 0xFF18; // Sound 2 (freq low)
pub const NR24:u16 = 0xFF19; // Sound 2 (freq high)
pub const NR30:u16 = 0xFF1A; // Sound 3 (on/off)
pub const NR31:u16 = 0xFF1B; // Sound 3 (length)
pub const NR32:u16 = 0xFF1C; // Sound 3 (output level)
pub const NR33:u16 = 0xFF1D; // Sound 3 (freq low)
pub const NR34:u16 = 0xFF1E; // Sound 3 (freq high)
pub const NR41:u16 = 0xFF20; // Sound 4 (length)
pub const NR42:u16 = 0xFF21; // Sound 4 (envelope)
pub const NR43:u16 = 0xFF22; // Sound 4 (polynomial counter)
pub const NR44:u16 = 0xFF23; // Sound 4 (counter/consecutive)
pub const NR50:u16 = 0xFF24; // Channel control
pub const NR51:u16 = 0xFF25; // Sound output terminal
pub const NR52:u16 = 0xFF26; // Sound on/off
pub const LCDC:u16 = 0xFF40; // LCD control
    pub const LCDC_BGP:u8 = 0b0000_0001; // BG/Window Display/Priority
    pub const LCDC_ODE:u8 = 0b0000_0010; // OBJ (Sprite) Display Enable
    pub const LCDC_OSZ:u8 = 0b0000_0100; // OBJ (Sprite) Size
    pub const LCDC_BDS:u8 = 0b0000_1000; // BG Tile Map Display Select
    pub const LCDC_BTS:u8 = 0b0001_0000; // BG & Window Tile Data Select
    pub const LCDC_WDE:u8 = 0b0010_0000; // Window Display Enable
    pub const LCDC_WDS:u8 = 0b0100_0000; // Window Tile Map Display Select
    pub const LCDC_ENA:u8 = 0b1000_0000; // LCD Display Enable
pub const STAT:u16 = 0xFF41; // LCDC status
pub const SCY :u16 = 0xFF42; // Scroll Y
pub const SCX :u16 = 0xFF43; // Scroll X
pub const LY  :u16 = 0xFF44; // LCDC Y
pub const LYC :u16 = 0xFF45; // LCDC LY-compare
pub const DMA :u16 = 0xFF46; // DMA transfer & start address
pub const BGP :u16 = 0xFF47; // Background/Window palette data
pub const OBP0:u16 = 0xFF48; // Object palette 0 data
pub const OBP1:u16 = 0xFF49; // Object palette 1 data
pub const WY  :u16 = 0xFF4A; // Window Y position
pub const WX  :u16 = 0xFF4B; // Window X position
pub const IE  :u16 = 0xFFFF; // Interrupt enable
    pub const IE_VBLNK:u8 = 0b0000_0001; // V-Blank start
    pub const IE_LSTAT:u8 = 0b0000_0010; // LCD Status
    pub const IE_TIMER:u8 = 0b0000_0100; // Timer
    pub const IE_SRIAL:u8 = 0b0000_1000; // Serial complete
    pub const IE_JYPAD:u8 = 0b0001_0000; // Joypad event
    pub const IE_ALL  :u8 = 0b0001_1111;

impl MemMap {
    pub fn init() -> MemMap {
        let mut this = MemMap {
            mem:[0; 0x10000]
        };

        this.set_memory(0xFF10, 0x80);
        this.set_memory(0xFF11, 0x88);
        this.set_memory(0xFF12, 0xF3);
        this.set_memory(0xFF14, 0xBF);
        this.set_memory(0xFF16, 0x3F);
        this.set_memory(0xFF19, 0xBF);
        this.set_memory(0xFF1A, 0x7F);
        this.set_memory(0xFF1B, 0xFF);
        this.set_memory(0xFF1C, 0x9F);
        this.set_memory(0xFF1E, 0xBF);
        this.set_memory(0xFF20, 0xFF);
        this.set_memory(0xFF23, 0xBF);
        this.set_memory(0xFF24, 0x77);
        this.set_memory(0xFF25, 0xF3);
        this.set_memory(0xFF26, 0xF1);
        this.set_memory(0xFF40, 0x91);
        this.set_memory(0xFF47, 0xFC);
        this.set_memory(0xFF48, 0xFF);
        this.set_memory(0xFF49, 0xFF);

        this
    }

    // Memory addresses from the game code or registers
    // needs to be endian-swapped for indexing
    pub fn set_memory(&mut self, addr:usize, val:u8) {
        if addr == DIV as usize {
            self.mem[addr] = 0
        }
        else {
            if addr >= 0xE000 && addr <= 0xFDFF {
                self.mem[addr - 0x2000] = val
            }
            else if addr >= 0xC000 && addr <= 0xDDFF {
                self.mem[addr + 0x2000] = val
            }
            self.mem[addr] = val
        }
    }

    pub fn get_byte(&mut self, addr:usize) -> u8 {
        self.mem[addr]
    }

    pub fn get_word(&mut self, addr:usize) -> u16 {
        ((self.mem[addr] as u16) << 8) | self.mem[addr + 1] as u16
    }
}
