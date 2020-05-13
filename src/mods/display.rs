use sdl2::pixels::Color;
use sdl2::render;
use sdl2::video;

use crate::memory;
use crate::memory::MemMap;

////////////////
// LCD struct //
////////////////
pub struct LCD {}

impl LCD {
    pub fn init() -> LCD {
        LCD {}
    }

    // V-Blank interrupt occurs at LY = 144
    // V-Blank lasts 1.1 milliseconds
    // Occurs 59.7 times per second
    pub fn begin_vblank(&mut self, mem:&mut MemMap) {
        if mem.get_byte(memory::STAT as usize) & memory::STAT_VBI > 0 {
            // Set V-Blank interrupt
            let get_if = mem.get_byte(memory::IF as usize);
            mem.set_memory(memory::IF as usize, get_if | memory::IF_VBLNK);
        }

        // Set V-Blank mode flag in STAT register
        let get_stat = mem.get_byte(memory::STAT as usize);
        mem.set_memory(memory::STAT as usize, get_stat | memory::STAT_MFV)
    }

    // OAM search for 1 scanline after V-Blank or H-Blank when LY < 144
    pub fn begin_oam(&mut self, mem:&mut MemMap) {
        if mem.get_byte(memory::STAT as usize) & memory::STAT_OAM > 0 {
            // Set OAM interrupt
            let get_if = mem.get_byte(memory::IF as usize);
            // TODO: What the hell is OAM interrupt?
        }

        // Set OAM search mode flag in STAT register
        let get_stat = mem.get_byte(memory::STAT as usize);
        mem.set_memory(memory::STAT as usize, get_stat | memory::STAT_MFO)
    }
}
