use sdl2::pixels::Color;

use crate::memory::MemMap;

pub struct LCD {
    scanline_counter:u8
}

impl LCD {
    pub fn init() -> LCD {
        LCD {
            scanline_counter:0
        }
    }

    // V-Blank interrupt occurs at LY = 144
    // V-Blank lasts 1.1 milliseconds
    // Occurs 59.7 times per second
    pub fn begin_vblank(&mut self, mem:&mut MemMap) {
        
    }
}