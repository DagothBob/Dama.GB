use std::vec;

use sdl2::pixels::Color;
use sdl2::render;
use sdl2::video;

use crate::system::Timer;
use crate::memory;
use crate::memory::MemMap;

////////////////
// LCD struct //
////////////////
pub const TILE_SIZE:u64 = 16;

pub struct LCD {
    pub transfer_is_done:bool
}

impl LCD {
    pub fn init() -> LCD {
        LCD {
            transfer_is_done:false
        }
    }

    pub fn draw_scanline(&mut self, mem:&mut MemMap) {
        // DEBUG: Draw tile memory straight to screen

    }

    // Get tile from memory using 8000-method of indexing (normal indexing)
    pub fn get_tile_8000(&mut self, mem:&mut MemMap, offset:usize) -> vec::Vec<u8> {
        let mut tile:vec::Vec<u8> = vec![0;16];

        for i in 0..tile.len() {
            tile[i] = mem.get_byte(memory::TILE_DATA1 as usize + (offset * TILE_SIZE as usize) + i);
        }
        tile
    }

    // Get tile from memory using 8800-method of indexing (signed index)(kill me)
    pub fn get_tile_8800(&mut self, mem:&mut MemMap, offset:i8) -> vec::Vec<u8> {
        let mut tile:vec::Vec<u8> = vec![0;16];

        for i in 0..tile.len() {
            tile[i] = mem.get_byte(memory::TILE_DATA2 as usize - (offset as i32 * TILE_SIZE as i32) as usize - i as usize);
        }

        tile
    }

    pub fn begin_hblank(&mut self, timer:&mut Timer, mem:&mut MemMap) {
        // Set LSTAT interrupt if enabled in STAT
        if mem.get_byte(memory::STAT as usize) & memory::STAT_HBI > 0 {
            let get_if = mem.get_byte(memory::IF as usize);
            mem.set_memory(timer, memory::IF as usize, get_if | memory::IF_LSTAT);
        }

        // Set H-Blank mode (0) flag in STAT register
        let get_stat = mem.get_byte(memory::STAT as usize);
        mem.set_memory(timer, memory::STAT as usize, get_stat | memory::STAT_MFH)
    }

    // V-Blank interrupt occurs at LY = 144
    // V-Blank lasts 1.1 milliseconds
    // Occurs 59.7 times per second
    pub fn begin_vblank(&mut self, timer:&mut Timer, mem:&mut MemMap) {
        // Set V-Blank interrupt
        let mut get_if = mem.get_byte(memory::IF as usize);
        mem.set_memory(timer, memory::IF as usize, get_if | memory::IF_VBLNK);

        // Set LSTAT interrupt if enabled in STAT
        if mem.get_byte(memory::STAT as usize) & memory::STAT_VBI > 0 {
            get_if = mem.get_byte(memory::IF as usize);
            mem.set_memory(timer, memory::IF as usize, get_if | memory::IF_LSTAT);
        }

        // Set V-Blank mode (1) flag in STAT register
        mem.set_memory(timer, memory::STAT as usize, memory::STAT_MFV)
    }

    // OAM search for 1 scanline after V-Blank or H-Blank when LY < 144
    pub fn begin_oam(&mut self, timer:&mut Timer, mem:&mut MemMap) {
        if mem.get_byte(memory::STAT as usize) & memory::STAT_OAM > 0 {
            // Set OAM interrupt
            let get_if = mem.get_byte(memory::IF as usize);
            mem.set_memory(timer, memory::IF as usize, get_if | memory::IF_LSTAT);
        }

        // Set OAM search mode (2) flag in STAT register
        mem.set_memory(timer, memory::STAT as usize, memory::STAT_MFO)
    }

    // LCD transfer for 168 - 291 cycles after OAM search
    pub fn begin_lcd_transfer(&mut self, timer:&mut Timer, mem:&mut MemMap) {
        // Set LCD transfer mode (3) flag in STAT register
        mem.set_memory(timer, memory::STAT as usize, memory::STAT_MFT)
    }

    pub fn get_video_mode(&mut self, mem:&mut MemMap) -> u8 {
        // Mask STAT register. MFG because it is 0b11
        mem.get_byte(memory::STAT as usize) & memory::STAT_MFT
    }
}
