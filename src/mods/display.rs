use std::vec;

use sdl2::video::Window;
use sdl2::pixels::Color;
use sdl2::rect::Point;
use sdl2::render;
use sdl2::render::Canvas;
use sdl2::render::Texture;
use sdl2::video;

use crate::system;
use crate::system::Timer;
use crate::memory;
use crate::memory::MemMap;

////////////////
// LCD struct //
////////////////
pub const TILE_SIZE:u64 = 16;
pub const TILES_PER_SCANLINE:u64 = 20;

pub const TWO_BIT_MASKS:[u8;4] = [memory::BGP_COL1, memory::BGP_COL2, memory::BGP_COL3, memory::BGP_COL4];

pub struct LCD {
    pub transfer_is_done:bool
}

impl LCD {
    pub fn init() -> LCD {
        LCD {
            transfer_is_done:false
        }
    }

    pub fn draw_scanline(&mut self, mem:&mut MemMap, canvas:&mut Canvas<Window>, screen_texture:&mut Texture) {
        let get_ly = mem.get_byte(memory::LY as usize);
        let mut scanline_index = 0;
        let mut scanline_tiles:Vec<Vec<u8>> = Vec::new();

        // DEBUG: Draw tile memory straight to screen
        for i in 0..scanline_tiles.len() {
            scanline_tiles[i] = self.get_tile_8000(mem, (get_ly as usize * TILES_PER_SCANLINE as usize) + i);
        }

        match canvas.with_texture_canvas(screen_texture, |texture_canvas| {
            for tile in scanline_tiles {
                let tile_pixels = LCD::get_tile_pixels(mem, tile);

                for i in (get_ly % 8)..(get_ly % 8) + 8 {
                    texture_canvas.set_draw_color(tile_pixels[i as usize]);
                    match texture_canvas.draw_point(Point::new(scanline_index, get_ly as i32)) {
                        Ok(point) => {},
                        Err(error) => {
                            panic!("Failure drawing point to screen: {}", error);
                        }
                    }
                    scanline_index += 1;
                }
            }
        }) {
            Ok(result) => {},
            Err(error) => {
                panic!("Failure updating screen texture: {}", error);
            }
        }
    }

    // Get tile from memory using 8000-method of indexing (normal indexing)
    pub fn get_tile_8000(&mut self, mem:&mut MemMap, offset:usize) -> vec::Vec<u8> {
        let mut tile:vec::Vec<u8> = vec![0;TILE_SIZE as usize];

        for i in 0..tile.len() {
            tile[i] = mem.get_byte(memory::TILE_DATA0 as usize + (offset * TILE_SIZE as usize) + i);
        }
        tile
    }

    // Get tile from memory using 8800-method of indexing (signed index)(kill me)
    pub fn get_tile_8800(&mut self, mem:&mut MemMap, offset:i8) -> vec::Vec<u8> {
        let mut tile:vec::Vec<u8> = vec![0;16];

        for i in 0..tile.len() {
            tile[i] = mem.get_byte(memory::TILE_DATA1 as usize - (offset as i32 * TILE_SIZE as i32) as usize - i as usize);
        }

        tile
    }

    // Gets the current palette from BGP and converts into SDL Colors
    // then reads the provided tile Vector to produce a Vector of pixel colors
    pub fn get_tile_pixels(mem:&mut MemMap, tile:Vec<u8>) -> Vec<Color> {
        let mut gb_colors:Vec<Color> = vec![Color::BLACK;4];
        let get_bgp = mem.get_byte(memory::BGP as usize);
        
        // Normalize the palette bits and save into a vector
        for mask in TWO_BIT_MASKS.iter() {    
            let color = get_bgp & mask;
            let normalized_color:u8;

            // 192,128,64 (possible values for top 2 bits sans 0)
            if color > 48 {
                normalized_color = color >> 6;
            }
            // 48,32,16 (possible values for next 2 bits sans 0)
            else if color > 12 {
                normalized_color = color >> 4;
            }
            // 12,8,4 (possible values for 2nd-last 2 bits sans 0)
            else if color > 3 {
                normalized_color = color >> 2;
            }
            // 3,2,1,0 (possible values for last 2 bits including 0
            // which is the same if checking higher bit pairs)
            else {
                normalized_color = color;
            }

            // Convert bits into SDL2 Color
            let sdl_color:Color;

            match normalized_color {
                0 => {
                    sdl_color = Color::WHITE;
                }
                1 => {
                    sdl_color = Color::RGBA(85, 85, 85, 255);
                }
                2 => {
                    sdl_color = Color::RGBA(170, 170, 170, 255);
                }
                3 => {
                    sdl_color = Color::BLACK;
                }
                _ => { // Impossible
                    sdl_color = Color::MAGENTA;
                }
            }

            // 192,48,12,3 mod 3 = 0, but so does 0 mod 3 :(
            if (color % 3 == 0) && (color != 0) {
                gb_colors[3] = sdl_color;
            }
            else {
                gb_colors[color as usize % 3] = sdl_color;
            }
        }

        let mut pixels:Vec<Color> = Vec::new();

        // Per-byte loop (16 bytes)
        for byte in tile {
            // Per-bit-pair loop (4 per byte)
            for mask in TWO_BIT_MASKS.iter() {
                let bits = byte & mask;
                let normalized_bits:u8;
                
                if bits > 48 {
                    normalized_bits = bits >> 6;
                }
                else if bits > 12 {
                    normalized_bits = bits >> 4;
                }
                else if bits > 3 {
                    normalized_bits = bits >> 2;
                }
                else {
                    normalized_bits = bits;
                }

                pixels.push(gb_colors[normalized_bits as usize]);
            }
        }
        pixels
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
