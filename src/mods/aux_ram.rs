use crate::graphics::Tile;

pub const TILE_DATA0:u16 = 0x8000; // Starting offset for 8000-style tile addr
pub const TILE_DATA1:u16 = 0x8800; // Starting offset for 8800-style tile addr
pub const TILE_DATA2:u16 = 0x9000; // 
pub const BG_MAPS1:u16 = 0x9800; // Starting offset for background tile maps 1
pub const BG_MAPS2:u16 = 0x9C00; // Starting offset for background tile maps 2

pub struct AuxRAM {
    pub disp_ram: [Tile; 0x200],
    pub work_ram: [u8; 0x2000]
}

impl AuxRAM {
    pub fn init() -> Self {
        AuxRAM { 
            disp_ram: [Tile::init(); 0x200], 
            work_ram: [0; 0x2000] 
        }
    }
}