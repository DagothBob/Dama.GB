use bitfield::*;

bitfield! {
    #[derive(Clone, Copy)]
    struct LineBits(u16);
    impl Debug;
    u8;
    lsbyte, set_lsbyte: 0, 7;
    msbyte, set_msbyte: 8, 15;
}

#[derive(Clone, Copy)]
pub struct Tile {
    lines: [LineBits; 8]
}

#[derive(Clone, Copy)]
pub enum PixelColour {
    Black,
    Dark,
    Light,
    White
}

impl Tile {
    pub fn init() -> Self {
        Tile { 
            lines: [LineBits(0); 8]
        }
    }

    pub fn get_byte(&self, index: usize) -> u8 {
        return if index % 2 == 0 {
            self.lines[index / 8].lsbyte()
        }
        else {
            self.lines[index / 8].msbyte()
        };
    }

    pub fn set_byte(&mut self, index: usize, val: u8) {
        if index % 2 == 0 {
            self.lines[index / 8].set_lsbyte(val);
        }
        else {
            self.lines[index / 8].set_msbyte(val);
        }
    }
 
    pub fn get_pixel_colour(&self, index: usize) -> PixelColour {
        let line: usize = if index < 64 { index / 8 } else { panic!("Tile index exceeded: {}", index) };
        let pixel: usize = index % 8;
        let lsb: bool = self.lines[line].bit(pixel * 2);
        let msb: bool = self.lines[line].bit((pixel * 2) + 1);

        return match (msb, lsb) {
            (false, false) => PixelColour::Black,
            (false, true) => PixelColour::Dark,
            (true, false) => PixelColour::Light,
            (true, true) => PixelColour::White
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tile() {
        
        
    }
}