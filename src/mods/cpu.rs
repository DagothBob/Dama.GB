// LR35902

////////////////
// CPU struct //
////////////////
pub struct CPU {
    registers:Registers
}

//////////////////////
// Registers struct //
//////////////////////
pub struct Registers {
    a:u8,
    b:u8,
    c:u8,
    d:u8,
    e:u8,
    f:u8,
    h:u8,
    l:u8,
    sp:u16,
    pc:u16
}

impl Registers {
    pub fn init() -> Registers {
        let this = Registers {
            a:0x01,
            b:0x00,
            c:0x13,
            d:0x00,
            e:0xD8,
            f:0xB0,
            h:0x01,
            l:0x4D,
            sp:0xFFFE,
            pc:0x0100
        };
        return this;
    }

    pub fn concat_registers(most:&u8, least:&u8) -> u16 {
        return ((*most as u16) << 8) | *least as u16;
    }

    pub fn dec_8bit(reg:u8) -> u8 {
        return reg - 1;
    }

    pub fn dec_16bit(reg:u16) -> u16 {
        return reg - 1;
    }

    pub fn get_upper_byte(word:u16) -> u8 {
        return (word >> 8) as u8;
    }

    pub fn inc_8bit(reg:u8) -> u8 {
        return reg + 1;
    }

    pub fn inc_16bit(reg:u16) -> u16 {
        return reg + 1;
    }
}
