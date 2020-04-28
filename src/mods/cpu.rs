// LR35902
use crate::memory;

////////////////
// CPU struct //
////////////////
pub struct CPU {
    registers:Registers
}

pub enum JumpCondition {
    NotZero,
    Zero,
    NotCarry,
    Carry
}

impl CPU {
    // Constructor
    pub fn init() -> CPU {
        CPU {
            registers:Registers::init()
        }
    }

    // Treat two 8 bit values as one 16 bit value
    pub fn concat_registers(most:u8, least:u8) -> u16 {
        ((most as u16) << 8) | least as u16
        
    }

    // Decrement 8 bit value
    pub fn dec_8bit(reg:u8) -> u8 {
        reg - 1
    }

    // Decrement 16 bit value
    pub fn dec_16bit(reg:u16) -> u16 {
        reg - 1
    }

    // Return most significant byte of word
    pub fn get_upper_byte(word:u16) -> u8 {
        (word >> 8) as u8
    }

    // Increment 8 bit value
    pub fn inc_8bit(reg:u8) -> u8 {
        reg + 1
    }

    // Increment 16 bit value
    pub fn inc_16bit(reg:u16) -> u16 {
        reg + 1
    }

    // opcode = opcode found in first 256, opcode2 = opcode after CB, arg = literal value
    pub fn op_match(opcode:u8, opcode2:Option<u8>, arg:u16) {
        // TODO
    }

    // opcode = opcode in CB table, arg = literal value
    pub fn op_match_cb(opcode:u8, arg:u16) {
        // TODO
    }

    pub fn op_load_8bit(from:u8) -> u8 {
        from
    }

    pub fn op_load_16bit(from:u16) -> u16 {
        from
    }

    pub fn op_push(&self, val:u16) {
        // TODO
    }

    pub fn op_pop(&self) -> u16 {
        1 // TODO
    }

    pub fn op_add_8bit(&self, val:u8) {
        // TODO
    }

    pub fn op_sub_8bit(&self, val:u8) {
        // TODO
    }

    pub fn op_and_8bit(&self, val:u8) {
        // TODO
    }

    pub fn op_or_8bit(&self, val:u8) {
        // TODO
    }

    pub fn op_xor_8bit(&self, val:u8) {
        // TODO
    }

    pub fn op_compare_8bit(&self, val:u8) {
        // TODO
    }

    pub fn op_increment_8bit(&self) {
        // TODO
    }

    pub fn op_decrement_8bit(&self) {
        // TODO
    }

    pub fn op_add_16bit(&self, val:u16) {
        // TODO
    }

    pub fn op_increment_16bit(&self) {
        // TODO
    }

    pub fn op_decrement_16bit(&self) {
        // TODO
    }

    pub fn op_swap(val:u8) -> u8 {
        1 // TODO
    }

    pub fn op_daa(&self) {
        // TODO
    }

    pub fn op_complement(&self) {
        // TODO
    }

    pub fn op_complement_carry(&self) {
        // TODO
    }

    pub fn op_set_carry(&self) {
        // TODO
    }

    pub fn op_nop() {
        // Do nothing
    }

    pub fn op_halt() {
        // TODO
    }

    pub fn op_stop() {
        // TODO
    }

    pub fn op_disable_interrupts(&self) {
        // TODO
    }

    pub fn op_enable_interrupts(&self) {
        // TODO
    }

    pub fn op_rotate_left(&self, bits:u8) {
        // TODO
    }

    pub fn op_rotate_left_carry(&self, bits:u8) {
        // TODO
    }

    pub fn op_rotate_right(&self, bits:u8) {
        // TODO
    }

    pub fn op_rotate_right_carry(&self, bits:u8) {
        // TODO
    }

    pub fn op_shift_left_carry(&self, bits:u8) {
        // TODO
    }

    pub fn op_shift_rightl_carry(&self, bits:u8) {
        // TODO
    }

    pub fn op_shift_righta_carry(&self, bits:u8) {
        // TODO
    }

    pub fn op_test_bit(&self, val:u8, bit:u8) {
        // TODO
    }

    pub fn op_set_bit(val:u8, bit:u8) -> u8 {
        1 // TODO
    }

    pub fn op_reset_bit(val:u8, bit:u8) -> u8 {
        1 // TODO
    }

    pub fn op_jump(&self, addr:u16) {
        // TODO
    }

    pub fn op_jump_if(&self, addr:u16, cond:JumpCondition) {
        // TODO
    }

    pub fn op_jump_add(&self, offset:u8) {
        // TODO
    }

    pub fn op_jump_if_add(&self, offset:u8, cond:JumpCondition) {
        // TODO
    }

    pub fn op_call(&self, addr:u16) {
        // TODO
    }

    pub fn op_call_if(&self, addr:u16, cond:JumpCondition) {
        // TODO
    }

    pub fn op_restart(&self, offset:u8) {
        // TODO
    }

    pub fn op_return(&self) {
        // TODO
    }

    pub fn op_return_if(&self, cond:JumpCondition) {
        // TODO
    }

    pub fn op_return_ei(&self) {
        // TODO
    }
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
        Registers {
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
        }
    }
}
