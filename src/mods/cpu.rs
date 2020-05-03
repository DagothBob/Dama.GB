// LR35902
use crate::memory;

////////////////
// CPU struct //
////////////////
pub struct CPU {
    registers:Registers,
    mem:memory::MemMap
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
            registers:Registers::init(),
            mem:memory::MemMap::init()
        }
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

    // Swap endianness of 16 bit word
    // for memory access, mainly.
    // For combined registers, use Registers::concat_registers(lower_byte, upper_byte)
    pub fn swap_endian(word:u16) -> u16 {
        (word >> 8) | (word << 8)
    }

    // opcode = opcode found in first 256, opcode2 = opcode after CB, arg = literal value
    pub fn op_match(&mut self, opcode:u8, opcode2:Option<u8>, arg:u16) {
        match opcode {
            0x00 => { // NOP
                CPU::op_nop()
            },
            0x01 => { // LD BC,d16
                self.op_load_16bits(RegisterPairs::bc, arg)
            },
            0x02 => { // LD (BC),A
                self.mem.set_memory(Registers::concat_registers(self.registers.c, self.registers.b) as usize, self.registers.a)
            },
            0x03 => { // INC BC
            },
            0x04 => { // INC B
            },
            0x05 => { // DEC B
            },
            0x06 => { // LD B,d8
                self.registers.b = CPU::op_load_8bit(arg as u8)
            },
            0x07 => { // RLCA
            },
            0x08 => { // LD (a16),SP
                self.registers.sp = CPU::op_load_16bit(arg)
            },
            0x09 => { // ADD HL,BC
            },
            0x0A => { // LD A,(BC)
                self.registers.a = self.mem.get_byte(Registers::concat_registers(self.registers.c, self.registers.b) as usize)
            },
            0x0B => { // DEC BC
            },
            0x0C => { // INC C
            },
            0x0D => { // DEC C
            },
            0x0E => { // LD C,d8
                self.registers.c = CPU::op_load_8bit(arg as u8)
            },
            0x0F => { // RRCA
            },
            0x10 => { // STOP
            },
            0x11 => { // LD DE,d16
                self.op_load_16bits(RegisterPairs::de, arg)
            },
            0x12 => { // LD (DE),A
            },
            0x13 => { // INC DE
            },
            0x14 => { // INC D
            },
            0x15 => { // DEC D
            },
            0x16 => { // LD D,d8
                self.registers.d = CPU::op_load_8bit(arg as u8)
            },
            0x17 => { // RLA
            },
            0x18 => { // JR r8
            },
            0x19 => { // ADD HL,DE
            },
            0x1A => { // LD A,(DE)
            },
            0x1B => { // DEC DE
            },
            0x1C => { // INC E
            },
            0x1D => { // DEC E
            },
            0x1E => { // LD E,d8
                self.registers.e = CPU::op_load_8bit(arg as u8)
            },
            0x1F => { // RRA
            },
            0x20 => { // JR NZ,r8
            },
            0x21 => { // LD HL,d16
                self.op_load_16bits(RegisterPairs::hl, arg)
            },
            0x22 => { // LD (HL+),A
            },
            0x23 => { // INC HL
            },
            0x24 => { // INC H
            },
            0x25 => { // DEC H
            },
            0x26 => { // LD H,d8
                self.registers.h = CPU::op_load_8bit(arg as u8)
            },
            0x27 => { // DAA
            },
            0x28 => { // JR Z,r8
            },
            0x29 => { // ADD HL,HL
            },
            0x2A => { // LD A,(HL+)
            },
            0x2B => { // DEC HL
            },
            0x2C => { // INC L
            },
            0x2D => { // DEC L
            },
            0x2E => { // LD L,d8
                self.registers.l = CPU::op_load_8bit(arg as u8)
            },
            0x2F => { // CPL
            },
            0x30 => { // JR NC,r8
            },
            0x31 => { // LD SP,d16
                self.registers.sp = CPU::op_load_16bit(arg)
            },
            0x32 => { // LD (HL-),A
            },
            0x33 => { // INC SP
            },
            0x34 => { // INC (HL)
            },
            0x35 => { // DEC (HL)
            },
            0x36 => { // LD (HL),d8
                self.mem.set_memory(Registers::concat_registers(self.registers.l, self.registers.h) as usize, arg as u8)
            },
            0x37 => { // SCF
            },
            0x38 => { // JR C,r8
            },
            0x39 => { // ADD HL,SP
            },
            0x3A => { // LD A,(HL-)
            },
            0x3B => { // DEC SP
            },
            0x3C => { // INC A
            },
            0x3D => { // DEC A
            },
            0x3E => { // LD A,d8
                self.registers.a = CPU::op_load_8bit(arg as u8)
            },
            0x3F => { // CCF
            },
            0x40 => { // LD B,B
                self.registers.b = CPU::op_load_8bit(self.registers.b)
            },
            0x41 => { // LD B,C
                self.registers.b = CPU::op_load_8bit(self.registers.c)
            },
            0x42 => { // LD B,D
                self.registers.b = CPU::op_load_8bit(self.registers.d)
            },
            0x43 => { // LD B,E
                self.registers.b = CPU::op_load_8bit(self.registers.e)
            },
            0x44 => { // LD B,H
                self.registers.b = CPU::op_load_8bit(self.registers.h)
            },
            0x45 => { // LD B,L
                self.registers.b = CPU::op_load_8bit(self.registers.l)
            },
            0x46 => { // LD B,(HL)
                self.registers.b = CPU::op_load_8bit(self.mem.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize))
            },
            0x47 => { // LD B,A
                self.registers.b = CPU::op_load_8bit(self.registers.a)
            },
            0x48 => { // LD C,B
                self.registers.c = CPU::op_load_8bit(self.registers.b)
            },
            0x49 => { // LD C,C
                self.registers.c = CPU::op_load_8bit(self.registers.c)
            },
            0x4A => { // LD C,D
                self.registers.c = CPU::op_load_8bit(self.registers.d)
            },
            0x4B => { // LD C,E
                self.registers.c = CPU::op_load_8bit(self.registers.e)
            },
            0x4C => { // LD C,H
                self.registers.c = CPU::op_load_8bit(self.registers.h)
            },
            0x4D => { // LD C,L
                self.registers.c = CPU::op_load_8bit(self.registers.l)
            },
            0x4E => { // LD C,(HL)
                self.registers.c = CPU::op_load_8bit(self.mem.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize))
            },
            0x4F => { // LD C,A
                self.registers.c = CPU::op_load_8bit(self.registers.a)
            },
            0x50 => { // LD D,B
                self.registers.d = CPU::op_load_8bit(self.registers.b)
            },
            0x51 => { // LD D,C
                self.registers.d = CPU::op_load_8bit(self.registers.c)
            },
            0x52 => { // LD D,D
                self.registers.d = CPU::op_load_8bit(self.registers.d)
            },
            0x53 => { // LD D,E
                self.registers.d = CPU::op_load_8bit(self.registers.e)
            },
            0x54 => { // LD D,H
                self.registers.d = CPU::op_load_8bit(self.registers.h)
            },
            0x55 => { // LD D,L
                self.registers.d = CPU::op_load_8bit(self.registers.l)
            },
            0x56 => { // LD D,(HL)
                self.registers.d = CPU::op_load_8bit(self.mem.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize))
            },
            0x57 => { // LD D,A
                self.registers.d = CPU::op_load_8bit(self.registers.a)
            },
            0x58 => { // LD E,B
                self.registers.e = CPU::op_load_8bit(self.registers.b)
            },
            0x59 => { // LD E,C
                self.registers.e = CPU::op_load_8bit(self.registers.c)
            },
            0x5A => { // LD E,D
                self.registers.e = CPU::op_load_8bit(self.registers.d)
            },
            0x5B => { // LD E,E
                self.registers.e = CPU::op_load_8bit(self.registers.e)
            },
            0x5C => { // LD E,H
                self.registers.e = CPU::op_load_8bit(self.registers.h)
            },
            0x5D => { // LD E,L
                self.registers.e = CPU::op_load_8bit(self.registers.l)
            },
            0x5E => { // LD E,(HL)
                self.registers.e = CPU::op_load_8bit(self.mem.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize))
            },
            0x5F => { // LD E,A
                self.registers.e = CPU::op_load_8bit(self.registers.a)
            },
            0x60 => { // LD H,B
                self.registers.h = CPU::op_load_8bit(self.registers.b)
            },
            0x61 => { // LD H,C
                self.registers.h = CPU::op_load_8bit(self.registers.c)
            },
            0x62 => { // LD H,D
                self.registers.h = CPU::op_load_8bit(self.registers.d)
            },
            0x63 => { // LD H,E
                self.registers.h = CPU::op_load_8bit(self.registers.e)
            },
            0x64 => { // LD H,H - miss ya brother
                self.registers.h = CPU::op_load_8bit(self.registers.h)
            },
            0x65 => { // LD H,L
                self.registers.h = CPU::op_load_8bit(self.registers.l)
            },
            0x66 => { // LD H,(HL)
                self.registers.h = CPU::op_load_8bit(self.mem.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize))
            },
            0x67 => { // LD H,A
                self.registers.h = CPU::op_load_8bit(self.registers.a)
            },
            0x68 => { // LD L,B
                self.registers.l = CPU::op_load_8bit(self.registers.b)
            },
            0x69 => { // LD L,C
                self.registers.l = CPU::op_load_8bit(self.registers.c)
            },
            0x6A => { // LD L,D
                self.registers.l = CPU::op_load_8bit(self.registers.d)
            },
            0x6B => { // LD L,E
                self.registers.l = CPU::op_load_8bit(self.registers.e)
            },
            0x6C => { // LD L,H
                self.registers.l = CPU::op_load_8bit(self.registers.h)
            },
            0x6D => { // LD L,L
                self.registers.l = CPU::op_load_8bit(self.registers.l)
            },
            0x6E => { // LD L,(HL)
                self.registers.l = CPU::op_load_8bit(self.mem.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize))
            },
            0x6F => { // LD L,A
                self.registers.l = CPU::op_load_8bit(self.registers.a)
            },
            0x70 => { // LD (HL),B
                self.mem.set_memory(Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.b)
            },
            0x71 => { // LD (HL),C
                self.mem.set_memory(Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.c)
            },
            0x72 => { // LD (HL),D
                self.mem.set_memory(Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.d)
            },
            0x73 => { // LD (HL),E
                self.mem.set_memory(Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.e)
            },
            0x74 => { // LD (HL),H
                self.mem.set_memory(Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.h)
            },
            0x75 => { // LD (HL),L
                self.mem.set_memory(Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.l)
            },
            0x76 => { // HALT
            },
            0x77 => { // LD (HL),A
                self.mem.set_memory(Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.a)
            },
            0x78 => { // LD A,B
                self.registers.a = CPU::op_load_8bit(self.registers.b)
            },
            0x79 => { // LD A,C
                self.registers.a = CPU::op_load_8bit(self.registers.c)
            },
            0x7A => { // LD A,D
                self.registers.a = CPU::op_load_8bit(self.registers.d)
            },
            0x7B => { // LD A,E
                self.registers.a = CPU::op_load_8bit(self.registers.e)
            },
            0x7C => { // LD A,H
                self.registers.a = CPU::op_load_8bit(self.registers.h)
            },
            0x7D => { // LD A,L
                self.registers.a = CPU::op_load_8bit(self.registers.l)
            },
            0x7E => { // LD A,(HL)
                self.registers.a = CPU::op_load_8bit(self.mem.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize))
            },
            0x7F => { // LD A,A
                self.registers.a = CPU::op_load_8bit(self.registers.a)
            },
            0x80 => { // ADD A,B
            },
            0x81 => { // ADD A,C
            },
            0x82 => { // ADD A,D
            },
            0x83 => { // ADD A,E
            },
            0x84 => { // ADD A,H
            },
            0x85 => { // ADD A,L
            },
            0x86 => { // ADD A,(HL)
            },
            0x87 => { // ADD A,A
            },
            0x88 => { // ADC A,B
            },
            0x89 => { // ADC A,C
            },
            0x8A => { // ADC A,D
            },
            0x8B => { // ADC A,E
            },
            0x8C => { // ADC A,H
            },
            0x8D => { // ADC A,L
            },
            0x8E => { // ADC A,(HL)
            },
            0x8F => { // ADC A,A
            },
            0x90 => { // SUB B
            },
            0x91 => { // SUB C
            },
            0x92 => { // SUB D
            },
            0x93 => { // SUB E
            },
            0x94 => { // SUB H
            },
            0x95 => { // SUB L
            },
            0x96 => { // SUB (HL)
            },
            0x97 => { // SUB A
            },
            0x98 => { // SBC A,B
            },
            0x99 => { // SBC A,C
            },
            0x9A => { // SBC A,D
            },
            0x9B => { // SBC A,E
            },
            0x9C => { // SBC A,H
            },
            0x9D => { // SBC A,L
            },
            0x9E => { // SBC A,(HL)
            },
            0x9F => { // SBC A,A
            },
            0xA0 => { // AND B
            },
            0xA1 => { // AND C
            },
            0xA2 => { // AND D
            },
            0xA3 => { // AND E
            },
            0xA4 => { // AND H
            },
            0xA5 => { // AND L
            },
            0xA6 => { // AND (HL)
            },
            0xA7 => { // AND A
            },
            0xA8 => { // XOR B
            },
            0xA9 => { // XOR C
            },
            0xAA => { // XOR D
            },
            0xAB => { // XOR E
            },
            0xAC => { // XOR H
            },
            0xAD => { // XOR L
            },
            0xAE => { // XOR (HL)
            },
            0xAF => { // XOR A
            },
            0xB0 => { // OR B
            },
            0xB1 => { // OR C
            },
            0xB2 => { // OR D
            },
            0xB3 => { // OR E
            },
            0xB4 => { // OR H
            },
            0xB5 => { // OR L
            },
            0xB6 => { // OR (HL)
            },
            0xB7 => { // OR A
            },
            0xB8 => { // CP B
            },
            0xB9 => { // CP C
            },
            0xBA => { // CP D
            },
            0xBB => { // CP E
            },
            0xBC => { // CP H
            },
            0xBD => { // CP L
            },
            0xBE => { // CP (HL)
            },
            0xBF => { // CP A
            },
            0xC0 => { // RET NZ
            },
            0xC1 => { // POP BC
            },
            0xC2 => { // JP NZ,a16
            },
            0xC3 => { // JP a16
            },
            0xC4 => { // CALL NZ,a16
            },
            0xC5 => { // PUSH BC
            },
            0xC6 => { // ADD A,d8
            },
            0xC7 => { // RST 00H
            },
            0xC8 => { // RET Z
            },
            0xC9 => { // RET
            },
            0xCA => { // JP Z,a16
            },
            0xCB => { // PREFIX CB
            },
            0xCC => { // CALL Z,a16
            },
            0xCD => { // CALL a16
            },
            0xCE => { // ADC A,d8
            },
            0xCF => { // RST 08H
            },
            0xD0 => { // RET NC
            },
            0xD1 => { // POP DE
            },
            0xD2 => { // JP NC,a16
            },
            0xD4 => { // CALL NC,a16
            },
            0xD5 => { // PUSH DE
            },
            0xD6 => { // SUB d8
            },
            0xD7 => { // RST 10H
            },
            0xD8 => { // RET C
            },
            0xD9 => { // RETI
            },
            0xDA => { // JP C,a16
            },
            0xDC => { // CALL C,a16
            },
            0xDE => { // SBC A,d8
            },
            0xDF => { // RST 1 8H
            }
            0xE0 => { // LDH (a8),A
            },
            0xE1 => { // POP HL
            },
            0xE2 => { // LD (C),A
            },
            0xE5 => { // PUSH HL
            },
            0xE6 => { // AND d8
            },
            0xE7 => { // RST 20H
            },
            0xE8 => { // ADD SP,r8
            },
            0xE9 => { // JP (HL)
            },
            0xEA => { // LD (a16),A
            },
            0xEE => { // XOR d8
            },
            0xEF => { // RST 2 8H
            },
            0xF0 => { // LDH A,(a8)
            },
            0xF1 => { // POP AF
            },
            0xF2 => { // LD A,(C)
            },
            0xF3 => { // DI
            },
            0xF5 => { // PUSH AF
            },
            0xF6 => { // OR d8
            },
            0xF7 => { // RST 30H
            },
            0xF8 => { // LD HL,SP+r8
            },
            0xF9 => { // LD SP,HL
            },
            0xFA => { // LD A,(a16)
            },
            0xFB => { // EI
            },
            0xFE => { // CP d8
            },
            0xFF => { // RST 38H
            },
            _ => { // Unknown OPcode
                panic!("Unknown opcode {}", opcode)
            }
        }   
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

    pub fn op_load_16bits(&mut self, to:RegisterPairs, from:u16) {
        match to {
            RegisterPairs::bc => {
                self.registers.b = CPU::get_upper_byte(from);
                self.registers.c = from as u8
            }
            RegisterPairs::de => {
                self.registers.d = CPU::get_upper_byte(from);
                self.registers.e = from as u8
            }
            RegisterPairs::hl => {
                self.registers.h = CPU::get_upper_byte(from);
                self.registers.l = from as u8
            }
        }
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

pub enum RegisterPairs {
    bc,
    de,
    hl
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

    // Treat two 8 bit values as one 16 bit value
    pub fn concat_registers(most:u8, least:u8) -> u16 {
        ((most as u16) << 8) | least as u16
    }
}
