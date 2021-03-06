/////////////////////////////
// Sharp LR35902 CPU       //
//                         //
// Structs: CPU, Registers //
/////////////////////////////
#![allow(non_camel_case_types)]

use crate::memory::MemMap;
use crate::system::Timer;

////////////////
// CPU struct //
////////////////
pub struct CPU {
    pub registers:Registers,
    pub ime:bool,            // Interrupt Master Enable flag
    pub halt:bool,           // Waiting for interrupt
    pub stop:bool            // Evidently, "VERY low power" according to gbdev
}

pub enum JumpCondition {
    NotZero,
    Zero,
    NotCarry,
    Carry
}

pub const FLAG_ZERO:u8 = 0b1000_0000;
pub const FLAG_SUBT:u8 = 0b0100_0000;
pub const FLAG_HALF:u8 = 0b0010_0000;
pub const FLAG_CARR:u8 = 0b0001_0000;

impl CPU {
    // Constructor
    pub fn init() -> CPU {
        CPU {
            registers:Registers::init(),
            ime:false,
            halt:false,
            stop:false
        }
    }

    // Entry point for fetch-decode-execute cycle
    pub fn cpu_cycle(&mut self, memory:&mut MemMap, timer:&mut Timer) {
        self.fetch(memory, timer)
    }

    // Get next opcode
    pub fn fetch(&mut self, memory:&mut MemMap, timer:&mut Timer) {
        println!(" Fetch");
        let get = memory.get_byte(self.registers.pc.swap_bytes() as usize);
        self.decode(memory, timer, get)
    }

    // Handle length of opcode args
    pub fn decode(&mut self, memory:&mut MemMap, timer:&mut Timer, opcode:u8) {
        // CB-prefixed opcodes
        if opcode == 0xCB {
            self.registers.pc = CPU::inc_16bit(self.registers.pc);
            let get_op = memory.get_byte(self.registers.pc.swap_bytes() as usize);

            self.registers.pc = CPU::inc_16bit(self.registers.pc);
            let get_arg1 = memory.get_byte(self.registers.pc.swap_bytes() as usize);

            self.registers.pc = CPU::inc_16bit(self.registers.pc);
            let get_arg2 = memory.get_byte(self.registers.pc.swap_bytes() as usize);

            let arg16 = ((get_arg1 as u16) << 8) | get_arg2 as u16;
            println!(" op: {:X} {:X} {:X} {:X}", opcode, get_op, get_arg1, get_arg2);
            self.execute(memory, timer, 0xCB, Some(get_op), arg16)
        }
        else {
            match opcode {
                // 3-byte opcodes
                0x01 | 0x08 | 0x11 | 0x21 | 0x31 | 0xC2 | 0xC3 | 0xC4 | 
                0xCA | 0xCC | 0xCD | 0xD2 | 0xD4 | 0xDA | 0xDC | 0xEA | 0xFA => {
                    self.registers.pc = CPU::inc_16bit(self.registers.pc);
                    let get_arg1 = memory.get_byte(self.registers.pc.swap_bytes() as usize);

                    self.registers.pc = CPU::inc_16bit(self.registers.pc);
                    let get_arg2 = memory.get_byte(self.registers.pc.swap_bytes() as usize);

                    let arg16 = ((get_arg1 as u16) << 8) | get_arg2 as u16;
                    println!(" op: {:X} {:X} {:X}", opcode, get_arg1, get_arg2);
                    self.execute(memory, timer, opcode, None, arg16)
                },
                // 2-byte opcodes
                0x06 | 0x0E | 0x10 | 0x16 | 0x18 | 0x1E | 0x20 | 0x26 | 
                0x28 | 0x2E | 0x30 | 0x36 | 0x38 | 0x3E | 0xC6 | 0xCE | 0xD6 |
                0xDE | 0xE0 | 0xE6 | 0xE8 | 0xEE | 0xF0 | 0xF6 | 0xF8 | 0xFE => {
                    self.registers.pc = CPU::inc_16bit(self.registers.pc);
                    let arg8 = memory.get_byte(self.registers.pc.swap_bytes() as usize);

                    println!(" op: {:X} {:X}", opcode, arg8);
                    self.execute(memory, timer, opcode, None, arg8 as u16)
                },
                // 1-byte opcodes
                _ => {
                    if opcode == 0xCB { // Should be impossible
                        panic!("CB opcode passed twice to decode()")
                    }
                    else {
                        println!(" op: {:X}", opcode);
                        self.execute(memory, timer, opcode, None, 0)
                    }
                }
            }
        }
    }

    // Do it
    pub fn execute(&mut self, memory:&mut MemMap, timer:&mut Timer, opcode:u8, opcode2:Option<u8>, arg:u16) {
        self.op_match(memory, timer, opcode, opcode2, arg)
    }

    /////////////////////
    // Utility methods //
    /////////////////////
    
    // Add 16 bit values endian-swapped for proper carrying behaviour
    pub fn add_16bit_swap(reg:u16, val:u16) -> u16 {
        reg.swap_bytes().wrapping_add(val).swap_bytes()
    }

    // Decrement 8 bit value
    pub fn dec_8bit(reg:u8) -> u8 {
        reg.wrapping_sub(1)
    }

    // Decrement 16 bit value
    // Endian-swapped, so upper byte it decremented
    pub fn dec_16bit(reg:u16) -> u16 {
        reg.swap_bytes().wrapping_sub(1).swap_bytes()
    }

    // Decrement combined 8 bit registers as a 16 bit one
    // Endian-swapped, so upper byte is decremented
    pub fn dec_16bits(&mut self, reg:RegistersEnum) {
        match reg {
            RegistersEnum::bc => {
                let concat:u16 = Registers::concat_registers(self.registers.c, self.registers.b).wrapping_sub(1).swap_bytes();
                self.registers.b = CPU::get_upper_byte(concat);
                self.registers.c = concat as u8
            },
            RegistersEnum::de => {
                let concat:u16 = Registers::concat_registers(self.registers.e, self.registers.d).wrapping_sub(1).swap_bytes();
                self.registers.d = CPU::get_upper_byte(concat);
                self.registers.e = concat as u8
            },
            RegistersEnum::hl => {
                let concat:u16 = Registers::concat_registers(self.registers.l, self.registers.h).wrapping_sub(1).swap_bytes();
                self.registers.h = CPU::get_upper_byte(concat);
                self.registers.l = concat as u8
            }
            _ => {
                panic!("Invalid registers enum passed to dec_16bits\n")
            }
        }
    }

    // Return most significant byte of word
    pub fn get_upper_byte(word:u16) -> u8 {
        (word >> 8) as u8
    }

    // Increment 8 bit value
    pub fn inc_8bit(reg:u8) -> u8 {
        reg.wrapping_add(1)
    }

    // Increment 16 bit value
    // Endian-swapped, so upper byte is incremented
    pub fn inc_16bit(reg:u16) -> u16 {
        reg.swap_bytes().wrapping_add(1).swap_bytes()
    }

    // Increment combined 8 bit registers as a 16 bit one
    // Endian-swapped, so upper byte is incremented
    pub fn inc_16bits(&mut self, reg:RegistersEnum) {
        match reg {
            RegistersEnum::bc => {
                let concat:u16 = Registers::concat_registers(self.registers.c, self.registers.b).wrapping_add(1).swap_bytes();
                self.registers.b = CPU::get_upper_byte(concat);
                self.registers.c = concat as u8
            },
            RegistersEnum::de => {
                let concat:u16 = Registers::concat_registers(self.registers.e, self.registers.d).wrapping_add(1).swap_bytes();
                self.registers.d = CPU::get_upper_byte(concat);
                self.registers.e = concat as u8
            },
            RegistersEnum::hl => {
                let concat:u16 = Registers::concat_registers(self.registers.l, self.registers.h).wrapping_add(1).swap_bytes();
                self.registers.h = CPU::get_upper_byte(concat);
                self.registers.l = concat as u8
            }
            _ => {
                panic!("Invalid registers enum passed to inc_16bits")
            }
        }
    }

    // opcode = opcode found in first 256, opcode2 = opcode after CB, arg = literal value
    pub fn op_match(&mut self, memory:&mut MemMap, timer:&mut Timer, opcode:u8, opcode2:Option<u8>, arg:u16) {
        println!(" A: {:X}\n B: {:X}\n C: {:X}\n D: {:X}\n E: {:X}\n F: {:08b}\n H: {:X}\n L: {:X}", 
                self.registers.a, self.registers.b, self.registers.c, self.registers.d, self.registers.e, self.registers.f, self.registers.h, self.registers.l);
        println!(" pc:\t{:X}", self.registers.pc.swap_bytes());
        println!(" sp:\t{:X}", self.registers.sp.swap_bytes());
        match opcode {
            0x00 => { // NOP
                CPU::op_nop();
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x01 => { // LD BC,d16
                self.op_load_16bits(RegistersEnum::bc, arg);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x02 => { // LD (BC),A
                memory.set_memory(timer, Registers::concat_registers(self.registers.c, self.registers.b) as usize, self.registers.a);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x03 => { // INC BC
                self.op_increment_16bit(RegistersEnum::bc);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x04 => { // INC B
                self.registers.b = self.op_increment_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x05 => { // DEC B
                self.registers.b = self.op_decrement_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x06 => { // LD B,d8
                self.registers.b = CPU::op_load_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x07 => { // RLCA
                self.registers.a = self.op_rotate_left_carry(self.registers.a, 1);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x08 => { // LD (a16),SP
                memory.set_memory(timer, arg.swap_bytes() as usize, self.registers.sp as u8);
                memory.set_memory(timer, arg.wrapping_add(1).swap_bytes() as usize, CPU::get_upper_byte(self.registers.sp));
                timer.scanline_hz += 20;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x09 => { // ADD HL,BC
                self.op_add_16bit(RegistersEnum::hl, Registers::concat_registers(self.registers.b, self.registers.c));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0A => { // LD A,(BC)
                self.registers.a = CPU::op_load_8bit(memory.get_byte(Registers::concat_registers(self.registers.c, self.registers.b) as usize));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0B => { // DEC BC
                self.op_decrement_16bit(RegistersEnum::bc);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0C => { // INC C
                self.registers.c = self.op_increment_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0D => { // DEC C
                self.registers.c = self.op_decrement_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0E => { // LD C,d8
                self.registers.c = CPU::op_load_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0F => { // RRCA
                self.registers.a = self.op_rotate_right_carry(self.registers.a, 1);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x10 => { // STOP
                self.op_stop();
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x11 => { // LD DE,d16
                self.op_load_16bits(RegistersEnum::de, arg);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x12 => { // LD (DE),A
                memory.set_memory(timer, Registers::concat_registers(self.registers.e, self.registers.d) as usize, self.registers.a);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x13 => { // INC DE
                self.op_increment_16bit(RegistersEnum::de);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x14 => { // INC D
                self.registers.d = self.op_increment_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x15 => { // DEC D
                self.registers.d = self.op_decrement_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x16 => { // LD D,d8
                self.registers.d = CPU::op_load_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x17 => { // RLA
                self.registers.a = self.op_rotate_left(self.registers.a, 1);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x18 => { // JR r8
                self.op_jump_add(arg as u8);
                timer.scanline_hz += 12
            },
            0x19 => { // ADD HL,DE
                self.op_add_16bit(RegistersEnum::hl, Registers::concat_registers(self.registers.d, self.registers.e));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1A => { // LD A,(DE)
                self.registers.a = memory.get_byte(Registers::concat_registers(self.registers.e, self.registers.d) as usize);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1B => { // DEC DE
                self.op_decrement_16bit(RegistersEnum::de);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1C => { // INC E
                self.registers.e = self.op_increment_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1D => { // DEC E
                self.registers.e = self.op_decrement_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1E => { // LD E,d8
                self.registers.e = CPU::op_load_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1F => { // RRA
                self.registers.a = self.op_rotate_right(self.registers.a, 1);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x20 => { // JR NZ,r8
                if self.op_jump_if_add(arg as u8, JumpCondition::NotZero) {
                    timer.scanline_hz += 12
                }
                else {
                    timer.scanline_hz += 8;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0x21 => { // LD HL,d16
                self.op_load_16bits(RegistersEnum::hl, arg);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x22 => { // LD (HL+),A
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.a);
                self.inc_16bits(RegistersEnum::hl);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x23 => { // INC HL
                self.op_increment_16bit(RegistersEnum::hl);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x24 => { // INC H
                self.registers.h = self.op_increment_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x25 => { // DEC H
                self.registers.h = self.op_decrement_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x26 => { // LD H,d8
                self.registers.h = CPU::op_load_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x27 => { // DAA
                self.op_daa();
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x28 => { // JR Z,r8
                if self.op_jump_if_add(arg as u8, JumpCondition::Zero) {
                    timer.scanline_hz += 12
                }
                else {
                    timer.scanline_hz += 8;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0x29 => { // ADD HL,HL
                self.op_add_16bit(RegistersEnum::hl, Registers::concat_registers(self.registers.h, self.registers.l));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2A => { // LD A,(HL+)
                self.registers.a = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.inc_16bits(RegistersEnum::hl);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2B => { // DEC HL
                self.op_decrement_16bit(RegistersEnum::hl);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2C => { // INC L
                self.registers.l = self.op_increment_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2D => { // DEC L
                self.registers.l = self.op_decrement_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2E => { // LD L,d8
                self.registers.l = CPU::op_load_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2F => { // CPL
                self.op_complement();
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x30 => { // JR NC,r8
                if self.op_jump_if_add(arg as u8, JumpCondition::NotCarry) {
                    timer.scanline_hz += 12
                }
                else {
                    timer.scanline_hz += 8;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0x31 => { // LD SP,d16
                self.registers.sp = CPU::op_load_16bit(arg);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x32 => { // LD (HL-),A
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.a);
                self.dec_16bits(RegistersEnum::hl);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x33 => { // INC SP
                self.op_increment_16bit(RegistersEnum::sp);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x34 => { // INC (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get.wrapping_add(1));
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x35 => { // DEC (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get.wrapping_sub(1));
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x36 => { // LD (HL),d8
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, arg as u8);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x37 => { // SCF
                self.op_set_carry();
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x38 => { // JR C,r8
                if self.op_jump_if_add(arg as u8, JumpCondition::Carry) {
                    timer.scanline_hz += 12
                }
                else {
                    timer.scanline_hz += 8;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0x39 => { // ADD HL,SP
                self.op_add_16bit(RegistersEnum::hl, self.registers.sp);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3A => { // LD A,(HL-)
                self.registers.a = CPU::op_load_8bit(memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize));
                self.dec_16bits(RegistersEnum::hl);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3B => { // DEC SP
                self.op_decrement_16bit(RegistersEnum::sp);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3C => { // INC A
                self.registers.a = self.op_increment_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3D => { // DEC A
                self.registers.a = self.op_decrement_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3E => { // LD A,d8
                self.registers.a = CPU::op_load_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3F => { // CCF
                self.op_complement_carry();
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x40 => { // LD B,B
                self.registers.b = CPU::op_load_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x41 => { // LD B,C
                self.registers.b = CPU::op_load_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x42 => { // LD B,D
                self.registers.b = CPU::op_load_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x43 => { // LD B,E
                self.registers.b = CPU::op_load_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x44 => { // LD B,H
                self.registers.b = CPU::op_load_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x45 => { // LD B,L
                self.registers.b = CPU::op_load_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x46 => { // LD B,(HL)
                self.registers.b = CPU::op_load_8bit(memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x47 => { // LD B,A
                self.registers.b = CPU::op_load_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x48 => { // LD C,B
                self.registers.c = CPU::op_load_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x49 => { // LD C,C
                self.registers.c = CPU::op_load_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4A => { // LD C,D
                self.registers.c = CPU::op_load_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4B => { // LD C,E
                self.registers.c = CPU::op_load_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4C => { // LD C,H
                self.registers.c = CPU::op_load_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4D => { // LD C,L
                self.registers.c = CPU::op_load_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4E => { // LD C,(HL)
                self.registers.c = CPU::op_load_8bit(memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4F => { // LD C,A
                self.registers.c = CPU::op_load_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x50 => { // LD D,B
                self.registers.d = CPU::op_load_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x51 => { // LD D,C
                self.registers.d = CPU::op_load_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x52 => { // LD D,D
                self.registers.d = CPU::op_load_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x53 => { // LD D,E
                self.registers.d = CPU::op_load_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x54 => { // LD D,H
                self.registers.d = CPU::op_load_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x55 => { // LD D,L
                self.registers.d = CPU::op_load_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x56 => { // LD D,(HL)
                self.registers.d = CPU::op_load_8bit(memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x57 => { // LD D,A
                self.registers.d = CPU::op_load_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x58 => { // LD E,B
                self.registers.e = CPU::op_load_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x59 => { // LD E,C
                self.registers.e = CPU::op_load_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5A => { // LD E,D
                self.registers.e = CPU::op_load_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5B => { // LD E,E
                self.registers.e = CPU::op_load_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5C => { // LD E,H
                self.registers.e = CPU::op_load_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5D => { // LD E,L
                self.registers.e = CPU::op_load_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5E => { // LD E,(HL)
                self.registers.e = CPU::op_load_8bit(memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5F => { // LD E,A
                self.registers.e = CPU::op_load_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x60 => { // LD H,B
                self.registers.h = CPU::op_load_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x61 => { // LD H,C
                self.registers.h = CPU::op_load_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x62 => { // LD H,D
                self.registers.h = CPU::op_load_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x63 => { // LD H,E
                self.registers.h = CPU::op_load_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x64 => { // LD H,H - miss ya brother
                self.registers.h = CPU::op_load_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x65 => { // LD H,L
                self.registers.h = CPU::op_load_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x66 => { // LD H,(HL)
                self.registers.h = CPU::op_load_8bit(memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x67 => { // LD H,A
                self.registers.h = CPU::op_load_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x68 => { // LD L,B
                self.registers.l = CPU::op_load_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x69 => { // LD L,C
                self.registers.l = CPU::op_load_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6A => { // LD L,D
                self.registers.l = CPU::op_load_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6B => { // LD L,E
                self.registers.l = CPU::op_load_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6C => { // LD L,H
                self.registers.l = CPU::op_load_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6D => { // LD L,L
                self.registers.l = CPU::op_load_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6E => { // LD L,(HL)
                self.registers.l = CPU::op_load_8bit(memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6F => { // LD L,A
                self.registers.l = CPU::op_load_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x70 => { // LD (HL),B
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.b);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x71 => { // LD (HL),C
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.c);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x72 => { // LD (HL),D
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.d);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x73 => { // LD (HL),E
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.e);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x74 => { // LD (HL),H
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.h);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x75 => { // LD (HL),L
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.l);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x76 => { // HALT
                self.op_halt();
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x77 => { // LD (HL),A
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, self.registers.a);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x78 => { // LD A,B
                self.registers.a = CPU::op_load_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x79 => { // LD A,C
                self.registers.a = CPU::op_load_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7A => { // LD A,D
                self.registers.a = CPU::op_load_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7B => { // LD A,E
                self.registers.a = CPU::op_load_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7C => { // LD A,H
                self.registers.a = CPU::op_load_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7D => { // LD A,L
                self.registers.a = CPU::op_load_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7E => { // LD A,(HL)
                self.registers.a = CPU::op_load_8bit(memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7F => { // LD A,A
                self.registers.a = CPU::op_load_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x80 => { // ADD A,B
                self.op_add_8bit(self.registers.b, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x81 => { // ADD A,C
                self.op_add_8bit(self.registers.c, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x82 => { // ADD A,D
                self.op_add_8bit(self.registers.d, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x83 => { // ADD A,E
                self.op_add_8bit(self.registers.e, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x84 => { // ADD A,H
                self.op_add_8bit(self.registers.h, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x85 => { // ADD A,L
                self.op_add_8bit(self.registers.l, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x86 => { // ADD A,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_add_8bit(get, false);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x87 => { // ADD A,A
                self.op_add_8bit(self.registers.a, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x88 => { // ADC A,B
                self.op_add_8bit(self.registers.b, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x89 => { // ADC A,C
                self.op_add_8bit(self.registers.c, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8A => { // ADC A,D
                self.op_add_8bit(self.registers.d, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8B => { // ADC A,E
                self.op_add_8bit(self.registers.e, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8C => { // ADC A,H
                self.op_add_8bit(self.registers.h, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8D => { // ADC A,L
                self.op_add_8bit(self.registers.l, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8E => { // ADC A,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_add_8bit(get, true);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8F => { // ADC A,A
                self.op_add_8bit(self.registers.a, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x90 => { // SUB B
                self.op_sub_8bit(self.registers.b, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x91 => { // SUB C
                self.op_sub_8bit(self.registers.c, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x92 => { // SUB D
                self.op_sub_8bit(self.registers.d, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x93 => { // SUB E
                self.op_sub_8bit(self.registers.e, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x94 => { // SUB H
                self.op_sub_8bit(self.registers.h, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x95 => { // SUB L
                self.op_sub_8bit(self.registers.l, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x96 => { // SUB (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_sub_8bit(get, false);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x97 => { // SUB A
                self.op_sub_8bit(self.registers.a, false);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x98 => { // SBC A,B
                self.op_sub_8bit(self.registers.b, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x99 => { // SBC A,C
                self.op_sub_8bit(self.registers.c, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9A => { // SBC A,D
                self.op_sub_8bit(self.registers.d, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9B => { // SBC A,E
                self.op_sub_8bit(self.registers.e, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9C => { // SBC A,H
                self.op_sub_8bit(self.registers.h, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9D => { // SBC A,L
                self.op_sub_8bit(self.registers.l, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9E => { // SBC A,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_sub_8bit(get, true);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9F => { // SBC A,A
                self.op_sub_8bit(self.registers.a, true);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA0 => { // AND B
                self.op_and_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA1 => { // AND C
                self.op_and_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA2 => { // AND D
                self.op_and_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA3 => { // AND E
                self.op_and_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA4 => { // AND H
                self.op_and_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA5 => { // AND L
                self.op_and_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA6 => { // AND (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_and_8bit(get);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA7 => { // AND A
                self.op_and_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA8 => { // XOR B
                self.op_xor_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA9 => { // XOR C
                self.op_xor_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAA => { // XOR D
                self.op_xor_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAB => { // XOR E
                self.op_xor_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAC => { // XOR H
                self.op_xor_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAD => { // XOR L
                self.op_xor_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAE => { // XOR (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_xor_8bit(get);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAF => { // XOR A
                self.op_xor_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB0 => { // OR B
                self.op_or_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB1 => { // OR C
                self.op_or_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB2 => { // OR D
                self.op_or_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB3 => { // OR E
                self.op_or_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB4 => { // OR H
                self.op_or_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB5 => { // OR L
                self.op_or_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB6 => { // OR (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_or_8bit(get);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB7 => { // OR A
                self.op_or_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB8 => { // CP B
                self.op_compare_8bit(self.registers.b);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB9 => { // CP C
                self.op_compare_8bit(self.registers.c);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBA => { // CP D
                self.op_compare_8bit(self.registers.d);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBB => { // CP E
                self.op_compare_8bit(self.registers.e);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBC => { // CP H
                self.op_compare_8bit(self.registers.h);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBD => { // CP L
                self.op_compare_8bit(self.registers.l);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBE => { // CP (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_compare_8bit(get);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBF => { // CP A
                self.op_compare_8bit(self.registers.a);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC0 => { // RET NZ
                if self.op_return_if(memory, JumpCondition::NotZero) {
                    timer.scanline_hz += 20
                }
                else {
                    timer.scanline_hz += 8;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xC1 => { // POP BC
                self.registers.c = self.op_pop(memory);
                self.registers.b = self.op_pop(memory);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC2 => { // JP NZ,a16
                if self.op_jump_if(arg, JumpCondition::NotZero) {
                    timer.scanline_hz += 16
                }
                else {
                    timer.scanline_hz += 12;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xC3 => { // JP a16
                self.op_jump(arg);
                timer.scanline_hz += 16
            },
            0xC4 => { // CALL NZ,a16
                if self.op_call_if(timer, memory, JumpCondition::NotZero) {
                    timer.scanline_hz += 24
                }
                else {
                    timer.scanline_hz += 12;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xC5 => { // PUSH BC
                self.op_push(timer, memory, self.registers.b);
                self.op_push(timer, memory, self.registers.c);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC6 => { // ADD A,d8
                self.op_add_8bit(arg as u8, false);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC7 => { // RST 00H
                self.op_restart(timer, memory, 0x00);
                timer.scanline_hz += 16
            },
            0xC8 => { // RET Z
                if self.op_return_if(memory, JumpCondition::Zero) {
                    timer.scanline_hz += 20
                }
                else {
                    timer.scanline_hz += 8;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xC9 => { // RET
                self.op_return(memory);
                timer.scanline_hz += 16
            },
            0xCA => { // JP Z,a16
                if self.op_jump_if(arg, JumpCondition::Zero) {
                    timer.scanline_hz += 16
                }
                else {
                    timer.scanline_hz += 12;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xCB => { // PREFIX CB
                self.op_match_cb(memory, timer, opcode2.unwrap(), arg);
                timer.scanline_hz += 4;
            },
            0xCC => { // CALL Z,a16
                if self.op_call_if(timer, memory, JumpCondition::Zero) {
                    timer.scanline_hz += 24
                }
                else {
                    timer.scanline_hz += 12;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xCD => { // CALL a16
                self.op_call(timer, memory);
                timer.scanline_hz += 24
            },
            0xCE => { // ADC A,d8
                self.op_add_8bit(arg as u8, true);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xCF => { // RST 08H
                self.op_restart(timer, memory, 0x08);
                timer.scanline_hz += 16
            },
            0xD0 => { // RET NC
                if self.op_return_if(memory, JumpCondition::NotCarry) {
                    timer.scanline_hz += 20
                }
                else {
                    timer.scanline_hz += 8;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xD1 => { // POP DE
                self.registers.e = self.op_pop(memory);
                self.registers.d = self.op_pop(memory);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD2 => { // JP NC,a16
                if self.op_jump_if(arg, JumpCondition::NotCarry) {
                    timer.scanline_hz += 16
                }
                else {
                    timer.scanline_hz += 12;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xD4 => { // CALL NC,a16
                if self.op_call_if(timer, memory, JumpCondition::NotCarry) {
                    timer.scanline_hz += 24
                }
                else {
                    timer.scanline_hz += 12;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xD5 => { // PUSH DE
                self.op_push(timer, memory, self.registers.d);
                self.op_push(timer, memory, self.registers.e);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD6 => { // SUB d8
                self.op_sub_8bit(arg as u8, false);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD7 => { // RST 10H
                self.op_restart(timer, memory, 0x10);
                timer.scanline_hz += 16
            },
            0xD8 => { // RET C
                if self.op_return_if(memory, JumpCondition::Carry) {
                    timer.scanline_hz += 20
                }
                else {
                    timer.scanline_hz += 8;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xD9 => { // RETI
                self.op_return_ei(timer, memory);
                timer.scanline_hz += 16
            },
            0xDA => { // JP C,a16
                if self.op_jump_if(arg, JumpCondition::Carry) {
                    timer.scanline_hz += 16
                }
                else {
                    timer.scanline_hz += 12;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xDC => { // CALL C,a16
                if self.op_call_if(timer, memory, JumpCondition::Carry) {
                    timer.scanline_hz += 24
                }
                else {
                    timer.scanline_hz += 12;
                    self.registers.pc = CPU::inc_16bit(self.registers.pc)
                }
            },
            0xDE => { // SBC A,d8
                self.op_sub_8bit(arg as u8, true);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xDF => { // RST 18H
                self.op_restart(timer, memory, 0x18);
                timer.scanline_hz += 16
            }
            0xE0 => { // LDH (a8),A
                memory.set_memory(timer, (0x00FFu16.wrapping_add((arg as u8) as u16)).swap_bytes() as usize, self.registers.a);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE1 => { // POP HL
                self.registers.l = self.op_pop(memory);
                self.registers.h = self.op_pop(memory);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE2 => { // LD (C),A
                memory.set_memory(timer, ((0xFF00 as u16) + self.registers.c as u16) as usize, self.registers.a);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE5 => { // PUSH HL
                self.op_push(timer, memory, self.registers.h);
                self.op_push(timer, memory, self.registers.l);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE6 => { // AND d8
                self.op_and_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE7 => { // RST 20H
                self.op_restart(timer, memory, 0x20);
                timer.scanline_hz += 16
            },
            0xE8 => { // ADD SP,r8
                self.op_add_16bit(RegistersEnum::sp, arg);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE9 => { // JP (HL)
                self.op_jump(Registers::concat_registers(self.registers.h, self.registers.l));
                timer.scanline_hz += 4;
            },
            0xEA => { // LD (a16),A
                memory.set_memory(timer, arg.swap_bytes() as usize, self.registers.a);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xEE => { // XOR d8
                self.op_xor_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xEF => { // RST 28H
                self.op_restart(timer, memory, 0x28);
                timer.scanline_hz += 16
            },
            0xF0 => { // LDH A,(a8)
                self.registers.a = CPU::op_load_8bit(memory.get_byte((0xFF00 + (arg as u8) as u16) as usize));
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF1 => { // POP AF
                self.registers.f = self.op_pop(memory);
                self.registers.a = self.op_pop(memory);
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF2 => { // LD A,(C)
                self.registers.a = CPU::op_load_8bit(memory.get_byte((0xFF00 + (self.registers.c) as u16) as usize));
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF3 => { // DI
                self.op_disable_interrupts(timer, memory);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF5 => { // PUSH AF
                self.op_push(timer, memory, self.registers.a);
                self.op_push(timer, memory, self.registers.f);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF6 => { // OR d8
                self.op_or_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF7 => { // RST 30H
                self.op_restart(timer, memory, 0x30);
                timer.scanline_hz += 16
            },
            0xF8 => { // LD HL,SP+r8
                let sp_new = CPU::add_16bit_swap(self.registers.sp, arg << 8);
                let old_l = self.registers.l;

                self.op_load_16bits(RegistersEnum::hl, sp_new);

                self.registers.f &= !FLAG_ZERO | !FLAG_SUBT;

                if sp_new < arg {
                    self.registers.f |= FLAG_CARR;
                }
                else {
                    self.registers.f &= !FLAG_CARR;
                }

                if old_l > self.registers.l {
                    self.registers.f |= FLAG_HALF;
                }
                else {
                    self.registers.f &= !FLAG_HALF;
                }
                timer.scanline_hz += 12;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF9 => { // LD SP,HL
                self.registers.sp = Registers::concat_registers(self.registers.h, self.registers.l);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFA => { // LD A,(a16)
                self.registers.a = CPU::op_load_8bit(memory.get_byte(arg.swap_bytes() as usize));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFB => { // EI
                self.op_enable_interrupts(timer, memory);
                timer.scanline_hz += 4;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFE => { // CP d8
                self.op_compare_8bit(arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFF => { // RST 38H
                self.op_restart(timer, memory, 0x38);
                timer.scanline_hz += 16
            },
            _ => { // Unknown OPcode
                panic!("Unknown opcode {}", opcode)
            }
        }   
    }

    // opcode = opcode in CB table, arg = literal value
    pub fn op_match_cb(&mut self, memory:&mut MemMap, timer:&mut Timer, opcode:u8, arg:u16) {
        match opcode {
            0x00 => { // RLC B
                self.registers.b = self.op_rotate_left_carry(self.registers.b, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x01 => { // RLC C
                self.registers.c = self.op_rotate_left_carry(self.registers.c, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x02 => { // RLC D
                self.registers.d = self.op_rotate_left_carry(self.registers.d, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x03 => { // RLC E
                self.registers.e = self.op_rotate_left_carry(self.registers.e, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x04 => { // RLC H
                self.registers.h = self.op_rotate_left_carry(self.registers.h, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x05 => { // RLC L
                self.registers.l = self.op_rotate_left_carry(self.registers.l, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x06 => { // RLC (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                let val = self.op_rotate_left_carry(get, arg as u8);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, val);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x07 => { // RLC A
                self.registers.a = self.op_rotate_left_carry(self.registers.a, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x08 => { // RRC B
                self.registers.b = self.op_rotate_right_carry(self.registers.b, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x09 => { // RRC C
                self.registers.c = self.op_rotate_right_carry(self.registers.c, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0A => { // RRC D
                self.registers.d = self.op_rotate_right_carry(self.registers.d, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0B => { // RRC E
                self.registers.e = self.op_rotate_right_carry(self.registers.e, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0C => { // RRC H
                self.registers.h = self.op_rotate_right_carry(self.registers.h, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0D => { // RRC L
                self.registers.l = self.op_rotate_right_carry(self.registers.l, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0E => { // RRC (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                let val = self.op_rotate_right_carry(get, arg as u8);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, val);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x0F => { // RRC A
                self.registers.a = self.op_rotate_right_carry(self.registers.a, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x10 => { // RL B
                self.registers.b = self.op_rotate_left(self.registers.b, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x11 => { // RL C
                self.registers.c = self.op_rotate_left(self.registers.c, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x12 => { // RL D
                self.registers.d = self.op_rotate_left(self.registers.d, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x13 => { // RL E
                self.registers.e = self.op_rotate_left(self.registers.e, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x14 => { // RL H
                self.registers.h = self.op_rotate_left(self.registers.h, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x15 => { // RL L
                self.registers.l = self.op_rotate_left(self.registers.l, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x16 => { // RL (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                let val = self.op_rotate_left(get, arg as u8);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, val);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x17 => { // RL A
                self.registers.a = self.op_rotate_left(self.registers.a, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x18 => { // RR B
                self.registers.b = self.op_rotate_right(self.registers.b, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x19 => { // RR C
                self.registers.c = self.op_rotate_right(self.registers.c, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1A => { // RR D
                self.registers.d = self.op_rotate_right(self.registers.d, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1B => { // RR E
                self.registers.e = self.op_rotate_right(self.registers.e, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1C => { // RR H
                self.registers.h = self.op_rotate_right(self.registers.h, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1D => { // RR L
                self.registers.l = self.op_rotate_right(self.registers.l, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1E => { // RR (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                let val = self.op_rotate_right(get, arg as u8);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, val);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x1F => { // RR A
                self.registers.a = self.op_rotate_right(self.registers.a, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x20 => { // SLA B
                self.registers.b = self.op_shift_left_carry(self.registers.b, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x21 => { // SLA C
                self.registers.c = self.op_shift_left_carry(self.registers.c, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x22 => { // SLA D
                self.registers.d = self.op_shift_left_carry(self.registers.d, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x23 => { // SLA E
                self.registers.e = self.op_shift_left_carry(self.registers.e, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x24 => { // SLA H
                self.registers.h = self.op_shift_left_carry(self.registers.h, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x25 => { // SLA L
                self.registers.l = self.op_shift_left_carry(self.registers.l, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x26 => { // SLA (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                let val = self.op_shift_left_carry(get, arg as u8);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, val);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x27 => { // SLA A
                self.registers.a = self.op_shift_left_carry(self.registers.a, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x28 => { // SRA B
                self.registers.b = self.op_shift_righta_carry(self.registers.b, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x29 => { // SRA C
                self.registers.c = self.op_shift_righta_carry(self.registers.c, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2A => { // SRA D
                self.registers.d = self.op_shift_righta_carry(self.registers.d, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2B => { // SRA E
                self.registers.e = self.op_shift_righta_carry(self.registers.e, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2C => { // SRA H
                self.registers.h = self.op_shift_righta_carry(self.registers.h, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2D => { // SRA L
                self.registers.l = self.op_shift_righta_carry(self.registers.l, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2E => { // SRA (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                let val = self.op_shift_righta_carry(get, arg as u8);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, val);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x2F => { // SRA A
                self.registers.a = self.op_shift_righta_carry(self.registers.a, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x30 => { // SWAP B
                self.registers.b = self.op_swap(self.registers.b);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x31 => { // SWAP C
                self.registers.c = self.op_swap(self.registers.c);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x32 => { // SWAP D
                self.registers.d = self.op_swap(self.registers.d);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x33 => { // SWAP E
                self.registers.e = self.op_swap(self.registers.e);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x34 => { // SWAP H
                self.registers.h = self.op_swap(self.registers.h);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x35 => { // SWAP L
                self.registers.l = self.op_swap(self.registers.l);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x36 => { // SWAP (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                let val = self.op_swap(get);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, val);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x37 => { // SWAP A
                self.registers.a = self.op_swap(self.registers.a);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x38 => { // SRL B
                self.registers.b = self.op_shift_rightl_carry(self.registers.b, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x39 => { // SRL C
                self.registers.c = self.op_shift_rightl_carry(self.registers.c, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3A => { // SRL D
                self.registers.d = self.op_shift_rightl_carry(self.registers.d, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3B => { // SRL E
                self.registers.e = self.op_shift_rightl_carry(self.registers.e, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3C => { // SRL H
                self.registers.h = self.op_shift_rightl_carry(self.registers.h, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3D => { // SRL L
                self.registers.l = self.op_shift_rightl_carry(self.registers.l, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3E => { // SRL (HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                let val = self.op_shift_rightl_carry(get, arg as u8);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, val);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x3F => { // SRL A
                self.registers.a = self.op_shift_rightl_carry(self.registers.a, arg as u8);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x40 => { // BIT 0,B
                self.op_test_bit(self.registers.b, 0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x41 => { // BIT 0,C
                self.op_test_bit(self.registers.c, 0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x42 => { // BIT 0,D
                self.op_test_bit(self.registers.d, 0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x43 => { // BIT 0,E
                self.op_test_bit(self.registers.e, 0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x44 => { // BIT 0,H
                self.op_test_bit(self.registers.h, 0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x45 => { // BIT 0,L
                self.op_test_bit(self.registers.l, 0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x46 => { // BIT 0,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_test_bit(get, 0);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x47 => { // BIT 0,A
                self.op_test_bit(self.registers.a, 0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x48 => { // BIT 1,B
                self.op_test_bit(self.registers.b, 1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x49 => { // BIT 1,C
                self.op_test_bit(self.registers.c, 1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4A => { // BIT 1,D
                self.op_test_bit(self.registers.d, 1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4B => { // BIT 1,E
                self.op_test_bit(self.registers.e, 1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4C => { // BIT 1,H
                self.op_test_bit(self.registers.h, 1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4D => { // BIT 1,L
                self.op_test_bit(self.registers.l, 1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4E => { // BIT 1,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_test_bit(get, 1);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x4F => { // BIT 1,A
                self.op_test_bit(self.registers.a, 1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x50 => { // BIT 2,B
                self.op_test_bit(self.registers.b, 2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x51 => { // BIT 2,C
                self.op_test_bit(self.registers.c, 2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x52 => { // BIT 2,D
                self.op_test_bit(self.registers.d, 2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x53 => { // BIT 2,E
                self.op_test_bit(self.registers.e, 2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x54 => { // BIT 2,H
                self.op_test_bit(self.registers.h, 2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x55 => { // BIT 2,L
                self.op_test_bit(self.registers.l, 2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x56 => { // BIT 2,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_test_bit(get, 2);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x57 => { // BIT 2,A
                self.op_test_bit(self.registers.a, 2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x58 => { // BIT 3,B
                self.op_test_bit(self.registers.b, 3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x59 => { // BIT 3,C
                self.op_test_bit(self.registers.c, 3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5A => { // BIT 3,D
                self.op_test_bit(self.registers.d, 3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5B => { // BIT 3,E
                self.op_test_bit(self.registers.e, 3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5C => { // BIT 3,H
                self.op_test_bit(self.registers.h, 3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5D => { // BIT 3,L
                self.op_test_bit(self.registers.l, 3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5E => { // BIT 3,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_test_bit(get, 3);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x5F => { // BIT 3,A
                self.op_test_bit(self.registers.a, 3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x60 => { // BIT 4,B
                self.op_test_bit(self.registers.b, 4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x61 => { // BIT 4,C
                self.op_test_bit(self.registers.c, 4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x62 => { // BIT 4,D
                self.op_test_bit(self.registers.d, 4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x63 => { // BIT 4,E
                self.op_test_bit(self.registers.e, 4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x64 => { // BIT 4,H
                self.op_test_bit(self.registers.h, 4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x65 => { // BIT 4,L
                self.op_test_bit(self.registers.l, 4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x66 => { // BIT 4,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_test_bit(get, 4);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x67 => { // BIT 4,A
                self.op_test_bit(self.registers.a, 4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x68 => { // BIT 5,B
                self.op_test_bit(self.registers.b, 5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x69 => { // BIT 5,C
                self.op_test_bit(self.registers.c, 5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6A => { // BIT 5,D
                self.op_test_bit(self.registers.d, 5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6B => { // BIT 5,E
                self.op_test_bit(self.registers.e, 5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6C => { // BIT 5,H
                self.op_test_bit(self.registers.h, 5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6D => { // BIT 5,L
                self.op_test_bit(self.registers.l, 5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6E => { // BIT 5,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_test_bit(get, 5);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x6F => { // BIT 5,A
                self.op_test_bit(self.registers.a, 5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x70 => { // BIT 6,B
                self.op_test_bit(self.registers.b, 6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x71 => { // BIT 6,C
                self.op_test_bit(self.registers.c, 6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x72 => { // BIT 6,D
                self.op_test_bit(self.registers.d, 6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x73 => { // BIT 6,E
                self.op_test_bit(self.registers.e, 6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x74 => { // BIT 6,H
                self.op_test_bit(self.registers.h, 6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x75 => { // BIT 6,L
                self.op_test_bit(self.registers.l, 6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x76 => { // BIT 6,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_test_bit(get, 6);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x77 => { // BIT 6,A
                self.op_test_bit(self.registers.a, 6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x78 => { // BIT 7,B
                self.op_test_bit(self.registers.b, 7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x79 => { // BIT 7,C
                self.op_test_bit(self.registers.c, 7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7A => { // BIT 7,D
                self.op_test_bit(self.registers.d, 7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7B => { // BIT 7,E
                self.op_test_bit(self.registers.e, 7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7C => { // BIT 7,H
                self.op_test_bit(self.registers.h, 7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7D => { // BIT 7,L
                self.op_test_bit(self.registers.l, 7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7E => { // BIT 7,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                self.op_test_bit(get, 7);
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x7F => { // BIT 7,A
                self.op_test_bit(self.registers.a, 7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x80 => { // RES 0,B
                self.registers.b &= !CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x81 => { // RES 0,C
                self.registers.c &= !CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x82 => { // RES 0,D
                self.registers.d &= !CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x83 => { // RES 0,E
                self.registers.e &= !CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x84 => { // RES 0,H
                self.registers.h &= !CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x85 => { // RES 0,L
                self.registers.l &= !CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x86 => { // RES 0,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get & !CPU::op_bit(0));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x87 => { // RES 0,A
                self.registers.a &= !CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x88 => { // RES 1,B
                self.registers.b &= !CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x89 => { // RES 1,C
                self.registers.c &= !CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8A => { // RES 1,D
                self.registers.d &= !CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8B => { // RES 1,E
                self.registers.e &= !CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8C => { // RES 1,H
                self.registers.h &= !CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8D => { // RES 1,L
                self.registers.l &= !CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8E => { // RES 1,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get & !CPU::op_bit(1));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x8F => { // RES 1,A
                self.registers.a &= !CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x90 => { // RES 2,B
                self.registers.b &= !CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x91 => { // RES 2,C
                self.registers.c &= !CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x92 => { // RES 2,D
                self.registers.d &= !CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x93 => { // RES 2,E
                self.registers.e &= !CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x94 => { // RES 2,H
                self.registers.h &= !CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x95 => { // RES 2,L
                self.registers.l &= !CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x96 => { // RES 2,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get & !CPU::op_bit(2));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x97 => { // RES 2,A
                self.registers.a &= !CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x98 => { // RES 3,B
                self.registers.b &= !CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x99 => { // RES 3,C
                self.registers.c &= !CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9A => { // RES 3,D
                self.registers.d &= !CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9B => { // RES 3,E
                self.registers.e &= !CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9C => { // RES 3,H
                self.registers.h &= !CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9D => { // RES 3,L
                self.registers.l &= !CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9E => { // RES 3,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get & !CPU::op_bit(3));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0x9F => { // RES 3,A
                self.registers.a &= !CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA0 => { // RES 4,B
                self.registers.b &= !CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA1 => { // RES 4,C
                self.registers.c &= !CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA2 => { // RES 4,D
                self.registers.d &= !CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA3 => { // RES 4,E
                self.registers.e &= !CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA4 => { // RES 4,H
                self.registers.h &= !CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA5 => { // RES 4,L
                self.registers.l &= !CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA6 => { // RES 4,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get & !CPU::op_bit(4));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA7 => { // RES 4,A
                self.registers.a &= !CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA8 => { // RES 5,B
                self.registers.b &= !CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xA9 => { // RES 5,C
                self.registers.c &= !CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAA => { // RES 5,D
                self.registers.d &= !CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAB => { // RES 5,E
                self.registers.e &= !CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAC => { // RES 5,H
                self.registers.h &= !CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAD => { // RES 5,L
                self.registers.l &= !CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAE => { // RES 5,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get & !CPU::op_bit(5));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xAF => { // RES 5,A
                self.registers.a &= !CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB0 => { // RES 6,B
                self.registers.b &= !CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB1 => { // RES 6,C
                self.registers.c &= !CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB2 => { // RES 6,D
                self.registers.d &= !CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB3 => { // RES 6,E
                self.registers.e &= !CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB4 => { // RES 6,H
                self.registers.h &= !CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB5 => { // RES 6,L
                self.registers.l &= !CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB6 => { // RES 6,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get & !CPU::op_bit(6));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB7 => { // RES 6,A
                self.registers.a &= !CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB8 => { // RES 7,B
                self.registers.b &= !CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xB9 => { // RES 7,C
                self.registers.c &= !CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBA => { // RES 7,D
                self.registers.d &= !CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBB => { // RES 7,E
                self.registers.e &= !CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBC => { // RES 7,H
                self.registers.h &= !CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBD => { // RES 7,L
                self.registers.l &= !CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBE => { // RES 7,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get & !CPU::op_bit(7));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xBF => { // RES 7,A
                self.registers.a &= !CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC0 => { // SET 0,B
                self.registers.b |= CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC1 => { // SET 0,C
                self.registers.c |= CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC2 => { // SET 0,D
                self.registers.d |= CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC3 => { // SET 0,E
                self.registers.e |= CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC4 => { // SET 0,H
                self.registers.h |= CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC5 => { // SET 0,L
                self.registers.l |= CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC6 => { // SET 0,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get | CPU::op_bit(0));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC7 => { // SET 0,A
                self.registers.a |= CPU::op_bit(0);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC8 => { // SET 1,B
                self.registers.b |= CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xC9 => { // SET 1,C
                self.registers.c |= CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xCA => { // SET 1,D
                self.registers.d |= CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xCB => { // SET 1,E
                self.registers.e |= CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xCC => { // SET 1,H
                self.registers.h |= CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xCD => { // SET 1,L
                self.registers.l |= CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xCE => { // SET 1,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get | CPU::op_bit(1));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xCF => { // SET 1,A
                self.registers.a |= CPU::op_bit(1);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD0 => { // SET 2,B
                self.registers.b |= CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD1 => { // SET 2,C
                self.registers.c |= CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD2 => { // SET 2,D
                self.registers.d |= CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD3 => { // SET 2,E
                self.registers.e |= CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD4 => { // SET 2,H
                self.registers.h |= CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD5 => { // SET 2,L
                self.registers.l |= CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD6 => { // SET 2,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get | CPU::op_bit(2));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD7 => { // SET 2,A
                self.registers.a |= CPU::op_bit(2);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD8 => { // SET 3,B
                self.registers.b |= CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xD9 => { // SET 3,C
                self.registers.c |= CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xDA => { // SET 3,D
                self.registers.d |= CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xDB => { // SET 3,E
                self.registers.e |= CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xDC => { // SET 3,H
                self.registers.h |= CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xDD => { // SET 3,L
                self.registers.l |= CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xDE => { // SET 3,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get | CPU::op_bit(3));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xDF => { // SET 3,A
                self.registers.a |= CPU::op_bit(3);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE0 => { // SET 4,B
                self.registers.b |= CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE1 => { // SET 4,C
                self.registers.c |= CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE2 => { // SET 4,D
                self.registers.d |= CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE3 => { // SET 4,E
                self.registers.e |= CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE4 => { // SET 4,H
                self.registers.h |= CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE5 => { // SET 4,L
                self.registers.l |= CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE6 => { // SET 4,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get | CPU::op_bit(4));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE7 => { // SET 4,A
                self.registers.a |= CPU::op_bit(4);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE8 => { // SET 5,B
                self.registers.b |= CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xE9 => { // SET 5,C
                self.registers.c |= CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xEA => { // SET 5,D
                self.registers.d |= CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xEB => { // SET 5,E
                self.registers.e |= CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xEC => { // SET 5,H
                self.registers.h |= CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xED => { // SET 5,L
                self.registers.l |= CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xEE => { // SET 5,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get | CPU::op_bit(5));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xEF => { // SET 5,A
                self.registers.a |= CPU::op_bit(5);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF0 => { // SET 6,B
                self.registers.b |= CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF1 => { // SET 6,C
                self.registers.c |= CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF2 => { // SET 6,D
                self.registers.d |= CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF3 => { // SET 6,E
                self.registers.e |= CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF4 => { // SET 6,H
                self.registers.h |= CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF5 => { // SET 6,L
                self.registers.l |= CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF6 => { // SET 6,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get | CPU::op_bit(6));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF7 => { // SET 6,A
                self.registers.a |= CPU::op_bit(6);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF8 => { // SET 7,B
                self.registers.b |= CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xF9 => { // SET 7,C
                self.registers.c |= CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFA => { // SET 7,D
                self.registers.d |= CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFB => { // SET 7,E
                self.registers.e |= CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFC => { // SET 7,H
                self.registers.h |= CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFD => { // SET 7,L
                self.registers.l |= CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFE => { // SET 7,(HL)
                let get = memory.get_byte(Registers::concat_registers(self.registers.l, self.registers.h) as usize);
                memory.set_memory(timer, Registers::concat_registers(self.registers.l, self.registers.h) as usize, get | CPU::op_bit(7));
                timer.scanline_hz += 16;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            },
            0xFF => { // SET 7,A
                self.registers.a |= CPU::op_bit(7);
                timer.scanline_hz += 8;
                self.registers.pc = CPU::inc_16bit(self.registers.pc)
            }
        }
    }

    //////////////
    // OP codes //
    //////////////

    pub fn op_load_8bit(from:u8) -> u8 {
        from
    }

    pub fn op_load_16bit(from:u16) -> u16 {
        from
    }

    pub fn op_load_16bits(&mut self, to:RegistersEnum, from:u16) {
        match to {
            RegistersEnum::bc => {
                self.registers.b = CPU::get_upper_byte(from);
                self.registers.c = from as u8
            }
            RegistersEnum::de => {
                self.registers.d = CPU::get_upper_byte(from);
                self.registers.e = from as u8
            }
            RegistersEnum::hl => {
                self.registers.h = CPU::get_upper_byte(from);
                self.registers.l = from as u8
            }
            _ => {
                panic!("Invalid pass of register enum to op_load_16bits")
            }
        }
    }

    pub fn op_push(&mut self, timer:&mut Timer, memory:&mut MemMap, val:u8) {
        self.registers.sp = CPU::dec_16bit(self.registers.sp);
        memory.set_memory(timer, self.registers.sp.swap_bytes() as usize, val);
    }

    pub fn op_pop(&mut self, memory:&mut MemMap) -> u8 {
        let ret = memory.get_byte(self.registers.sp.swap_bytes() as usize);
        self.registers.sp = CPU::dec_16bit(self.registers.sp);
        ret
    }

    pub fn op_add_8bit(&mut self, val:u8, check_carry:bool) {
        let old_a = self.registers.a;
        let flag_check = (self.registers.f & FLAG_CARR) > 0;
        self.registers.a = self.registers.a.wrapping_add(val);

        let carry = if check_carry && flag_check {
            self.registers.a = CPU::inc_8bit(self.registers.a);
            true
        }
        else {
            false
        };

        if self.registers.a == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f &= !FLAG_SUBT;

        if old_a > self.registers.a {
            self.registers.f |= FLAG_CARR;
        }
        else {
            self.registers.f &= !FLAG_CARR;
        }

        if carry {
            if ((((old_a & 0xF).wrapping_add((val + 1) & 0xF)) & 0x10) as i16) > 0 {
                self.registers.f |= FLAG_HALF;
            }
            else {
                self.registers.f &= !FLAG_HALF;
            }
        }
        else if ((((old_a & 0xF).wrapping_add(val & 0xF)) & 0x10) as i16) > 0 {
            self.registers.f |= FLAG_HALF;
        }
        else {
            self.registers.f &= !FLAG_HALF;
        }
    }

    pub fn op_sub_8bit(&mut self, val:u8, check_carry:bool) {
        let old_a = self.registers.a;
        self.registers.a = self.registers.a.wrapping_sub(val);

        let carry = if check_carry && (self.registers.f & FLAG_CARR) > 0 {
            self.registers.a = CPU::dec_8bit(self.registers.a);
            true
        }
        else {
            false
        };

        if self.registers.a == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f |= FLAG_SUBT;

        if old_a < self.registers.a {
            self.registers.f |= FLAG_CARR;
        }
        else {
            self.registers.f &= !FLAG_CARR;
        }

        if carry {
            if ((((old_a & 0xF).wrapping_sub((val - 1) & 0xF)) & 0x10) as i16) < 0 {
                self.registers.f |= FLAG_HALF;
            }
            else {
                self.registers.f &= !FLAG_HALF;
            }
        }
        else if ((((old_a & 0xF).wrapping_sub(val & 0xF)) & 0x10) as i16) < 0 {
            self.registers.f |= FLAG_HALF;
        }
        else {
            self.registers.f &= !FLAG_HALF;
        }
    }

    pub fn op_and_8bit(&mut self, val:u8) {
        self.registers.a &= val;

        if self.registers.a == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f &= !FLAG_SUBT;
        self.registers.f |= FLAG_HALF;
        self.registers.f &= !FLAG_CARR;
    }

    pub fn op_or_8bit(&mut self, val:u8) {
        self.registers.a |= val;

        if self.registers.a == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;
        self.registers.f &= !FLAG_CARR;
    }

    pub fn op_xor_8bit(&mut self, val:u8) {
        self.registers.a ^= val;

        if self.registers.a == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;
        self.registers.f &= !FLAG_CARR;
    }

    pub fn op_compare_8bit(&mut self, val:u8) {
        let comp = self.registers.a.wrapping_sub(val);

        if comp == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f |= FLAG_SUBT;

        if comp < self.registers.a {
            self.registers.f |= FLAG_CARR;
        }
        else {
            self.registers.f &= !FLAG_CARR;
        }

        if ((((self.registers.a & 0xF).wrapping_sub(val & 0xF)) & 0x10) as i16) < 0 {
            self.registers.f |= FLAG_HALF;
        }
        else {
            self.registers.f &= !FLAG_HALF;
        }
    }

    pub fn op_increment_8bit(&mut self, reg:u8) -> u8 {
        let old_reg = reg;
        let new_reg = CPU::inc_8bit(old_reg);

        if new_reg == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f &= !FLAG_SUBT;

        if (((old_reg & 0xF).wrapping_add(1 & 0xF)) & 0x10) > 0 {
            self.registers.f |= FLAG_HALF;
        }
        else {
            self.registers.f &= !FLAG_HALF;
        }
        new_reg
    }

    pub fn op_decrement_8bit(&mut self, reg:u8) -> u8 {
        let old_reg = reg;
        let new_reg = CPU::dec_8bit(old_reg);

        if new_reg == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f |= FLAG_SUBT;

        if ((((old_reg & 0xF).wrapping_sub(1 & 0xF)) & 0x10) as i16) < 0 {
            self.registers.f |= FLAG_HALF;
        }
        else {
            self.registers.f &= !FLAG_HALF;
        }
        new_reg
    }

    pub fn op_add_16bit(&mut self, reg:RegistersEnum, val:u16) {
        let old_reg:u16;
        let mod_reg:u16;

        match reg {
            RegistersEnum::hl => {
                old_reg = Registers::concat_registers(self.registers.h, self.registers.l);
                mod_reg = old_reg.swap_bytes().wrapping_add(val);
                self.registers.h = CPU::get_upper_byte(mod_reg);
                self.registers.l = mod_reg as u8;
            }
            RegistersEnum::sp => {
                old_reg = self.registers.sp;
                self.registers.sp = self.registers.sp.swap_bytes().wrapping_add(val);
                mod_reg = self.registers.sp;
                self.registers.f &= !FLAG_ZERO;
            }
            _ => {
                panic!("Invalid registers enum passed to op_add_16bit");
            }
        }

        self.registers.f &= !FLAG_SUBT;

        if old_reg > mod_reg {
            self.registers.f |= FLAG_CARR;
        }
        else {
            self.registers.f &= !FLAG_CARR;
        }

        if (mod_reg as u8) < (old_reg as u8) {
            self.registers.f |= FLAG_HALF;
        }
        else {
            self.registers.f &= !FLAG_HALF;
        }
    }

    pub fn op_increment_16bit(&mut self, reg:RegistersEnum) {
        match reg {
            RegistersEnum::bc => {
                self.inc_16bits(RegistersEnum::bc);
            }
            RegistersEnum::de => {
                self.inc_16bits(RegistersEnum::de);
            }
            RegistersEnum::hl => {
                self.inc_16bits(RegistersEnum::hl);
            }
            RegistersEnum::sp => {
                self.registers.sp = CPU::inc_16bit(self.registers.sp);
            }
            _ => {
                panic!("Invalid registers enum passed to op_increment_16bit");
            }
        }
    }

    pub fn op_decrement_16bit(&mut self, reg:RegistersEnum) {
        match reg {
            RegistersEnum::bc => {
                self.dec_16bits(RegistersEnum::bc);
            }
            RegistersEnum::de => {
                self.dec_16bits(RegistersEnum::de);
            }
            RegistersEnum::hl => {
                self.dec_16bits(RegistersEnum::hl);
            }
            RegistersEnum::sp => {
                self.registers.sp = CPU::dec_16bit(self.registers.sp);
            }
            _ => {
                panic!("Invalid registers enum passed to op_decrement_16bit");
            }
        }
    }

    pub fn op_swap(&mut self, val:u8) -> u8 {
        let swap = (val << 4) | (val >> 4);

        if swap == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;
        self.registers.f &= !FLAG_CARR;

        swap
    }

    pub fn op_daa(&mut self) {
        let old_a = self.registers.a;
        self.registers.a = ((old_a >> 4) * 10) + ((old_a << 4) >> 4);

        if self.registers.a == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f &= !FLAG_HALF;

        if old_a > self.registers.a {
            self.registers.f |= FLAG_CARR;
        }
        else {
            self.registers.f &= !FLAG_CARR;
        }
    }

    pub fn op_complement(&mut self) {
        self.registers.a = !self.registers.a;

        self.registers.f |= FLAG_SUBT;
        self.registers.f |= FLAG_HALF;
    }

    pub fn op_complement_carry(&mut self) {
        if (self.registers.f & FLAG_CARR) == 0 {
            self.registers.f |= FLAG_CARR;
        }
        else {
            self.registers.f &= !FLAG_CARR;
        }

        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;
    }

    pub fn op_set_carry(&mut self) {
        self.registers.f |= FLAG_CARR;
        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;
    }

    pub fn op_nop() {
        // Do nothing
    }

    pub fn op_halt(&mut self) {
        self.halt = true
    }

    pub fn op_stop(&mut self) {
        self.stop = true
    }

    pub fn op_disable_interrupts(&mut self, timer:&mut Timer, memory:&mut MemMap) {
        self.ime = false;
        memory.set_memory(timer, crate::memory::IE as usize, 0)
    }

    pub fn op_enable_interrupts(&mut self, timer:&mut Timer, memory:&mut MemMap) {
        self.ime = true;
        memory.set_memory(timer, crate::memory::IE as usize, crate::memory::IE_ALL)
    }

    pub fn op_rotate_left(&mut self, reg:u8, bits:u8) -> u8 {
        if (reg >> 7) == 0 {
            self.registers.f &= !FLAG_CARR;
        }
        else {
            self.registers.f |= FLAG_CARR;
        }

        let rot = reg.rotate_left(bits as u32);

        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;

        if rot == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }
        rot
    }

    pub fn op_rotate_left_carry(&mut self, reg:u8, bits:u8) -> u8 {
        let mut rot = reg;
        let carry_bit = if (reg >> 7) == 0 {
            FLAG_CARR
        }
        else {
            0b0000_0000
        };

        if (self.registers.f & FLAG_CARR) > 0 {
            rot = reg.rotate_left(bits as u32);
        }

        self.registers.f |= carry_bit;
        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;

        if rot == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }
        rot
    }

    pub fn op_rotate_right(&mut self, reg:u8, bits:u8) -> u8 {
        if (reg << 7) == 0 {
            self.registers.f &= !FLAG_CARR;
        }
        else {
            self.registers.f |= FLAG_CARR;
        }

        let rot = reg.rotate_right(bits as u32);

        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;

        if rot == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }
        rot
    }

    pub fn op_rotate_right_carry(&mut self, reg:u8, bits:u8) -> u8 {
        let mut rot = reg;
        let carry_bit = if (reg << 7) == 0 {
            FLAG_CARR
        }
        else {
            0b0000_0000
        };

        if (self.registers.f & FLAG_CARR) > 0 {
            rot = reg.rotate_right(bits as u32);
        }

        self.registers.f |= carry_bit;
        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;

        if rot == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }
        rot
    }

    pub fn op_shift_left_carry(&mut self, reg:u8, bits:u8) -> u8 {
        let mut shift = reg;
        let carry_bit = if (reg >> 7) == 0 {
            FLAG_CARR
        }
        else {
            0b0000_0000
        };

        if (self.registers.f & FLAG_CARR) > 0 {
            shift = reg << bits;
        }

        self.registers.f |= carry_bit;
        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;

        if shift == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }
        shift
    }

    pub fn op_shift_rightl_carry(&mut self, reg:u8, bits:u8) -> u8 {
        let mut shift = reg;
        let carry_bit = if (reg << 7) == 0 {
            FLAG_CARR
        }
        else {
            0b0000_0000
        };

        if (self.registers.f & FLAG_CARR) > 0 {
            shift = reg >> bits;
        }

        self.registers.f |= carry_bit;
        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;

        if shift == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }
        shift
    }

    pub fn op_shift_righta_carry(&mut self, reg:u8, bits:u8) -> u8 {
        let mut shift = reg;
        let carry_bit = if (reg << 7) == 0 {
            FLAG_CARR
        }
        else {
            0b0000_0000
        };

        if (self.registers.f & FLAG_CARR) > 0 {
            shift = (reg as i16 >> bits) as u8; // Arith RS only for signed values smdh rust
        }

        self.registers.f |= carry_bit;
        self.registers.f &= !FLAG_SUBT;
        self.registers.f &= !FLAG_HALF;

        if shift == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }
        shift
    }

    pub fn op_test_bit(&mut self, reg:u8, bit:u8) {
        let test = 0b0000_0001 << bit;

        if (reg & test) == 0 {
            self.registers.f |= FLAG_ZERO;
        }
        else {
            self.registers.f &= !FLAG_ZERO;
        }

        self.registers.f &= !FLAG_SUBT;
        self.registers.f |= FLAG_HALF;
    }

    pub fn op_bit(bit:u8) -> u8 {
        0b0000_0001 << bit
    }

    pub fn op_jump(&mut self, addr:u16) {
        self.registers.pc = addr;
    }

    pub fn op_jump_if(&mut self, addr:u16, cond:JumpCondition) -> bool {
        let mut jump = false;

        match cond {
            JumpCondition::NotZero => {
                if (self.registers.f & FLAG_ZERO) == 0 {
                    jump = true;
                }
            }
            JumpCondition::Zero => {
                if (self.registers.f & FLAG_ZERO) > 0 {
                    jump = true;
                }
            }
            JumpCondition::NotCarry => {
                if (self.registers.f & FLAG_CARR) == 0 {
                    jump = true;
                }
            }
            JumpCondition::Carry => {
                if (self.registers.f & FLAG_CARR) > 0 {
                    jump = true;
                }
            }
        }

        if jump {
            self.registers.pc = addr;
        }
        jump
    }

    pub fn op_jump_add(&mut self, offset:u8) {
        self.registers.pc = CPU::add_16bit_swap(self.registers.pc, (offset as u16) << 8);
    }

    pub fn op_jump_if_add(&mut self, offset:u8, cond:JumpCondition) -> bool {
        let mut jump = false;

        match cond {
            JumpCondition::NotZero => {
                if (self.registers.f & FLAG_ZERO) == 0 {
                    jump = true;
                }
            }
            JumpCondition::Zero => {
                if (self.registers.f & FLAG_ZERO) > 0 {
                    jump = true;
                }
            }
            JumpCondition::NotCarry => {
                if (self.registers.f & FLAG_CARR) == 0 {
                    jump = true;
                }
            }
            JumpCondition::Carry => {
                if (self.registers.f & FLAG_CARR) > 0 {
                    jump = true;
                }
            }
        }

        if jump {
            self.registers.pc = CPU::add_16bit_swap(self.registers.pc, (offset as u16) << 8);
        }
        jump
    }

    pub fn op_call(&mut self, timer:&mut Timer, memory:&mut MemMap) {
        self.registers.sp = CPU::dec_16bit(self.registers.sp);
        memory.set_memory(timer, self.registers.sp.swap_bytes() as usize, CPU::get_upper_byte(self.registers.pc));
        self.registers.sp = CPU::dec_16bit(self.registers.sp);
        memory.set_memory(timer, self.registers.sp.swap_bytes() as usize, self.registers.pc as u8);
    }

    pub fn op_call_if(&mut self, timer:&mut Timer, memory:&mut MemMap, cond:JumpCondition) -> bool {
        let mut call = false;

        match cond {
            JumpCondition::NotZero => {
                if (self.registers.f & FLAG_ZERO) == 0 {
                    call = true;
                }
            }
            JumpCondition::Zero => {
                if (self.registers.f & FLAG_ZERO) > 0 {
                    call = true;
                }
            }
            JumpCondition::NotCarry => {
                if (self.registers.f & FLAG_CARR) == 0 {
                    call = true;
                }
            }
            JumpCondition::Carry => {
                if (self.registers.f & FLAG_CARR) > 0 {
                    call = true;
                }
            }
        }

        if call {
            self.registers.sp = CPU::dec_16bit(self.registers.sp);
            memory.set_memory(timer, self.registers.sp.swap_bytes() as usize, CPU::get_upper_byte(self.registers.pc));
            self.registers.sp = CPU::dec_16bit(self.registers.sp);
            memory.set_memory(timer, self.registers.sp.swap_bytes() as usize, self.registers.pc as u8);
        }
        call
    }

    pub fn op_restart(&mut self, timer:&mut Timer, memory:&mut MemMap, offset:u8) {
        self.registers.sp = CPU::dec_16bit(self.registers.sp);
        memory.set_memory(timer, self.registers.sp.swap_bytes() as usize, CPU::get_upper_byte(self.registers.pc));
        self.registers.sp = CPU::dec_16bit(self.registers.sp);
        memory.set_memory(timer, self.registers.sp.swap_bytes() as usize, self.registers.pc as u8);
        self.registers.pc = (offset as u16) << 8;
    }

    pub fn op_return(&mut self, memory:&mut MemMap) {
        self.registers.pc = memory.get_word(self.registers.sp.swap_bytes() as usize);
        self.registers.sp = CPU::inc_16bit(self.registers.sp);
        self.registers.sp = CPU::inc_16bit(self.registers.sp);
    }

    pub fn op_return_if(&mut self, memory:&mut MemMap, cond:JumpCondition) -> bool {
        let mut ret = false;

        match cond {
            JumpCondition::NotZero => {
                if (self.registers.f & FLAG_ZERO) == 0 {
                    ret = true;
                }
            }
            JumpCondition::Zero => {
                if (self.registers.f & FLAG_ZERO) > 0 {
                    ret = true;
                }
            }
            JumpCondition::NotCarry => {
                if (self.registers.f & FLAG_CARR) == 0 {
                    ret = true;
                }
            }
            JumpCondition::Carry => {
                if (self.registers.f & FLAG_CARR) > 0 {
                    ret = true;
                }
            }
        }

        if ret {
            self.registers.pc = memory.get_word(self.registers.sp.swap_bytes() as usize);
            self.registers.sp = CPU::inc_16bit(self.registers.sp);
            self.registers.sp = CPU::inc_16bit(self.registers.sp);
        }
        ret
    }

    pub fn op_return_ei(&mut self, timer:&mut Timer, memory:&mut MemMap) {
        self.registers.pc = memory.get_word(self.registers.sp.swap_bytes() as usize);
        self.registers.sp = CPU::inc_16bit(self.registers.sp);
        self.registers.sp = CPU::inc_16bit(self.registers.sp);
        self.ime = true;
        memory.set_memory(timer, crate::memory::IE as usize, crate::memory::IE_ALL)
    }
}

//////////////////////
// Registers struct //
//////////////////////
pub struct Registers {
    pub a:u8,
    pub b:u8,
    pub c:u8,
    pub d:u8,
    pub e:u8,
    pub f:u8,
    pub h:u8,
    pub l:u8,
    pub sp:u16,
    pub pc:u16
}

pub enum RegistersEnum {
    a,
    b,
    c,
    d,
    e,
    f,
    h,
    l,
    sp,
    pc,
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
            pc:0x0001
        }
    }

    // Treat two 8 bit values as one 16 bit value
    pub fn concat_registers(most:u8, least:u8) -> u16 {
        ((most as u16) << 8) | least as u16
    }
}
