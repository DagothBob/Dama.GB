use std::mem::transmute;

use crate::{gb_cpu::LR35902, gamepak::GamePak, aux_ram::AuxRAM, MERGE_U8, SPLIT_U16};
use paste::paste;

pub struct GB {
    ops: [OpCode; 0x100],
    cb: [OpCode; 0x100],
    cpu: LR35902,
    game: GamePak,
    mem: AuxRAM
}

type OpCode = fn(&mut GB);

////////////////////////
// Instruction Macros //
////////////////////////

/// Load 16-bit literal into merged register.
/// 
/// eg. LD BC,$FFFF
macro_rules! LD_XX_D16 {
    ($reg:tt) => {
        paste! {
            fn [<ld_ $reg _d16>](&mut self) {
                let value: u16 = self.read_word_pc_advance();
                self.cpu.[<set_ $reg>](value);
            }
        }
    };
}

/// Increment merged register.
/// 
/// eg. INC BC
macro_rules! INC_XX {
    ($reg:tt) => {
        paste! {
            fn [<inc_ $reg>](&mut self) {
                let old: u16 = self.cpu.[<get_ $reg>]();
                self.cpu.[<set_ $reg>](old.wrapping_add(1));
            }
        }
    };
}

/// Increment register.
/// 
/// eg. INC B
macro_rules! INC_X {
    ($reg:tt) => {
        paste! {
            fn [<inc_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                self.cpu.[<set_ $reg>](old.wrapping_add(1));
                let new: u8 = self.cpu.[<get_ $reg>]();
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
        
                if new & 0xF == 0 {
                    self.cpu.set_half_flag();
                }
            }
        }
    };
}

/// Decrement register.
/// 
/// eg. DEC B
macro_rules! DEC_X {
    ($reg:tt) => {
        paste! {
            fn [<dec_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                self.cpu.[<set_ $reg>](old.wrapping_sub(1));
                let new: u8 = self.cpu.[<get_ $reg>]();
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.set_subt_flag();
        
                if new & 0xF == 0xF {
                    self.cpu.set_half_flag();
                }
            }
        }
    };
}

/// Add merged register to HL.
/// 
/// eg. ADD HL,BC
macro_rules! ADD_HL_XX {
    ($reg:tt) => {
        paste! {
            fn [<add_hl_ $reg>](&mut self) {
                let old: u16 = self.cpu.get_hl();
                let reg: u16 = self.cpu.[<get_ $reg>]();
                let new: u16 = old.wrapping_add(reg);
        
                self.cpu.set_hl(new);
                self.cpu.reset_subt_flag();
        
                // Half-carry is stupid. I stole this code unashamedly.
                if ((old & 0xFFF).wrapping_add(reg & 0xFFF)) & 0x1000 != 0 {
                    self.cpu.set_half_flag();
                }
        
                if new < old {
                    self.cpu.set_carr_flag();
                }
            }
        }
    };
}

/// Load memory @ merged register into register.
/// 
/// eg. LD B,(HL)
macro_rules! LD_X_YYM {
    ($into:tt, $from:tt) => {
        paste! {
            fn [<ld_ $into _ $from m>](&mut self) {
                let addr: u16 = self.cpu.[<get_ $from>]();
                let value: u8 = self.read_cycle(addr);
                self.cpu.[<set_ $into>](value);
            }
        }
    };
}

/// Decrement merged register.
/// 
/// eg. DEC BC
macro_rules! DEC_XX {
    ($reg:tt) => {
        paste! {
            fn [<dec_ $reg>](&mut self) {
                let old: u16 = self.cpu.[<get_ $reg>]();
                self.cpu.[<set_ $reg>](old.wrapping_sub(1));
            }
        }
    };
}

/// Increment/decrement HL after loading A into its
/// stored memory address.
/// 
/// eg. LD (HL+),A
macro_rules! LD_HLX_A {
    ($sign:tt) => {
        paste! {
            fn [<ld_hl $sign _a>](&mut self) {
                let val: u8 = self.cpu.get_a();
                let addr: u16 = self.cpu.get_hl();
        
                self.write_cycle(addr, val);
                self.cpu.set_hl(addr.[<wrapping_ $sign>](1));
            }
        }
    };
}

/// Increment/decrement HL after loading its
/// stored memory address into A.
/// 
/// eg. LD A,(HL+)
macro_rules! LD_A_HLX {
    ($sign:tt) => {
        paste! {
            fn [<ld_a_hl $sign>](&mut self) {
                let addr: u16 = self.cpu.get_hl();
                let val: u8 = self.read_cycle(addr);
        
                self.cpu.set_a(val);
                self.cpu.set_hl(addr.[<wrapping_ $sign>](1));
            }
        }
    };
}

/// Load register into memory @ merged register.
/// 
/// eg. LD (BC),A
macro_rules! LD_XXM_Y {
    ($into:tt, $from:tt) => {
        paste! {
            fn [<ld_ $into m_ $from>](&mut self) {
                let addr: u16 = self.cpu.[<get_ $into>]();
                self.write_cycle(addr, self.cpu.[<get_ $from>]());
            }
        }
    };
}

/// Load 8-bit literal into register.
/// 
/// eg. LD B,$FF
macro_rules! LD_X_D8 {
    ($reg:tt) => {
        paste! {
            fn [<ld_ $reg _d8>](&mut self) {
                let val: u8 = self.read_pc_advance();
                self.cpu.[<set_ $reg>](val);
            }
        }
    };
}

/// Jump to byte offset of PC if flag reset.
/// 
/// eg. JR NZ,r8
macro_rules! JR_NX_R8 {
    ($flag:tt) => {
        paste! {
            fn [<jr_n $flag _r8>](&mut self) {
                if self.cpu.[<get_ $flag _flag>]() == 0 {
                    let pc: u16 = self.cpu.get_pc();
                    let offset: u8 = self.read_pc_advance();
                    self.cpu.set_pc(pc.wrapping_add(offset.into()));
                }
            }
        }
    };
}

/// Jump to byte offset of PC if flag set.
/// 
/// eg. JR Z,r8
macro_rules! JR_X_R8 {
    ($flag:tt) => {
        paste! {
            fn [<jr_ $flag _r8>](&mut self) {
                if self.cpu.[<get_ $flag _flag>]() != 0 {
                    let pc: u16 = self.cpu.get_pc();
                    let offset: u8 = self.read_pc_advance();
                    self.cpu.set_pc(pc.wrapping_add(offset.into()));
                }
            }
        }
    };
}

/// Load register into register.
/// 
/// eg. LD B,C
macro_rules! LD_X_Y {
    ($into:tt, $from:tt) => {
        paste! {
            pub fn [<ld_ $into _ $from>](&mut self) {
                self.cpu.[<set_ $into>](self.cpu.[<get_ $from>]());
            }
        }
    };
}

/// Add register to A.
/// 
/// eg. ADD A,B
macro_rules! ADD_A_X {
    ($reg:tt) => {
        paste! {
            fn [<add_a_ $reg>](&mut self) {
                let old: u8 = self.cpu.get_a();
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let new: u8 = old.wrapping_add(reg);
                self.cpu.set_a(new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
        
                if (old & 0xF).wrapping_add(reg & 0xF) > 0x0F {
                    self.cpu.set_half_flag();
                }
        
                if new < old {
                    self.cpu.set_carr_flag();
                }
            }
        }
    };
}

/// Add register to A through carry.
/// 
/// eg. ADC A,B
macro_rules! ADC_A_X {
    ($reg:tt) => {
        paste! {
            fn [<adc_a_ $reg>](&mut self) {
                let old: u8 = self.cpu.get_a();
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let carry: u8 = self.cpu.get_carr_flag();
                let new: u8 = old.wrapping_add(reg).wrapping_add(carry);
                self.cpu.set_a(new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
        
                if (old & 0xF).wrapping_add(reg & 0xF).wrapping_add(carry) > 0x0F {
                    self.cpu.set_half_flag();
                }
        
                if new < old {
                    self.cpu.set_carr_flag();
                }
            }
        }
    };
}

/// Subtract register from A.
/// 
/// eg. SUB A,B
macro_rules! SUB_A_X {
    ($reg:tt) => {
        paste! {
            fn [<sub_a_ $reg>](&mut self) {
                let old: u8 = self.cpu.get_a();
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let new: u8 = old.wrapping_sub(reg);
                self.cpu.set_a(new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.set_subt_flag();
        
                if (old & 0xF) < (reg & 0xF) {
                    self.cpu.set_half_flag();
                }
        
                if new > old {
                    self.cpu.set_carr_flag();
                }
            }
        }
    };
}

/// Subtract register from A through carry.
/// 
/// eg. SBC A,B
macro_rules! SBC_A_X {
    ($reg:tt) => {
        paste! {
            fn [<sbc_a_ $reg>](&mut self) {
                let old: u8 = self.cpu.get_a();
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let carry: u8 = self.cpu.get_carr_flag();
                let new: u8 = old.wrapping_sub(reg).wrapping_sub(carry);
                self.cpu.set_a(new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.set_subt_flag();
        
                if (old & 0xF) < (reg & 0xF).wrapping_add(carry) {
                    self.cpu.set_half_flag();
                }
        
                if new > old {
                    self.cpu.set_carr_flag();
                }
            }
        }
    };
}

/// "AND" register with A.
/// 
/// eg. AND B
macro_rules! AND_A_X {
    ($reg:tt) => {
        paste! {
            fn [<and_a_ $reg>](&mut self) {
                let old: u8 = self.cpu.get_a();
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let new: u8 = old & reg;
                self.cpu.set_a(new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.set_half_flag();
                self.cpu.reset_carr_flag();
            }
        }
    };
}

/// "XOR" register with A.
/// 
/// eg. XOR B
macro_rules! XOR_A_X {
    ($reg:tt) => {
        paste! {
            fn [<xor_a_ $reg>](&mut self) {
                let old: u8 = self.cpu.get_a();
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let new: u8 = old ^ reg;
                self.cpu.set_a(new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
                self.cpu.reset_carr_flag();
            }
        }
    };
}

/// "OR" register with A.
/// 
/// eg. OR B
macro_rules! OR_A_X {
    ($reg:tt) => {
        paste! {
            fn [<or_a_ $reg>](&mut self) {
                let old: u8 = self.cpu.get_a();
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let new: u8 = old | reg;
                self.cpu.set_a(new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
                self.cpu.reset_carr_flag();
            }
        }
    };
}

/// Compare register with A, setting various flags.
/// 
/// eg. CP B
macro_rules! CP_A_X {
    ($reg:tt) => {
        paste! {
            fn [<cp_a_ $reg>](&mut self) {
                let old: u8 = self.cpu.get_a();
                let reg: u8 = self.cpu.[<get_ $reg>]();
        
                if old == reg {
                    self.cpu.set_zero_flag();
                }
                else {
                    self.cpu.reset_zero_flag();
                }
        
                self.cpu.set_subt_flag();
        
                if (old & 0xF) < (reg & 0xF) {
                    self.cpu.set_half_flag();
                }
                else {
                    self.cpu.reset_half_flag();
                }
        
                if old < reg {
                    self.cpu.set_carr_flag();
                }
                else {
                    self.cpu.reset_carr_flag();
                }
            }
        }
    };
}

/// Return if flag is reset.
/// 
/// eg. RET NZ
macro_rules! RET_NX {
    ($flag:tt) => {
        paste! {
            fn [<ret_n $flag>](&mut self) {
                self.advance_cycle();
        
                if self.cpu.[<get_ $flag _flag>]() == 0 {
                    self.advance_cycle();
        
                    let mut sp: u16 = self.cpu.get_sp();
                    let lo: u8 = self.read_cycle(sp);
                    self.cpu.increment_sp();
                    
                    sp = self.cpu.get_sp();
                    let hi: u8 = self.read_cycle(sp);
                    self.cpu.increment_sp();
        
                    self.cpu.set_pc(MERGE_U8!(hi, lo));
                }
            }
        }
    };
}

/// Pop word from stack into merged register.
/// 
/// eg. POP BC
macro_rules! POP_XX {
    ($reg:tt) => {
        paste! {
            fn [<pop_ $reg>](&mut self) {
                let mut sp: u16 = self.cpu.get_sp();
                let lo: u8 = self.read_cycle(sp);
                self.cpu.increment_sp();
                
                sp = self.cpu.get_sp();
                let hi: u8 = self.read_cycle(sp);
                self.cpu.increment_sp();
        
                self.cpu.[<set_ $reg>](MERGE_U8!(hi, lo));
            }
        }
    };
}

/// Jump to address literal if flag is reset.
/// 
/// eg. JP NZ,a16
macro_rules! JP_NX_A16 {
    ($flag:tt) => {
        paste! {
            fn [<jp_n $flag _a16>](&mut self) {
                let addr: u16 = self.read_word_pc_advance();
        
                if self.cpu.[<get_ $flag _flag>]() == 0 {
                    self.advance_cycle();
        
                    self.cpu.set_pc(addr);
                }
            }
        }
    };
}

/// Call if flag is reset.
/// 
/// eg. CALL NZ,a16
macro_rules! CALL_NX_A16 {
    ($flag:tt) => {
        paste! {
            fn [<call_n $flag _a16>](&mut self) {
                let new_lo: u8 = self.read_pc_advance();
                let new_hi: u8 = self.read_pc_advance();
        
                if self.cpu.[<get_ $flag _flag>]() == 0 {
                    let pc: u16 = self.cpu.get_pc();
                    let old_lo: u8 = SPLIT_U16!(pc)[1];
                    let old_hi: u8 = SPLIT_U16!(pc)[0];

                    self.advance_cycle();
        
                    self.cpu.decrement_sp();
                    let mut sp: u16 = self.cpu.get_sp();
                    self.write_cycle(sp, old_hi);
        
                    self.cpu.decrement_sp();
                    sp = self.cpu.get_sp();
                    self.write_cycle(sp, old_lo);

                    self.cpu.set_pc(MERGE_U8!(new_hi, new_lo));
                }
            }
        }
    };
}

/// Push word onto stack from merged register.
/// 
/// eg. PUSH BC
macro_rules! PUSH_XX {
    ($hi:tt, $lo:tt) => {
        paste! {
            fn [<push_ $hi $lo>](&mut self) {
                self.advance_cycle();
        
                self.cpu.decrement_sp();
                let mut sp: u16 = self.cpu.get_sp();
                let hi: u8 = self.cpu.[<get_ $hi>]();
                self.write_cycle(sp, hi);
                
                self.cpu.decrement_sp();
                sp = self.cpu.get_sp();
                let lo: u8 = self.cpu.[<get_ $lo>]();
                self.write_cycle(sp, lo);        
            }
        }
    };
}

/// Push PC to stack, then jump to $0000 + offset.
/// 
/// eg. RST 00H
macro_rules! RST_XXH {
    ($offset:tt) => {
        paste! {
            fn [<rst_ $offset h>](&mut self) {
                self.advance_cycle();
        
                let pc: [u8; 2] = SPLIT_U16!(self.cpu.get_pc());
        
                self.cpu.decrement_sp();
                let mut sp: u16 = self.cpu.get_sp();
                self.write_cycle(sp, pc[0]);
        
                self.cpu.decrement_sp();
                sp = self.cpu.get_sp();
                self.write_cycle(sp, pc[1]);
        
                self.cpu.set_pc([<$offset>]);
            }
        }
    };
}

/// Return if flag is set.
/// 
/// eg. RET NZ
macro_rules! RET_X {
    ($flag:tt) => {
        paste! {
            fn [<ret_ $flag>](&mut self) {
                self.advance_cycle();
        
                if self.cpu.[<get_ $flag _flag>]() != 0 {
                    self.advance_cycle();
        
                    let mut sp: u16 = self.cpu.get_sp();
                    let lo: u8 = self.read_cycle(sp);
                    self.cpu.increment_sp();
                    
                    sp = self.cpu.get_sp();
                    let hi: u8 = self.read_cycle(sp);
                    self.cpu.increment_sp();
        
                    self.cpu.set_pc(MERGE_U8!(hi, lo));
                }
            }
        }
    };
}

/// Jump to address literal if flag is set.
/// 
/// eg. JP Z,a16
macro_rules! JP_X_A16 {
    ($flag:tt) => {
        paste! {
            fn [<jp_ $flag _a16>](&mut self) {
                let addr: u16 = self.read_word_pc_advance();
        
                if self.cpu.[<get_ $flag _flag>]() != 0 {
                    self.advance_cycle();
        
                    self.cpu.set_pc(addr);
                }
            }
        }
    };
}

/// Call if flag is set.
/// 
/// eg. CALL Z,a16
macro_rules! CALL_X_A16 {
    ($flag:tt) => {
        paste! {
            fn [<call_ $flag _a16>](&mut self) {
                let new_lo: u8 = self.read_pc_advance();
                let new_hi: u8 = self.read_pc_advance();
        
                if self.cpu.[<get_ $flag _flag>]() != 0 {
                    let pc: u16 = self.cpu.get_pc();
                    let old_lo: u8 = SPLIT_U16!(pc)[1];
                    let old_hi: u8 = SPLIT_U16!(pc)[0];

                    self.advance_cycle();
        
                    self.cpu.decrement_sp();
                    let mut sp: u16 = self.cpu.get_sp();
                    self.write_cycle(sp, old_hi);
        
                    self.cpu.decrement_sp();
                    sp = self.cpu.get_sp();
                    self.write_cycle(sp, old_lo);

                    self.cpu.set_pc(MERGE_U8!(new_hi, new_lo));
                }
            }
        }
    };
}

/// Rotate register left through carry.
/// 
/// eg. RLC B
macro_rules! RLC_X {
    ($reg:tt) => {
        paste! {
            fn [<rlc_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                let new: u8 = old.rotate_left(1);
        
                self.cpu.[<set_ $reg>](new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
        
                if (old & 0b1000_0000) != 0 {
                    self.cpu.set_carr_flag();
                }
                else {
                    self.cpu.reset_carr_flag();
                }
            }
        }
    };
}

/// Rotate register left through carry.
/// 
/// eg. RLC B
macro_rules! RRC_X {
    ($reg:tt) => {
        paste! {
            fn [<rrc_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                let new: u8 = old.rotate_right(1);
        
                self.cpu.[<set_ $reg>](new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
        
                if (old & 0b0000_0001) != 0 {
                    self.cpu.set_carr_flag();
                }
                else {
                    self.cpu.reset_carr_flag();
                }
            }
        }
    };
}

/// Rotate register left.
/// 
/// eg. RL B
macro_rules! RL_X {
    ($reg:tt) => {
        paste! {
            fn [<rl_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                let carry: u8 = if self.cpu.get_carr_flag() != 0 { 0b0000_0001 } else { 0b0000_0000 };
                let new: u8 = (old << 1) | carry;
        
                self.cpu.[<set_ $reg>](new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
        
                if (old & 0b1000_0000) != 0 {
                    self.cpu.set_carr_flag();
                }
                else {
                    self.cpu.reset_carr_flag();
                }
            }
        }
    };
}

/// Rotate register left.
/// 
/// eg. RL B
macro_rules! RR_X {
    ($reg:tt) => {
        paste! {
            fn [<rr_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                let carry: u8 = if self.cpu.get_carr_flag() != 0 { 0b1000_0000 } else { 0b0000_0000 };
                let new: u8 = (old >> 1) | carry;
        
                self.cpu.[<set_ $reg>](new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
        
                if (old & 0b0000_0001) != 0 {
                    self.cpu.set_carr_flag();
                }
                else {
                    self.cpu.reset_carr_flag();
                }
            }
        }
    };
}

/// Shift register left into carry.
/// 
/// eg. SLA B
macro_rules! SLA_X {
    ($reg:tt) => {
        paste! {
            fn [<sla_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                let new: u8 = old << 1;
        
                self.cpu.[<set_ $reg>](new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
        
                if (old & 0b1000_0000) != 0 {
                    self.cpu.set_carr_flag();
                }
                else {
                    self.cpu.reset_carr_flag();
                }
            }
        }
    };
}

/// Shift register right (arithmetic) into carry.
/// 
/// eg. SRA B
macro_rules! SRA_X {
    ($reg:tt) => {
        paste! {
            fn [<sra_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                let sign: u8 = if old & 0b1000_0000 != 0 { 0b1000_0000 } else { 0b0000_0000 };
                let new: u8 = (old >> 1) | sign;
        
                self.cpu.[<set_ $reg>](new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
        
                if (old & 0b0000_0001) != 0 {
                    self.cpu.set_carr_flag();
                }
                else {
                    self.cpu.reset_carr_flag();
                }
            }
        }
    };
}

/// Swap upper and lower nibbles of register.
/// 
/// eg. SWAP B
macro_rules! SWAP_X {
    ($reg:tt) => {
        paste! {
            fn [<swap_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                let lo: u8 = (old & 0b1111_0000) >> 4;
                let hi: u8 = (old & 0b0000_1111) << 4;
                let new: u8 = hi | lo;
        
                self.cpu.[<set_ $reg>](new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
                self.cpu.reset_carr_flag();
            }
        }
    };
}

/// Shift register right (logical) into carry.
/// 
/// eg. SRL B
macro_rules! SRL_X {
    ($reg:tt) => {
        paste! {
            fn [<srl_ $reg>](&mut self) {
                let old: u8 = self.cpu.[<get_ $reg>]();
                let new: u8 = (old >> 1) & 0b0111_1111;
        
                self.cpu.[<set_ $reg>](new);
        
                if new == 0 {
                    self.cpu.set_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.reset_half_flag();
        
                if (old & 0b0000_0001) != 0 {
                    self.cpu.set_carr_flag();
                }
                else {
                    self.cpu.reset_carr_flag();
                }
            }
        }
    };
}

/// Test bit in register.
/// 
/// eg. BIT 0,B
macro_rules! BIT_B_X {
    ($bit:tt, $reg:tt) => {
        paste! {
            fn [<bit_ $bit _ $reg>](&mut self) {
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let bit: u8 = 0b0000_0001 << [<$bit>];
        
                if reg & bit == 0 {
                    self.cpu.set_zero_flag();
                }
                else {
                    self.cpu.reset_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.set_half_flag();
            }
        }
    };
}

/// Reset bit in register.
/// 
/// eg. RES 0,B
macro_rules! RES_B_X {
    ($bit:tt, $reg:tt) => {
        paste! {
            fn [<res_ $bit _ $reg>](&mut self) {
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let bit: u8 = 0b0000_0001 << [<$bit>];
        
                self.cpu.[<set_ $reg>](reg & !bit);
            }
        }
    };
}

/// Set bit in register.
/// 
/// eg. SET 0,B
macro_rules! SET_B_X {
    ($bit:tt, $reg:tt) => {
        paste! {
            fn [<set_ $bit _ $reg>](&mut self) {
                let reg: u8 = self.cpu.[<get_ $reg>]();
                let bit: u8 = 0b0000_0001 << [<$bit>];
        
                self.cpu.[<set_ $reg>](reg | bit);
            }
        }
    };
}

/// Test bit in value at memory address in HL.
/// 
/// eg. BIT 0,(HL)
macro_rules! BIT_B_HLM {
    ($bit:tt) => {
        paste! {
            fn [<bit_ $bit _hlm>](&mut self) {
                let addr: u16 = self.cpu.get_hl();
                let reg: u8 = self.read_cycle(addr);
                let bit: u8 = 0b0000_0001 << [<$bit>];
        
                if reg & bit == 0 {
                    self.cpu.set_zero_flag();
                }
                else {
                    self.cpu.reset_zero_flag();
                }
        
                self.cpu.reset_subt_flag();
                self.cpu.set_half_flag();
            }
        }
    };
}

/// Reset bit in value at memory address in HL.
/// 
/// eg. RES 0,(HL)
macro_rules! RES_B_HLM {
    ($bit:tt) => {
        paste! {
            fn [<res_ $bit _hlm>](&mut self) {
                let addr: u16 = self.cpu.get_hl();
                let reg: u8 = self.read_cycle(addr);
                let bit: u8 = 0b0000_0001 << [<$bit>];
        
                self.write_cycle(addr, reg & !bit);
            }
        }
    };
}

/// Set bit in value at memory address in HL.
/// 
/// eg. SET 0,(HL)
macro_rules! SET_B_HLM {
    ($bit:tt) => {
        paste! {
            fn [<set_ $bit _hlm>](&mut self) {
                let addr: u16 = self.cpu.get_hl();
                let reg: u8 = self.read_cycle(addr);
                let bit: u8 = 0b0000_0001 << [<$bit>];
        
                self.write_cycle(addr, reg | bit);
            }
        }
    };
}

impl GB {
    /// Initializer
    pub fn init() -> Self {
        GB {
            ops:[// 0x0                 0x1                 0x2                 0x3                 0x4                 0x5                 0x6                 0x7                 0x8                 0x9                 0xA                 0xB                 0xC                 0xD                 0xE                 0xF
        /* 0x0 */   GB::nop,            GB::ld_bc_d16,      GB::ld_bcm_a,       GB::inc_bc,         GB::inc_b,          GB::dec_b,          GB::ld_b_d8,        GB::rlca,           GB::ld_a16_sp,      GB::add_hl_bc,      GB::ld_a_bcm,       GB::dec_bc,         GB::inc_c,          GB::dec_c,          GB::ld_c_d8,        GB::rrca,          /* 0x0 */
        /* 0x1 */   GB::stop,           GB::ld_de_d16,      GB::ld_dem_a,       GB::inc_de,         GB::inc_d,          GB::dec_d,          GB::ld_d_d8,        GB::rla,            GB::jr_r8,          GB::add_hl_de,      GB::ld_a_dem,       GB::dec_de,         GB::inc_e,          GB::dec_e,          GB::ld_e_d8,        GB::rra,           /* 0x1 */
        /* 0x2 */   GB::jr_nzero_r8,    GB::ld_hl_d16,      GB::ld_hladd_a,     GB::inc_hl,         GB::inc_h,          GB::dec_h,          GB::ld_h_d8,        GB::daa,            GB::jr_zero_r8,     GB::add_hl_hl,      GB::ld_a_hladd,     GB::dec_hl,         GB::inc_l,          GB::dec_l,          GB::ld_l_d8,        GB::cpl,           /* 0x2 */
        /* 0x3 */   GB::jr_ncarr_r8,    GB::ld_sp_d16,      GB::ld_hlsub_a,     GB::inc_sp,         GB::inc_mhl,        GB::dec_mhl,        GB::ld_mhl_d8,      GB::scf,            GB::jr_carr_r8,     GB::add_hl_sp,      GB::ld_a_hlsub,     GB::dec_sp,         GB::inc_a,          GB::dec_a,          GB::ld_a_d8,        GB::ccf,           /* 0x3 */
        /* 0x4 */   GB::ld_b_b,         GB::ld_b_c,         GB::ld_b_d,         GB::ld_b_e,         GB::ld_b_h,         GB::ld_b_l,         GB::ld_b_hlm,       GB::ld_b_a,         GB::ld_c_b,         GB::ld_c_c,         GB::ld_c_d,         GB::ld_c_e,         GB::ld_c_h,         GB::ld_c_l,         GB::ld_c_hlm,       GB::ld_c_a,        /* 0x4 */
        /* 0x5 */   GB::ld_d_b,         GB::ld_d_c,         GB::ld_d_d,         GB::ld_d_e,         GB::ld_d_h,         GB::ld_d_l,         GB::ld_d_hlm,       GB::ld_d_a,         GB::ld_e_b,         GB::ld_e_c,         GB::ld_e_d,         GB::ld_e_e,         GB::ld_e_h,         GB::ld_e_l,         GB::ld_e_hlm,       GB::ld_e_a,        /* 0x5 */
        /* 0x6 */   GB::ld_h_b,         GB::ld_h_c,         GB::ld_h_d,         GB::ld_h_e,         GB::ld_h_h,         GB::ld_h_l,         GB::ld_h_hlm,       GB::ld_h_a,         GB::ld_l_b,         GB::ld_l_c,         GB::ld_l_d,         GB::ld_l_e,         GB::ld_l_h,         GB::ld_l_l,         GB::ld_l_hlm,       GB::ld_l_a,        /* 0x6 */
        /* 0x7 */   GB::ld_hlm_b,       GB::ld_hlm_c,       GB::ld_hlm_d,       GB::ld_hlm_e,       GB::ld_hlm_h,       GB::ld_hlm_l,       GB::halt,           GB::ld_hlm_a,       GB::ld_a_b,         GB::ld_a_c,         GB::ld_a_d,         GB::ld_a_e,         GB::ld_a_h,         GB::ld_a_l,         GB::ld_a_hlm,       GB::ld_a_a,        /* 0x7 */
        /* 0x8 */   GB::add_a_b,        GB::add_a_c,        GB::add_a_d,        GB::add_a_e,        GB::add_a_h,        GB::add_a_l,        GB::add_a_mhl,      GB::add_a_a,        GB::adc_a_b,        GB::adc_a_c,        GB::adc_a_d,        GB::adc_a_e,        GB::adc_a_h,        GB::adc_a_l,        GB::adc_a_mhl,      GB::adc_a_a,       /* 0x8 */
        /* 0x9 */   GB::sub_a_b,        GB::sub_a_c,        GB::sub_a_d,        GB::sub_a_e,        GB::sub_a_h,        GB::sub_a_l,        GB::sub_a_mhl,      GB::sub_a_a,        GB::sbc_a_b,        GB::sbc_a_c,        GB::sbc_a_d,        GB::sbc_a_e,        GB::sbc_a_h,        GB::sbc_a_l,        GB::sbc_a_mhl,      GB::sbc_a_a,       /* 0x9 */
        /* 0xA */   GB::and_a_b,        GB::and_a_c,        GB::and_a_d,        GB::and_a_e,        GB::and_a_h,        GB::and_a_l,        GB::and_a_mhl,      GB::and_a_a,        GB::xor_a_b,        GB::xor_a_c,        GB::xor_a_d,        GB::xor_a_e,        GB::xor_a_h,        GB::xor_a_l,        GB::xor_a_mhl,      GB::xor_a_a,       /* 0xA */
        /* 0xB */   GB::or_a_b,         GB::or_a_c,         GB::or_a_d,         GB::or_a_e,         GB::or_a_h,         GB::or_a_l,         GB::or_a_mhl,       GB::or_a_a,         GB::cp_a_b,         GB::cp_a_c,         GB::cp_a_d,         GB::cp_a_e,         GB::cp_a_h,         GB::cp_a_l,         GB::cp_a_mhl,       GB::cp_a_a,        /* 0xB */
        /* 0xC */   GB::ret_nzero,      GB::pop_bc,         GB::jp_nzero_a16,   GB::jp_a16,         GB::call_nzero_a16, GB::push_bc,        GB::add_a_d8,       GB::rst_00h,        GB::ret_zero,       GB::ret,            GB::jp_zero_a16,    GB::prefix_cb,      GB::call_zero_a16,  GB::call,           GB::adc_a_d8,       GB::rst_08h,       /* 0xC */
        /* 0xD */   GB::ret_ncarr,      GB::pop_de,         GB::jp_ncarr_a16,   GB::illegal,        GB::call_ncarr_a16, GB::push_de,        GB::sub_a_d8,       GB::rst_10h,        GB::ret_carr,       GB::reti,           GB::jp_carr_a16,    GB::illegal,        GB::call_carr_a16,  GB::illegal,        GB::sbc_a_d8,       GB::rst_18h,       /* 0xD */
        /* 0xE */   GB::ldh_a8_a,       GB::pop_hl,         GB::ld_cm_a,        GB::illegal,        GB::illegal,        GB::push_hl,        GB::and_a_d8,       GB::rst_20h,        GB::add_sp_r8,      GB::jp_hlm,         GB::ld_a16_a,       GB::illegal,        GB::illegal,        GB::illegal,        GB::xor_a_d8,       GB::rst_28h,       /* 0xE */
        /* 0xF */   GB::ldh_a_a8,       GB::pop_af,         GB::ld_a_cm,        GB::di,             GB::illegal,        GB::push_af,        GB::or_a_d8,        GB::rst_30h,        GB::ld_hl_sp_r8,    GB::ld_sp_hl,       GB::ld_a_a16,       GB::ei,             GB::illegal,        GB::illegal,        GB::cp_a_d8,        GB::rst_38h,       /* 0xF */
                ],
            cb:[ // 0x0                 0x1                 0x2                 0x3                 0x4                 0x5                 0x6                 0x7                 0x8                 0x9                 0xA                 0xB                 0xC                 0xD                 0xE                 0xF
        /* 0x0 */   GB::rlc_b,          GB::rlc_c,          GB::rlc_d,          GB::rlc_e,          GB::rlc_h,          GB::rlc_l,          GB::rlc_hlm,        GB::rlc_a,          GB::rrc_b,          GB::rrc_c,          GB::rrc_d,          GB::rrc_e,          GB::rrc_h,          GB::rrc_l,          GB::rrc_hlm,        GB::rrc_a,         /* 0x0 */
        /* 0x1 */   GB::rl_b,           GB::rl_c,           GB::rl_d,           GB::rl_e,           GB::rl_h,           GB::rl_l,           GB::rl_hlm,         GB::rl_a,           GB::rr_b,           GB::rr_c,           GB::rr_d,           GB::rr_e,           GB::rr_h,           GB::rr_l,           GB::rr_hlm,         GB::rr_a,          /* 0x1 */
        /* 0x2 */   GB::sla_b,          GB::sla_c,          GB::sla_d,          GB::sla_e,          GB::sla_h,          GB::sla_l,          GB::sla_hlm,        GB::sla_a,          GB::sra_b,          GB::sra_c,          GB::sra_d,          GB::sra_e,          GB::sra_h,          GB::sra_l,          GB::sra_hlm,        GB::sra_a,         /* 0x2 */
        /* 0x3 */   GB::swap_b,         GB::swap_c,         GB::swap_d,         GB::swap_e,         GB::swap_h,         GB::swap_l,         GB::swap_hlm,       GB::swap_a,         GB::srl_b,          GB::srl_c,          GB::srl_d,          GB::srl_e,          GB::srl_h,          GB::srl_l,          GB::srl_hlm,        GB::srl_a,         /* 0x3 */
        /* 0x4 */   GB::bit_0_b,        GB::bit_0_c,        GB::bit_0_d,        GB::bit_0_e,        GB::bit_0_h,        GB::bit_0_l,        GB::bit_0_hlm,      GB::bit_0_a,        GB::bit_1_b,        GB::bit_1_c,        GB::bit_1_d,        GB::bit_1_e,        GB::bit_1_h,        GB::bit_1_l,        GB::bit_1_hlm,      GB::bit_1_a,       /* 0x4 */
        /* 0x5 */   GB::bit_2_b,        GB::bit_2_c,        GB::bit_2_d,        GB::bit_2_e,        GB::bit_2_h,        GB::bit_2_l,        GB::bit_2_hlm,      GB::bit_2_a,        GB::bit_3_b,        GB::bit_3_c,        GB::bit_3_d,        GB::bit_3_e,        GB::bit_3_h,        GB::bit_3_l,        GB::bit_3_hlm,      GB::bit_3_a,       /* 0x5 */
        /* 0x6 */   GB::bit_4_b,        GB::bit_4_c,        GB::bit_4_d,        GB::bit_4_e,        GB::bit_4_h,        GB::bit_4_l,        GB::bit_4_hlm,      GB::bit_4_a,        GB::bit_5_b,        GB::bit_5_c,        GB::bit_5_d,        GB::bit_5_e,        GB::bit_5_h,        GB::bit_5_l,        GB::bit_5_hlm,      GB::bit_5_a,       /* 0x6 */
        /* 0x7 */   GB::bit_6_b,        GB::bit_6_c,        GB::bit_6_d,        GB::bit_6_e,        GB::bit_6_h,        GB::bit_6_l,        GB::bit_6_hlm,      GB::bit_6_a,        GB::bit_7_b,        GB::bit_7_c,        GB::bit_7_d,        GB::bit_7_e,        GB::bit_7_h,        GB::bit_7_l,        GB::bit_7_hlm,      GB::bit_7_a,       /* 0x7 */
        /* 0x8 */   GB::res_0_b,        GB::res_0_c,        GB::res_0_d,        GB::res_0_e,        GB::res_0_h,        GB::res_0_l,        GB::res_0_hlm,      GB::res_0_a,        GB::res_1_b,        GB::res_1_c,        GB::res_1_d,        GB::res_1_e,        GB::res_1_h,        GB::res_1_l,        GB::res_1_hlm,      GB::res_1_a,       /* 0x8 */
        /* 0x9 */   GB::res_2_b,        GB::res_2_c,        GB::res_2_d,        GB::res_2_e,        GB::res_2_h,        GB::res_2_l,        GB::res_2_hlm,      GB::res_2_a,        GB::res_3_b,        GB::res_3_c,        GB::res_3_d,        GB::res_3_e,        GB::res_3_h,        GB::res_3_l,        GB::res_3_hlm,      GB::res_3_a,       /* 0x9 */
        /* 0xA */   GB::res_4_b,        GB::res_4_c,        GB::res_4_d,        GB::res_4_e,        GB::res_4_h,        GB::res_4_l,        GB::res_4_hlm,      GB::res_4_a,        GB::res_5_b,        GB::res_5_c,        GB::res_5_d,        GB::res_5_e,        GB::res_5_h,        GB::res_5_l,        GB::res_5_hlm,      GB::res_5_a,       /* 0xA */
        /* 0xB */   GB::res_6_b,        GB::res_6_c,        GB::res_6_d,        GB::res_6_e,        GB::res_6_h,        GB::res_6_l,        GB::res_6_hlm,      GB::res_6_a,        GB::res_7_b,        GB::res_7_c,        GB::res_7_d,        GB::res_7_e,        GB::res_7_h,        GB::res_7_l,        GB::res_7_hlm,      GB::res_7_a,       /* 0xB */
        /* 0xC */   GB::set_0_b,        GB::set_0_c,        GB::set_0_d,        GB::set_0_e,        GB::set_0_h,        GB::set_0_l,        GB::set_0_hlm,      GB::set_0_a,        GB::set_1_b,        GB::set_1_c,        GB::set_1_d,        GB::set_1_e,        GB::set_1_h,        GB::set_1_l,        GB::set_1_hlm,      GB::set_1_a,       /* 0xC */
        /* 0xD */   GB::set_2_b,        GB::set_2_c,        GB::set_2_d,        GB::set_2_e,        GB::set_2_h,        GB::set_2_l,        GB::set_2_hlm,      GB::set_2_a,        GB::set_3_b,        GB::set_3_c,        GB::set_3_d,        GB::set_3_e,        GB::set_3_h,        GB::set_3_l,        GB::set_3_hlm,      GB::set_3_a,       /* 0xD */
        /* 0xE */   GB::set_4_b,        GB::set_4_c,        GB::set_4_d,        GB::set_4_e,        GB::set_4_h,        GB::set_4_l,        GB::set_4_hlm,      GB::set_4_a,        GB::set_5_b,        GB::set_5_c,        GB::set_5_d,        GB::set_5_e,        GB::set_5_h,        GB::set_5_l,        GB::set_5_hlm,      GB::set_5_a,       /* 0xE */
        /* 0xF */   GB::set_6_b,        GB::set_6_c,        GB::set_6_d,        GB::set_6_e,        GB::set_6_h,        GB::set_6_l,        GB::set_6_hlm,      GB::set_6_a,        GB::set_7_b,        GB::set_7_c,        GB::set_7_d,        GB::set_7_e,        GB::set_7_h,        GB::set_7_l,        GB::set_7_hlm,      GB::set_7_a,       /* 0xF */
               ],
            cpu:LR35902::init(),
            game:GamePak::init(),
            mem:AuxRAM::init()
        }
    }

    /////////////////////
    // Instruction Set //
    /////////////////////
    
    /// Illegal OP code.
    fn illegal(&mut self) {
        let pc: u16 = self.cpu.get_pc();
        let op: u8 = self.read_byte(pc-1);

        panic!("Illegal opcode: {:X}\n\
                PC: {:X}\n", op, pc);
    }
    
    /// Do nothing.
    fn nop(&mut self) {
        // Do nothing
    }

    /// Rotate A left through carry.
    fn rlca(&mut self) {
        let msb: bool = (self.cpu.get_a() & 0b1000_0000) != 0;
        let mut shift: u8 = self.cpu.get_a() << 1;

        if msb {
            self.cpu.set_carr_flag();
            shift |= 0b0000_0001;
        }

        self.cpu.set_a(shift);
        self.cpu.reset_half_flag();
        self.cpu.reset_subt_flag();
        self.cpu.reset_zero_flag();
    }

    /// Load SP to memory address.
    fn ld_a16_sp(&mut self) {
        let sp: u16 = self.cpu.get_sp();
        let addr: u16 = self.read_word_pc_advance();

        self.write_word_cycle(addr, sp);
    }

    /// Rotate A right through carry.
    fn rrca(&mut self) {
        let lsb: bool = (self.cpu.get_a() & 0b0000_0001) != 0;
        let mut shift: u8 = self.cpu.get_a() >> 1;

        if lsb {
            self.cpu.set_carr_flag();
            shift |= 0b1000_0000;
        }

        self.cpu.set_a(shift);
        self.cpu.reset_half_flag();
        self.cpu.reset_subt_flag();
        self.cpu.reset_zero_flag();
    }

    /// Halt CPU & LCD display until button pressed.
    fn stop(&mut self) {
        // TODO: Implement this
    }

    /// Rotate A left.
    fn rla(&mut self) {
        let msb: bool = (self.cpu.get_a() & 0b1000_0000) != 0;
        let mut shift: u8 = self.cpu.get_a() << 1;

        if msb {
            self.cpu.set_carr_flag();
        }

        if self.cpu.get_carr_flag() != 0 {
            shift |= 0b0000_0001;
        }

        self.cpu.set_a(shift);
        self.cpu.reset_half_flag();
        self.cpu.reset_subt_flag();
        self.cpu.reset_zero_flag();
    }

    /// Jump to byte offset from current PC.
    fn jr_r8(&mut self) {
        self.advance_cycle();

        let pc: u16 = self.cpu.get_pc();
        let offset: u8 = self.read_pc_advance();
        
        self.cpu.set_pc(pc.wrapping_add(offset.into()));
    }

    /// Rotate A left.
    fn rra(&mut self) {
        let lsb: bool = (self.cpu.get_a() & 0b0000_0001) != 0;
        let mut shift: u8 = self.cpu.get_a() >> 1;

        if lsb {
            self.cpu.set_carr_flag();
        }

        if self.cpu.get_carr_flag() != 0 {
            shift |= 0b1000_0000;
        }

        self.cpu.set_a(shift);
        self.cpu.reset_half_flag();
        self.cpu.reset_subt_flag();
        self.cpu.reset_zero_flag();
    }

    /// Binary coded decimal. Absolute travesty.
    fn daa(&mut self) {
        let mut result: i16 = self.cpu.get_a().into();

        if self.cpu.get_subt_flag() != 0 {
            if self.cpu.get_half_flag() != 0 {
                result = (result - 0x06) & 0xFF;
            }

            if self.cpu.get_carr_flag() != 0 {
                result -= 0x60;
            }
        }
        else {
            if (self.cpu.get_half_flag() != 0) || (result & 0x0F) > 0x09 {
                result += 0x06;
            }

            if (self.cpu.get_carr_flag() != 0) || result > 0x9F {
                result += 0x60;
            }
        }

        if (result & 0xFF) == 0 {
            self.cpu.set_zero_flag();
        }

        if (result & 0x100) == 0x100 {
            self.cpu.set_carr_flag();
        }

        self.cpu.reset_half_flag();
    }

    /// Increment byte at address contained in HL.
    fn inc_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let new: u8 = old.wrapping_add(1);
        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();

        if new & 0xF == 0 {
            self.cpu.set_half_flag();
        }
    }

    /// Decrement byte at address contained in HL.
    fn dec_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let new: u8 = old.wrapping_sub(1);
        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.set_subt_flag();

        if new & 0xF == 0xF {
            self.cpu.set_half_flag();
        }
    }

    /// Complement bits in A.
    fn cpl(&mut self) {
        let old: u8 = self.cpu.get_a();
        self.cpu.set_a(!old);

        self.cpu.set_subt_flag();
        self.cpu.set_half_flag();
    }

    /// Load byte into memory address stored in HL.
    fn ld_mhl_d8(&mut self) {
        let value: u8 = self.read_pc_advance();
        let addr: u16 = self.cpu.get_hl();
        self.write_cycle(addr, value);
    }

    /// Set carry flag.
    fn scf(&mut self) {
        self.cpu.set_carr_flag();
        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();
    }

    /// Complement carry flag.
    fn ccf(&mut self) {
        if self.cpu.get_carr_flag() == 0 {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();
    }

    /// Halt CPU.
    fn halt(&mut self) {
        // TODO: Implement this.
    }

    /// Add value at memory address stored in HL to A.
    fn add_a_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.cpu.get_a();
        let val: u8 = self.read_cycle(addr);
        let new: u8 = old.wrapping_add(val);
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();

        if (old & 0xF).wrapping_add(val & 0xF) > 0x0F {
            self.cpu.set_half_flag();
        }

        if new < old {
            self.cpu.set_carr_flag();
        }
    }

    /// Add value at memory address stored in HL to A through carry.
    fn adc_a_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.cpu.get_a();
        let val: u8 = self.read_cycle(addr);
        let carry: u8 = self.cpu.get_carr_flag();
        let new: u8 = old.wrapping_add(val).wrapping_add(carry);
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();

        if (old & 0xF).wrapping_add(val & 0xF).wrapping_add(carry) > 0x0F {
            self.cpu.set_half_flag();
        }

        if new < old {
            self.cpu.set_carr_flag();
        }
    }

    /// Subtract value at memory address stored in HL from A.
    fn sub_a_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.cpu.get_a();
        let val: u8 = self.read_cycle(addr);
        let new: u8 = old.wrapping_sub(val);
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.set_subt_flag();

        if (old & 0xF) < (val & 0xF) {
            self.cpu.set_half_flag();
        }

        if new > old {
            self.cpu.set_carr_flag();
        }
    }

    /// Subtract value at memory address stored in HL from A through carry.
    fn sbc_a_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.cpu.get_a();
        let val: u8 = self.read_cycle(addr);
        let carry: u8 = self.cpu.get_carr_flag();
        let new: u8 = old.wrapping_sub(val).wrapping_sub(carry);
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.set_subt_flag();

        if (old & 0xF) < (val & 0xF).wrapping_add(carry) {
            self.cpu.set_half_flag();
        }

        if new > old {
            self.cpu.set_carr_flag();
        }
    }

    /// "AND" value at memory address stored in HL with A.
    fn and_a_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.cpu.get_a();
        let val: u8 = self.read_cycle(addr);
        let new: u8 = old & val;
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.set_half_flag();
        self.cpu.reset_carr_flag();
    }

    /// "XOR" value at memory address stored in HL with A.
    fn xor_a_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.cpu.get_a();
        let val: u8 = self.read_cycle(addr);
        let new: u8 = old ^ val;
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();
        self.cpu.reset_carr_flag();
    }

    /// "OR" value at memory address stored in HL with A.
    fn or_a_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.cpu.get_a();
        let val: u8 = self.read_cycle(addr);
        let new: u8 = old | val;
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();
        self.cpu.reset_carr_flag();
    }

    /// Compare value at memory address stored in HL with A,
    /// setting various flags.
    fn cp_a_mhl(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.cpu.get_a();
        let val: u8 = self.read_cycle(addr);

        if old == val {
            self.cpu.set_zero_flag();
        }
        else {
            self.cpu.reset_zero_flag();
        }

        self.cpu.set_subt_flag();

        if (old & 0xF) < (val & 0xF) {
            self.cpu.set_half_flag();
        }
        else {
            self.cpu.reset_half_flag();
        }

        if old < val {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }
    }

    /// Jump to address literal.
    fn jp_a16(&mut self) {
        self.advance_cycle();

        let addr: u16 = self.read_word_pc_advance();
        self.cpu.set_pc(addr);
    }

    /// Add byte to A.
    fn add_a_d8(&mut self) {
        let val: u8 = self.read_pc_advance();
        let old: u8 = self.cpu.get_a();
        let new: u8 = old.wrapping_add(val);
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();

        if (old & 0xF).wrapping_add(val & 0xF) > 0x0F {
            self.cpu.set_half_flag();
        }

        if new < old {
            self.cpu.set_carr_flag();
        }
    }

    /// Return.
    fn ret(&mut self) {
        self.advance_cycle();

        let mut sp: u16 = self.cpu.get_sp();
        let lo: u8 = self.read_cycle(sp);
        self.cpu.increment_sp();
        
        sp = self.cpu.get_sp();
        let hi: u8 = self.read_cycle(sp);
        self.cpu.increment_sp();

        self.cpu.set_pc(MERGE_U8!(hi, lo));
    }

    /// CB prefix
    fn prefix_cb(&mut self) {
        let op: u8 = self.read_pc_advance();
        self.cb[op as usize](self);
    }

    /// Call.
    fn call(&mut self) {
        self.advance_cycle(); 

        let new_lo: u8 = self.read_pc_advance();
        let new_hi: u8 = self.read_pc_advance();

        let pc: u16 = self.cpu.get_pc();
        let old_lo: u8 = SPLIT_U16!(pc)[1];
        let old_hi: u8 = SPLIT_U16!(pc)[0];

        self.cpu.decrement_sp();
        let mut sp: u16 = self.cpu.get_sp();
        self.write_cycle(sp, old_hi);

        self.cpu.decrement_sp();
        sp = self.cpu.get_sp();
        self.write_cycle(sp, old_lo);

        self.cpu.set_pc(MERGE_U8!(new_hi, new_lo));
    }

    /// Add byte to A through carry.
    fn adc_a_d8(&mut self) {
        let val: u8 = self.read_pc_advance();
        let old: u8 = self.cpu.get_a();
        let carry: u8 = self.cpu.get_carr_flag();
        let new: u8 = old.wrapping_add(val).wrapping_add(carry);
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();

        if (old & 0xF).wrapping_add(val & 0xF).wrapping_add(carry) > 0x0F {
            self.cpu.set_half_flag();
        }

        if new < old {
            self.cpu.set_carr_flag();
        }
    }

    /// Subtract byte from A.
    fn sub_a_d8(&mut self) {
        let val: u8 = self.read_pc_advance();
        let old: u8 = self.cpu.get_a();
        let new: u8 = old.wrapping_add(val);
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.set_subt_flag();

        if (old & 0xF) < (val & 0xF) {
            self.cpu.set_half_flag();
        }

        if new > old {
            self.cpu.set_carr_flag();
        }
    }

    /// Return & enable interrupts.
    fn reti(&mut self) {
        self.advance_cycle();

        let mut sp: u16 = self.cpu.get_sp();
        let lo: u8 = self.read_cycle(sp);
        self.cpu.increment_sp();
        
        sp = self.cpu.get_sp();
        let hi: u8 = self.read_cycle(sp);
        self.cpu.increment_sp();

        self.cpu.set_pc(MERGE_U8!(hi, lo));

        // TODO: Implement interrupt enable.
    }

    /// Subtract byte from A through carry.
    fn sbc_a_d8(&mut self) {
        let val: u8 = self.read_pc_advance();
        let old: u8 = self.cpu.get_a();
        let carry: u8 = self.cpu.get_carr_flag();
        let new: u8 = old.wrapping_sub(val).wrapping_sub(carry);
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.set_subt_flag();

        if (old & 0xF) < (val & 0xF).wrapping_add(carry) {
            self.cpu.set_half_flag();
        }

        if new > old {
            self.cpu.set_carr_flag();
        }
    }

    /// Load A into memory address $FF00+offset byte.
    fn ldh_a8_a(&mut self) {
        let offset: u8 = self.read_pc_advance();
        let reg: u8 = self.cpu.get_a();

        self.write_cycle(0xFF00+offset as u16, reg);
    }

    /// Load A into memory address $FF00+C.
    fn ld_cm_a(&mut self) {
        let reg: u8 = self.cpu.get_a();
        let offset: u8 = self.cpu.get_c();

        self.write_cycle(0xFF00+offset as u16, reg);
    }

    /// "AND" byte with A.
    fn and_a_d8(&mut self) {
        let val: u8 = self.read_pc_advance();
        let old: u8 = self.cpu.get_a();
        let new: u8 = old & val;
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.set_half_flag();
        self.cpu.reset_carr_flag();
    }


    /// Add byte to SP.
    fn add_sp_r8(&mut self) {
        self.advance_cycle();
        self.advance_cycle();

        let old: u16 = self.cpu.get_sp();
        let val: u8 = self.read_pc_advance();
        let new: u16 = old.wrapping_add(val.into());
        self.cpu.set_sp(new);

        self.cpu.reset_zero_flag();
        self.cpu.reset_subt_flag();

        if ((old & 0xFFF).wrapping_add(val as u16 & 0xFFF)) & 0x1000 != 0 {
            self.cpu.set_half_flag();
        }

        if new < old {
            self.cpu.set_carr_flag();
        }
    }

    /// Jump to address in HL.
    fn jp_hlm(&mut self) {
        let reg: u16 = self.cpu.get_hl();
        self.cpu.set_pc(reg);
    }

    /// Load A to memory address.
    fn ld_a16_a(&mut self) {
        let addr: u16 = self.read_word_pc_advance();
        let reg: u8 = self.cpu.get_a();

        self.write_cycle(addr, reg);
    }

    /// "XOR" byte wth A.
    fn xor_a_d8(&mut self) {
        let val: u8 = self.read_pc_advance();
        let old: u8 = self.cpu.get_a();
        let new: u8 = old ^ val;
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();
        self.cpu.reset_carr_flag();
    }

    /// Load value at $FF00+offset into A.
    fn ldh_a_a8(&mut self) {
        let offset: u8 = self.read_pc_advance();
        let val: u8 = self.read_cycle(0xFF00+offset as u16);

        self.cpu.set_a(val);
    }

    /// Load value at $FF00+C into A.
    fn ld_a_cm(&mut self) {
        let offset: u8 = self.cpu.get_c();
        let val: u8 = self.read_cycle(0xFF00+offset as u16);

        self.cpu.set_a(val);
    }

    /// Disable interrupts.
    fn di(&mut self) {
        // TODO: Implement this.
    }

    /// "OR" byte with A.
    fn or_a_d8(&mut self) {
        let val: u8 = self.read_pc_advance();
        let old: u8 = self.cpu.get_a();
        let new: u8 = old | val;
        self.cpu.set_a(new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();
        self.cpu.reset_carr_flag();
    }

    /// Load SP+offset into HL.
    fn ld_hl_sp_r8(&mut self) {
        self.advance_cycle();

        unsafe {
            let offset: i8 = transmute(self.read_pc_advance());
            let sp: u16 = self.cpu.get_sp();
            let new: u16 = (sp as i32).wrapping_add(offset.into()).try_into().unwrap();

            self.cpu.set_hl(new);

            if (sp & 0xF) as i32 + (offset & 0xF) as i32 > 0xF {
                self.cpu.set_half_flag();
            }

            if (sp & 0xFF) as i32 + (offset & 127) as i32 > 0xFF {
                self.cpu.set_carr_flag();
            }
        }
    }

    /// Load HL into SP.
    fn ld_sp_hl(&mut self) {
        self.advance_cycle();

        let reg: u16 = self.cpu.get_hl();

        self.cpu.set_sp(reg);
    }

    /// Load value at memory address into A.
    fn ld_a_a16(&mut self) {
        let addr: u16 = self.read_word_pc_advance();
        let val: u8 = self.read_cycle(addr);

        self.cpu.set_a(val);
    }

    /// Enable interrupts.
    fn ei(&mut self) {
        // TODO: Implement this.
    }

    /// Compare byte with A, setting various flags.
    fn cp_a_d8(&mut self) {
        let val: u8 = self.read_pc_advance();
        let old: u8 = self.cpu.get_a();

        if old == val {
            self.cpu.set_zero_flag();
        }
        else {
            self.cpu.reset_zero_flag();
        }

        self.cpu.set_subt_flag();

        if (old & 0xF) < (val & 0xF) {
            self.cpu.set_half_flag();
        }
        else {
            self.cpu.reset_half_flag();
        }

        if old < val {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }
    }

    /// Rotate value at address stored in HL left through carry.
    fn rlc_hlm(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let new: u8 = old.rotate_left(1);

        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();

        if (old & 0b1000_0000) != 0 {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }
    }

    /// Rotate value at address stored in HL right through carry.
    fn rrc_hlm(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let new: u8 = old.rotate_right(1);

        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();

        if (old & 0b0000_0001) != 0 {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }
    }

    /// Rotate value at address stored in HL left.
    fn rl_hlm(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let carry: u8 = if self.cpu.get_carr_flag() != 0 { 0b0000_0001 } else { 0b0000_0000 };
        let new: u8 = (old << 1) | carry;

        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();

        if (old & 0b1000_0000) != 0 {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }
    }

    /// Rotate value at address stored in HL right.
    fn rr_hlm(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let carry: u8 = if self.cpu.get_carr_flag() != 0 { 0b1000_0000 } else { 0b0000_0000 };
        let new: u8 = (old >> 1) | carry;

        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();

        if (old & 0b0000_0001) != 0 {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }
    }

    /// Shift register left into carry.
    fn sla_hlm(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let new: u8 = old << 1;

        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();

        if (old & 0b1000_0000) != 0 {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }
    }

    /// Shift value at memory address in HL right (arithmetic) into carry.
    fn sra_hlm(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let sign: u8 = if old & 0b1000_0000 != 0 { 0b1000_0000 } else { 0b0000_0000 };
        let new: u8 = (old >> 1) | sign;

        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();

        if (old & 0b0000_0001) != 0 {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }
    }

    /// Swap upper and lower nibbles of value at memory address in HL.
    fn swap_hlm(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let lo: u8 = (old & 0b1111_0000) >> 4;
        let hi: u8 = (old & 0b0000_1111) << 4;
        let new: u8 = hi | lo;

        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();
        self.cpu.reset_carr_flag();
    }

    /// Shift value at memory address in HL right (logical) into carry.
    fn srl_hlm(&mut self) {
        let addr: u16 = self.cpu.get_hl();
        let old: u8 = self.read_cycle(addr);
        let new: u8 = (old >> 1) & 0b0111_1111;

        self.write_cycle(addr, new);

        if new == 0 {
            self.cpu.set_zero_flag();
        }

        self.cpu.reset_subt_flag();
        self.cpu.reset_half_flag();

        if (old & 0b0000_0001) != 0 {
            self.cpu.set_carr_flag();
        }
        else {
            self.cpu.reset_carr_flag();
        }
    }

    LD_XX_D16!(bc);
    LD_XX_D16!(de);
    LD_XX_D16!(hl);
    LD_XX_D16!(sp);
    LD_XXM_Y!(bc, a);
    LD_XXM_Y!(de, a);
    LD_XXM_Y!(hl, a);
    LD_XXM_Y!(hl, b);
    LD_XXM_Y!(hl, c);
    LD_XXM_Y!(hl, d);
    LD_XXM_Y!(hl, e);
    LD_XXM_Y!(hl, h);
    LD_XXM_Y!(hl, l);
    INC_XX!(bc);
    INC_XX!(de);
    INC_XX!(hl);
    INC_XX!(sp);
    INC_X!(a);
    INC_X!(b);
    INC_X!(c);
    INC_X!(d);
    INC_X!(e);
    INC_X!(h);
    INC_X!(l);
    DEC_X!(a);
    DEC_X!(b);
    DEC_X!(c);
    DEC_X!(d);
    DEC_X!(e);
    DEC_X!(h);
    DEC_X!(l);
    LD_HLX_A!(add);
    LD_HLX_A!(sub);
    ADD_HL_XX!(bc);
    ADD_HL_XX!(de);
    ADD_HL_XX!(hl);
    ADD_HL_XX!(sp);
    LD_X_YYM!(a, bc);
    LD_X_YYM!(a, de);
    LD_X_YYM!(a, hl);
    LD_X_YYM!(b, hl);
    LD_X_YYM!(c, hl);
    LD_X_YYM!(d, hl);
    LD_X_YYM!(e, hl);
    LD_X_YYM!(h, hl);
    LD_X_YYM!(l, hl);
    DEC_XX!(bc);
    DEC_XX!(de);
    DEC_XX!(hl);
    DEC_XX!(sp);
    LD_X_D8!(a);
    LD_X_D8!(b);
    LD_X_D8!(c);
    LD_X_D8!(d);
    LD_X_D8!(e);
    LD_X_D8!(h);
    LD_X_D8!(l);
    JR_NX_R8!(zero);
    JR_NX_R8!(carr);
    JR_X_R8!(zero);
    JR_X_R8!(carr);
    LD_A_HLX!(add);
    LD_A_HLX!(sub);
    LD_X_Y!(a, a);
    LD_X_Y!(a, b);
    LD_X_Y!(a, c);
    LD_X_Y!(a, d);
    LD_X_Y!(a, e);
    LD_X_Y!(a, h);
    LD_X_Y!(a, l);
    LD_X_Y!(b, a);
    LD_X_Y!(b, b);
    LD_X_Y!(b, c);
    LD_X_Y!(b, d);
    LD_X_Y!(b, e);
    LD_X_Y!(b, h);
    LD_X_Y!(b, l);
    LD_X_Y!(c, a);
    LD_X_Y!(c, b);
    LD_X_Y!(c, c);
    LD_X_Y!(c, d);
    LD_X_Y!(c, e);
    LD_X_Y!(c, h);
    LD_X_Y!(c, l);
    LD_X_Y!(d, a);
    LD_X_Y!(d, b);
    LD_X_Y!(d, c);
    LD_X_Y!(d, d);
    LD_X_Y!(d, e);
    LD_X_Y!(d, h);
    LD_X_Y!(d, l);
    LD_X_Y!(e, a);
    LD_X_Y!(e, b);
    LD_X_Y!(e, c);
    LD_X_Y!(e, d);
    LD_X_Y!(e, e);
    LD_X_Y!(e, h);
    LD_X_Y!(e, l);
    LD_X_Y!(h, a);
    LD_X_Y!(h, b);
    LD_X_Y!(h, c);
    LD_X_Y!(h, d);
    LD_X_Y!(h, e);
    LD_X_Y!(h, h);
    LD_X_Y!(h, l);
    LD_X_Y!(l, a);
    LD_X_Y!(l, b);
    LD_X_Y!(l, c);
    LD_X_Y!(l, d);
    LD_X_Y!(l, e);
    LD_X_Y!(l, h);
    LD_X_Y!(l, l);
    ADD_A_X!(a);
    ADD_A_X!(b);
    ADD_A_X!(c);
    ADD_A_X!(d);
    ADD_A_X!(e);
    ADD_A_X!(h);
    ADD_A_X!(l);
    ADC_A_X!(a);
    ADC_A_X!(b);
    ADC_A_X!(c);
    ADC_A_X!(d);
    ADC_A_X!(e);
    ADC_A_X!(h);
    ADC_A_X!(l);
    SUB_A_X!(a);
    SUB_A_X!(b);
    SUB_A_X!(c);
    SUB_A_X!(d);
    SUB_A_X!(e);
    SUB_A_X!(h);
    SUB_A_X!(l);
    SBC_A_X!(a);
    SBC_A_X!(b);
    SBC_A_X!(c);
    SBC_A_X!(d);
    SBC_A_X!(e);
    SBC_A_X!(h);
    SBC_A_X!(l);
    AND_A_X!(a);
    AND_A_X!(b);
    AND_A_X!(c);
    AND_A_X!(d);
    AND_A_X!(e);
    AND_A_X!(h);
    AND_A_X!(l);
    XOR_A_X!(a);
    XOR_A_X!(b);
    XOR_A_X!(c);
    XOR_A_X!(d);
    XOR_A_X!(e);
    XOR_A_X!(h);
    XOR_A_X!(l);
    OR_A_X!(a);
    OR_A_X!(b);
    OR_A_X!(c);
    OR_A_X!(d);
    OR_A_X!(e);
    OR_A_X!(h);
    OR_A_X!(l);
    CP_A_X!(a);
    CP_A_X!(b);
    CP_A_X!(c);
    CP_A_X!(d);
    CP_A_X!(e);
    CP_A_X!(h);
    CP_A_X!(l);
    RET_NX!(zero);
    RET_NX!(carr);
    POP_XX!(bc);
    POP_XX!(de);
    POP_XX!(hl);
    POP_XX!(af);
    JP_NX_A16!(zero);
    JP_NX_A16!(carr);
    CALL_NX_A16!(zero);
    CALL_NX_A16!(carr);
    PUSH_XX!(b, c);
    PUSH_XX!(d, e);
    PUSH_XX!(h, l);
    PUSH_XX!(a, f);
    RET_X!(zero);
    RET_X!(carr);
    RST_XXH!(00);
    RST_XXH!(10);
    RST_XXH!(20);
    RST_XXH!(30);
    RST_XXH!(08);
    RST_XXH!(18);
    RST_XXH!(28);
    RST_XXH!(38);
    JP_X_A16!(zero);
    JP_X_A16!(carr);
    CALL_X_A16!(zero);
    CALL_X_A16!(carr);
    RLC_X!(a);
    RLC_X!(b);
    RLC_X!(c);
    RLC_X!(d);
    RLC_X!(e);
    RLC_X!(h);
    RLC_X!(l);
    RRC_X!(a);
    RRC_X!(b);
    RRC_X!(c);
    RRC_X!(d);
    RRC_X!(e);
    RRC_X!(h);
    RRC_X!(l);
    RL_X!(a);
    RL_X!(b);
    RL_X!(c);
    RL_X!(d);
    RL_X!(e);
    RL_X!(h);
    RL_X!(l);
    RR_X!(a);
    RR_X!(b);
    RR_X!(c);
    RR_X!(d);
    RR_X!(e);
    RR_X!(h);
    RR_X!(l);
    SLA_X!(a);
    SLA_X!(b);
    SLA_X!(c);
    SLA_X!(d);
    SLA_X!(e);
    SLA_X!(h);
    SLA_X!(l);
    SRA_X!(a);
    SRA_X!(b);
    SRA_X!(c);
    SRA_X!(d);
    SRA_X!(e);
    SRA_X!(h);
    SRA_X!(l);
    SWAP_X!(a);
    SWAP_X!(b);
    SWAP_X!(c);
    SWAP_X!(d);
    SWAP_X!(e);
    SWAP_X!(h);
    SWAP_X!(l);
    SRL_X!(a);
    SRL_X!(b);
    SRL_X!(c);
    SRL_X!(d);
    SRL_X!(e);
    SRL_X!(h);
    SRL_X!(l);
    BIT_B_X!(0, a);
    BIT_B_X!(0, b);
    BIT_B_X!(0, c);
    BIT_B_X!(0, d);
    BIT_B_X!(0, e);
    BIT_B_X!(0, h);
    BIT_B_X!(0, l);
    BIT_B_X!(1, a);
    BIT_B_X!(1, b);
    BIT_B_X!(1, c);
    BIT_B_X!(1, d);
    BIT_B_X!(1, e);
    BIT_B_X!(1, h);
    BIT_B_X!(1, l);
    BIT_B_X!(2, a);
    BIT_B_X!(2, b);
    BIT_B_X!(2, c);
    BIT_B_X!(2, d);
    BIT_B_X!(2, e);
    BIT_B_X!(2, h);
    BIT_B_X!(2, l);
    BIT_B_X!(3, a);
    BIT_B_X!(3, b);
    BIT_B_X!(3, c);
    BIT_B_X!(3, d);
    BIT_B_X!(3, e);
    BIT_B_X!(3, h);
    BIT_B_X!(3, l);
    BIT_B_X!(4, a);
    BIT_B_X!(4, b);
    BIT_B_X!(4, c);
    BIT_B_X!(4, d);
    BIT_B_X!(4, e);
    BIT_B_X!(4, h);
    BIT_B_X!(4, l);
    BIT_B_X!(5, a);
    BIT_B_X!(5, b);
    BIT_B_X!(5, c);
    BIT_B_X!(5, d);
    BIT_B_X!(5, e);
    BIT_B_X!(5, h);
    BIT_B_X!(5, l);
    BIT_B_X!(6, a);
    BIT_B_X!(6, b);
    BIT_B_X!(6, c);
    BIT_B_X!(6, d);
    BIT_B_X!(6, e);
    BIT_B_X!(6, h);
    BIT_B_X!(6, l);
    BIT_B_X!(7, a);
    BIT_B_X!(7, b);
    BIT_B_X!(7, c);
    BIT_B_X!(7, d);
    BIT_B_X!(7, e);
    BIT_B_X!(7, h);
    BIT_B_X!(7, l);
    RES_B_X!(0, a);
    RES_B_X!(0, b);
    RES_B_X!(0, c);
    RES_B_X!(0, d);
    RES_B_X!(0, e);
    RES_B_X!(0, h);
    RES_B_X!(0, l);
    RES_B_X!(1, a);
    RES_B_X!(1, b);
    RES_B_X!(1, c);
    RES_B_X!(1, d);
    RES_B_X!(1, e);
    RES_B_X!(1, h);
    RES_B_X!(1, l);
    RES_B_X!(2, a);
    RES_B_X!(2, b);
    RES_B_X!(2, c);
    RES_B_X!(2, d);
    RES_B_X!(2, e);
    RES_B_X!(2, h);
    RES_B_X!(2, l);
    RES_B_X!(3, a);
    RES_B_X!(3, b);
    RES_B_X!(3, c);
    RES_B_X!(3, d);
    RES_B_X!(3, e);
    RES_B_X!(3, h);
    RES_B_X!(3, l);
    RES_B_X!(4, a);
    RES_B_X!(4, b);
    RES_B_X!(4, c);
    RES_B_X!(4, d);
    RES_B_X!(4, e);
    RES_B_X!(4, h);
    RES_B_X!(4, l);
    RES_B_X!(5, a);
    RES_B_X!(5, b);
    RES_B_X!(5, c);
    RES_B_X!(5, d);
    RES_B_X!(5, e);
    RES_B_X!(5, h);
    RES_B_X!(5, l);
    RES_B_X!(6, a);
    RES_B_X!(6, b);
    RES_B_X!(6, c);
    RES_B_X!(6, d);
    RES_B_X!(6, e);
    RES_B_X!(6, h);
    RES_B_X!(6, l);
    RES_B_X!(7, a);
    RES_B_X!(7, b);
    RES_B_X!(7, c);
    RES_B_X!(7, d);
    RES_B_X!(7, e);
    RES_B_X!(7, h);
    RES_B_X!(7, l);
    SET_B_X!(0, a);
    SET_B_X!(0, b);
    SET_B_X!(0, c);
    SET_B_X!(0, d);
    SET_B_X!(0, e);
    SET_B_X!(0, h);
    SET_B_X!(0, l);
    SET_B_X!(1, a);
    SET_B_X!(1, b);
    SET_B_X!(1, c);
    SET_B_X!(1, d);
    SET_B_X!(1, e);
    SET_B_X!(1, h);
    SET_B_X!(1, l);
    SET_B_X!(2, a);
    SET_B_X!(2, b);
    SET_B_X!(2, c);
    SET_B_X!(2, d);
    SET_B_X!(2, e);
    SET_B_X!(2, h);
    SET_B_X!(2, l);
    SET_B_X!(3, a);
    SET_B_X!(3, b);
    SET_B_X!(3, c);
    SET_B_X!(3, d);
    SET_B_X!(3, e);
    SET_B_X!(3, h);
    SET_B_X!(3, l);
    SET_B_X!(4, a);
    SET_B_X!(4, b);
    SET_B_X!(4, c);
    SET_B_X!(4, d);
    SET_B_X!(4, e);
    SET_B_X!(4, h);
    SET_B_X!(4, l);
    SET_B_X!(5, a);
    SET_B_X!(5, b);
    SET_B_X!(5, c);
    SET_B_X!(5, d);
    SET_B_X!(5, e);
    SET_B_X!(5, h);
    SET_B_X!(5, l);
    SET_B_X!(6, a);
    SET_B_X!(6, b);
    SET_B_X!(6, c);
    SET_B_X!(6, d);
    SET_B_X!(6, e);
    SET_B_X!(6, h);
    SET_B_X!(6, l);
    SET_B_X!(7, a);
    SET_B_X!(7, b);
    SET_B_X!(7, c);
    SET_B_X!(7, d);
    SET_B_X!(7, e);
    SET_B_X!(7, h);
    SET_B_X!(7, l);
    BIT_B_HLM!(0);
    BIT_B_HLM!(1);
    BIT_B_HLM!(2);
    BIT_B_HLM!(3);
    BIT_B_HLM!(4);
    BIT_B_HLM!(5);
    BIT_B_HLM!(6);
    BIT_B_HLM!(7);
    RES_B_HLM!(0);
    RES_B_HLM!(1);
    RES_B_HLM!(2);
    RES_B_HLM!(3);
    RES_B_HLM!(4);
    RES_B_HLM!(5);
    RES_B_HLM!(6);
    RES_B_HLM!(7);
    SET_B_HLM!(0);
    SET_B_HLM!(1);
    SET_B_HLM!(2);
    SET_B_HLM!(3);
    SET_B_HLM!(4);
    SET_B_HLM!(5);
    SET_B_HLM!(6);
    SET_B_HLM!(7);

    ////////////
    // Memory //
    ////////////

    /// Read byte from the memory map.
    /// Address must be native-endian.
    pub fn read_byte(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x00FF => { // Interrupt vector & RST address
                return self.game.rom[addr as usize];
            },
            0x0100..=0x7FFF => { // User Program Area
                return self.game.rom[addr as usize - 0x100];
            },
            0x8000..=0x9FFF => { // LCD Display RAM
                return self.mem.disp_ram[addr as usize - 0x8000];
            },
            0xA000..=0xBFFF => { // GamePak RAM
                return self.game.ram[addr as usize - 0xA000];
            },
            0xC000..=0xDFFF => { // Working RAM
                return self.mem.work_ram[addr as usize - 0xC000];
            },
            0xE000..=0xFDFF => { // "Prohibited area" - Echoes $C000-$DDFF
                return self.mem.work_ram[addr as usize - 0xE000];
            },
            0xFE00..=0xFEBF => { // OAM
                return self.cpu.oam_ram[addr as usize - 0xFE00];
            },
            0xFEA0..=0xFEFF => { // Unused CPU RAM
                return 0x0;
            },
            0xFF00..=0xFF7F => { // Flags & Registers
                return self.cpu.flags[addr as usize - 0xFF00];
            },
            0xFF80..=0xFFFE => { // CPU RAM
                return self.cpu.ram[addr as usize - 0xFF80];
            },
            0xFFFF => { // IE
                return self.cpu.ie;
            }
        }
    }
    
    /// Write byte to somewhere in the memory map.
    /// Address must be native-endian.
    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x00FF => { // Interrupt vector & RST address
                // Do nothing
            },
            0x0100..=0x7FFF => { // User Program Area
                // Do nothing
            },
            0x8000..=0x9FFF => { // LCD Display RAM
                self.mem.disp_ram[addr as usize - 0x8000] = byte;
            },
            0xA000..=0xBFFF => { // GamePak RAM
                self.game.ram[addr as usize - 0xA000] = byte;
            },
            0xC000..=0xDFFF => { // Working RAM
                self.mem.work_ram[addr as usize - 0xC000] = byte;
            },
            0xE000..=0xFDFF => { // "Prohibited area" - Echoes $C000-$DDFF
                self.mem.work_ram[addr as usize - 0xE000] = byte;
            },
            0xFE00..=0xFEBF => { // OAM
                self.cpu.oam_ram[addr as usize - 0xFE00] = byte;
            },
            0xFEA0..=0xFEFF => { // Unused CPU RAM
                // Do nothing?
            },
            0xFF00..=0xFF7F => { // Flags & Registers
                self.cpu.flags[addr as usize - 0xFF00] = byte;
            },
            0xFF80..=0xFFFE => { // CPU RAM
                self.cpu.ram[addr as usize - 0xFF80] = byte;
            },
            0xFFFF => { // IE
                self.cpu.ie = byte;
            }
        }
    }

    /// Read word in native-endian at addr.
    pub fn read_word(&mut self, addr: u16) -> u16 {
        let high: u8 = self.read_byte(addr+1);
        let low: u8 = self.read_byte(addr);

        return MERGE_U8!(high, low);
    }

    /// Write native-endian word starting at addr.
    pub fn write_word(&mut self, addr: u16, word: u16) {
        let bytes: [u8; 2] = SPLIT_U16!(word);
        
        self.write_byte(addr, bytes[1]);
        self.write_byte(addr+1, bytes[0]);
    }

    /// Queue 4 CPU cycles.
    pub fn advance_cycle(&mut self) {
        self.cpu.pending_cycles = self.cpu.pending_cycles.saturating_add(4);
    } 

    /// Read byte and advance CPU cycle by 4.
    pub fn read_cycle(&mut self, addr: u16) -> u8 {
        self.advance_cycle();

        return self.read_byte(addr);
    }

    /// Write byte and advance CPU cycle by 4.
    pub fn write_cycle(&mut self, addr: u16, byte: u8) {
        self.advance_cycle();

        self.write_byte(addr, byte);
    }

    /// Read word and advance CPU cycle by 8.
    pub fn read_word_cycle(&mut self, addr: u16) -> u16 {
        let high: u8 = self.read_cycle(addr+1);
        let low: u8 = self.read_cycle(addr);

        return MERGE_U8!(high, low);
    }

    /// Write word and advance CPU cycle by 8.
    pub fn write_word_cycle(&mut self, addr: u16, word: u16) {
        let bytes: [u8; 2] = SPLIT_U16!(word);

        self.write_cycle(addr, bytes[1]);
        self.write_cycle(addr+1, bytes[0]);
    }

    /// Read memory at PC, advance PC, then advance CPU cycle by 4.
    pub fn read_pc_advance(&mut self) -> u8 {
        let pc: u16 = self.cpu.get_pc_advance();

        return self.read_cycle(pc);
    }

    /// Read word in native-endian starting at PC, 
    /// advance PC twice, then advance CPU cycle by 8.
    pub fn read_word_pc_advance(&mut self) -> u16 {
        let first: u8 = self.read_pc_advance();
        let second: u8 = self.read_pc_advance();

        return MERGE_U8!(second, first);
    }
}

////////////////
// Unit tests //
////////////////

#[cfg(test)]
mod tests {
    use crate::gb_cpu::{FLAG_ZERO, FLAG_SUBT, FLAG_HALF, FLAG_CARR};

    use super::*;

    /// Test reading and writing memory.
    #[test]
    fn read_write() {
        let mut gb: GB = GB::init();

        // Default value should be 0x80 at $FF10.
        let read: u8 = gb.read_byte(0xFF10);
        assert_eq!(read, 0x80);

        // Writing to ROM space should be ignored.
        gb.write_byte(0x0000, 0x69);
        assert_ne!(gb.read_byte(0x0000), 0x69);

        // Writing to working RAM.
        gb.write_byte(0xC000, 0x69);
        assert_eq!(gb.read_byte(0xC000), 0x69);

        // Writing and reading IE byte.
        gb.write_byte(0xFFFF, 0x69);
        assert_eq!(gb.read_byte(0xFFFF), 0x69);
    }

    /// Test reading and writing words from/to memory.
    #[test]
    fn read_write_word() {
        let mut gb: GB = GB::init();
        let word: u16 = 0xFF11;
        
        gb.write_word(0xA000, word);

        assert_eq!(gb.read_byte(0xA000), 0x11);
        assert_eq!(gb.read_byte(0xA001), 0xFF);
        assert_eq!(gb.read_word(0xA000), 0xFF11);
    }

    #[test]
    fn read_pc_advance_test() {
        let mut gb: GB = GB::init();

        gb.cpu.set_pc(0xA000); // Set to a writable & readable area

        gb.write_byte(gb.cpu.get_pc(), 0x11);
        assert_eq!(gb.read_pc_advance(), 0x11);

        gb.write_byte(gb.cpu.get_pc(), 0x10);
        gb.write_byte(gb.cpu.get_pc()+1, 0x41);
        assert_eq!(gb.read_word_pc_advance(), 0x4110);
    }

    /// Test CPU flags.
    #[test]
    fn flags() {
        let mut gb: GB = GB::init();

        gb.cpu.set_f(0x0);
        assert_eq!(gb.cpu.get_zero_flag(), 0x0);
        assert_eq!(gb.cpu.get_subt_flag(), 0x0);
        assert_eq!(gb.cpu.get_half_flag(), 0x0);
        assert_eq!(gb.cpu.get_carr_flag(), 0x0);

        gb.cpu.set_zero_flag();
        assert_eq!(gb.cpu.get_zero_flag(), FLAG_ZERO);
        gb.cpu.set_subt_flag();
        assert_eq!(gb.cpu.get_subt_flag(), FLAG_SUBT);
        gb.cpu.set_half_flag();
        assert_eq!(gb.cpu.get_half_flag(), FLAG_HALF);
        gb.cpu.set_carr_flag();
        assert_eq!(gb.cpu.get_carr_flag(), FLAG_CARR);
    }

    /// Test return opcode.
    #[test]
    fn return_op() {
        let mut gb: GB = GB::init();

        gb.cpu.set_sp(0xA000);
        gb.write_word(0xA000, 0xFF11);
        gb.ret();
        assert_eq!(gb.cpu.get_sp(), 0xA002);
        assert_eq!(gb.cpu.get_pc(), 0xFF11);
    }

    /// Test basic ADD instruction.
    #[test]
    fn add_op() {
        let mut gb: GB = GB::init();

        assert_eq!(gb.cpu.get_a(), 0x1);
        gb.add_a_a();
        assert_eq!(gb.cpu.get_a(), 0x2);
    }

    /// Testing rotating and shifting instructions.
    #[test]
    fn rotate_shift() {
        let mut gb: GB = GB::init();

        // SRA: Test normal SRA usage
        gb.cpu.set_a(0b0000_0010);
        gb.sra_a();
        assert_eq!(gb.cpu.get_a(), 0b0000_0001);

        // SRA: Test edge cases
        gb.cpu.set_a(0b1000_0001);
        gb.sra_a();
        assert_eq!(gb.cpu.get_a(), 0b1100_0000);

        // SRL: Test edge cases
        gb.cpu.set_a(0b1000_0001);
        gb.srl_a();
        assert_eq!(gb.cpu.get_a(), 0b0100_0000);

        // RR: Test
        gb.cpu.set_a(0b0000_0001);
        gb.rr_a();
        assert_eq!(gb.cpu.get_a(), 0b1000_0000);

        // RL: Test
        gb.rl_a();
        assert_eq!(gb.cpu.get_a(), 0b0000_0001);
    }

    /// Test basic BIT instruction.
    #[test]
    fn bit_op() {
        let mut gb: GB = GB::init();

        gb.cpu.reset_zero_flag();
        gb.bit_0_a();
        assert!(gb.cpu.get_zero_flag() == 0);
    }

    /// Test jumping to HL.
    #[test]
    fn jump_hl() {
        let mut gb: GB = GB::init();

        gb.cpu.set_hl(0xC000);
        gb.jp_hlm();
        assert_eq!(gb.cpu.get_pc(), 0xC000);
    }

    /// Test SWAP instruction.
    #[test]
    fn swap_op() {
        let mut gb: GB = GB::init();

        gb.cpu.set_a(0xF1);
        gb.swap_a();
        assert_eq!(gb.cpu.get_a(), 0x1F);
    }

    /// Test CALL instruction.
    #[test]
    fn call_op() {
        let mut gb: GB = GB::init();

        gb.write_word(0xC000, 0xD000);
        gb.write_byte(0xC002, 0x69);
        gb.cpu.set_pc(0xC000);
        gb.call();
        assert_eq!(gb.cpu.get_pc(), 0xD000);
        
        let sp: u16 = gb.cpu.get_sp();
        assert_eq!(gb.read_byte(sp), 0x02);
        assert_eq!(gb.read_byte(sp+1), 0xC0);

        gb.ret();
        assert_eq!(gb.cpu.get_pc(), 0xC002);
    }
}