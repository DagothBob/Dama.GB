#[macro_use] mod utility;

pub struct LR35902 {
    cpu_registers: Registers
}

/// Registers are paired backwards due to little-endian
struct Registers {
    fa: MergedRegister,
    cb: MergedRegister,
    ed: MergedRegister,
    lh: MergedRegister,
    sp: u16,
    pc: u16
}

/// LR35902/Z80 registers can be paired to create
/// a single 16-bit register.
#[repr(C)]
#[derive(Clone, Copy)]
union MergedRegister {
    merged: u16,
    split: [u8;2]
}

impl LR35902 {
    /// Initializer.
    pub fn init() -> LR35902 {
        LR35902 { 
            cpu_registers:Registers::init() 
        }
    }

    /// Get AF registers in little-endian format.
    pub fn get_fa(&self) -> u16 {
        unsafe { self.cpu_registers.fa.merged }
    }

    /// Get AF registers in native-endian format.
    pub fn get_af(&self) -> u16 {
        unsafe { u16::from_le(self.cpu_registers.fa.merged) }
    }

    /// Get A register
    pub fn get_a(&self) -> u8 {
        unsafe { self.cpu_registers.fa.split[0] }
    }

    /// Get F register
    pub fn get_f(&self) -> u8 {
        unsafe { self.cpu_registers.fa.split[1] }
    }

    /// Set AF registers (value must be native-endian).
    pub fn set_af(&mut self, val:u16) {
        self.cpu_registers.fa.merged = val.to_le();
    }

    /// Set A register.
    pub fn set_a(&mut self, val:u8) {
        unsafe { self.cpu_registers.fa.split[0] = val; }
    }

    /// Set F register.
    pub fn set_f(&mut self, val:u8) {
        unsafe { self.cpu_registers.fa.split[1] = val; }
    }

    /// Get BC registers in little-endian format.
    pub fn get_cb(&self) -> u16 {
        unsafe { self.cpu_registers.cb.merged }
    }

    /// Get BC registers in native-endian format.
    pub fn get_bc(&self) -> u16 {
        unsafe { u16::from_le(self.cpu_registers.cb.merged) }
    }

    /// Get B register
    pub fn get_b(&self) -> u8 {
        unsafe { self.cpu_registers.cb.split[0] }
    }

    /// Get C register
    pub fn get_c(&self) -> u8 {
        unsafe { self.cpu_registers.cb.split[1] }
    }

    /// Set BC registers (value must be native-endian).
    pub fn set_bc(&mut self, val:u16) {
        self.cpu_registers.cb.merged = val.to_le();
    }

    /// Set B register.
    pub fn set_b(&mut self, val:u8) {
        unsafe { self.cpu_registers.cb.split[0] = val; }
    }

    /// Set C register.
    pub fn set_c(&mut self, val:u8) {
        unsafe { self.cpu_registers.cb.split[1] = val; }
    }

    /// Get DE registers in little-endian format.
    pub fn get_ed(&self) -> u16 {
        unsafe { self.cpu_registers.ed.merged }
    }

    /// Get DE registers in native-endian format.
    pub fn get_de(&self) -> u16 {
        unsafe { u16::from_le(self.cpu_registers.ed.merged) }
    }

    /// Get D register
    pub fn get_d(&self) -> u8 {
        unsafe { self.cpu_registers.ed.split[0] }
    }

    /// Get E register
    pub fn get_e(&self) -> u8 {
        unsafe { self.cpu_registers.ed.split[1] }
    }

    /// Set DE registers (value must be native-endian).
    pub fn set_de(&mut self, val:u16) {
        self.cpu_registers.ed.merged = val.to_le();
    }

    /// Set D register.
    pub fn set_d(&mut self, val:u8) {
        unsafe { self.cpu_registers.ed.split[0] = val; }
    }

    /// Set E register.
    pub fn set_e(&mut self, val:u8) {
        unsafe { self.cpu_registers.ed.split[1] = val; }
    }

    /// Get HL registers in little-endian format.
    pub fn get_lh(&self) -> u16 {
        unsafe { self.cpu_registers.lh.merged }
    }

    /// Get HL registers in native-endian format.
    pub fn get_hl(&self) -> u16 {
        unsafe { u16::from_le(self.cpu_registers.lh.merged) }
    }

    /// Get H register
    pub fn get_h(&self) -> u8 {
        unsafe { self.cpu_registers.lh.split[0] }
    }

    /// Get L register
    pub fn get_l(&self) -> u8 {
        unsafe { self.cpu_registers.lh.split[1] }
    }

    /// Set HL registers (value must be native-endian).
    pub fn set_hl(&mut self, val:u16) {
        self.cpu_registers.lh.merged = val.to_le();
    }

    /// Set H register.
    pub fn set_h(&mut self, val:u8) {
        unsafe { self.cpu_registers.lh.split[0] = val; }
    }

    /// Set L register.
    pub fn set_l(&mut self, val:u8) {
        unsafe { self.cpu_registers.lh.split[1] = val; }
    }

    /// Get SP register in little-endian format.
    pub fn get_sp_le(&self) -> u16 {
        self.cpu_registers.sp
    }

    /// Get SP register in native-endian format.
    pub fn get_sp_ne(&self) -> u16 {
        u16::from_le(self.cpu_registers.sp)
    }

    /// Set SP register (value must be native-endian).
    pub fn set_sp(&mut self, val:u16) {
        self.cpu_registers.sp = val.to_le();
    }

    /// Get PC register in little-endian format.
    pub fn get_pc_le(&self) -> u16 {
        self.cpu_registers.pc
    }

    /// Get PC registers in native-endian format.
    pub fn get_pc_ne(&self) -> u16 {
        u16::from_le(self.cpu_registers.pc)
    }

    /// Set SP register (value must be native-endian).
    pub fn set_pc(&mut self, val:u16) {
        self.cpu_registers.pc = val.to_le();
    }
}

impl Registers {
    /// Initializes registers to their observed initial state.
    fn init() -> Registers {
        Registers {
            fa:MergedRegister{split: [0xB0, 0x01]},
            cb:MergedRegister{split: [0x13, 0x00]},
            ed:MergedRegister{split: [0xD8, 0x00]},
            lh:MergedRegister{split: [0x4D, 0x01]},
            sp:(0xFFFEu16).to_le(),
            pc:(0x0001u16).to_le()
        }
    }
}

////////////////
// Unit tests //
////////////////

#[cfg(test)]
mod tests {
    use super::*;

    /// Test for MergedRegister union type.
    #[test]
    fn merged_register() {
        let reg:MergedRegister = MergedRegister { merged:(0xFF00u16).to_le() };
        unsafe { 
            assert_eq!(u16::from_le(reg.merged), 0xFF00u16);
            assert_eq!(u16::from_le(merge_u8!(reg.split[0], reg.split[1])), 0xFF00u16);
        }
    }

    /// Test for register_to_ne macro.
    #[test]
    fn native_endian_register() {
        let le_reg:MergedRegister = MergedRegister { merged:(0xFF00u16).to_le() };
        unsafe { 
            let ne_reg:u16 = u16::from_le(le_reg.merged);
            assert_eq!(ne_reg, 0xFF00u16);
        }
    }

    /// Test for init_cpu function and getters and setters.
    #[test]
    fn init_cpu() {
        let mut cpu:LR35902 = LR35902::init();

        // Test init & combined getters //
        assert_eq!(cpu.get_fa(), (0x01B0u16).to_le());
        assert_eq!(cpu.get_af(), 0x01B0u16);
        assert_eq!(cpu.get_cb(), (0x0013u16).to_le());
        assert_eq!(cpu.get_bc(), 0x0013u16);
        assert_eq!(cpu.get_ed(), (0x00D8u16).to_le());
        assert_eq!(cpu.get_de(), 0x00D8u16);
        assert_eq!(cpu.get_lh(), (0x014Du16).to_le());
        assert_eq!(cpu.get_hl(), 0x014Du16);
        assert_eq!(cpu.get_sp_le(), (0xFFFEu16).to_le());
        assert_eq!(cpu.get_sp_ne(), 0xFFFEu16);
        assert_eq!(cpu.get_pc_le(), (0x0001u16).to_le());
        assert_eq!(cpu.get_pc_ne(), 0x0001u16);

        // Test individual setters & getters //
        cpu.set_af(0xDEADu16);
        assert_eq!(cpu.get_af(), 0xDEADu16);
        cpu.set_a(0x13u8);
        assert_eq!(cpu.get_a(), 0x13u8);
        cpu.set_f(0x13u8);
        assert_eq!(cpu.get_f(), 0x13u8);

        cpu.set_bc(0xDEADu16);
        assert_eq!(cpu.get_bc(), 0xDEADu16);
        cpu.set_b(0x13u8);
        assert_eq!(cpu.get_b(), 0x13u8);
        cpu.set_c(0x13u8);
        assert_eq!(cpu.get_c(), 0x13u8);

        cpu.set_de(0xDEADu16);
        assert_eq!(cpu.get_de(), 0xDEADu16);
        cpu.set_d(0x13u8);
        assert_eq!(cpu.get_d(), 0x13u8);
        cpu.set_e(0x13u8);
        assert_eq!(cpu.get_e(), 0x13u8);

        cpu.set_hl(0xDEADu16);
        assert_eq!(cpu.get_hl(), 0xDEADu16);
        cpu.set_h(0x13u8);
        assert_eq!(cpu.get_h(), 0x13u8);
        cpu.set_l(0x13u8);
        assert_eq!(cpu.get_l(), 0x13u8);

        cpu.set_pc(0xDEADu16);
        assert_eq!(cpu.get_pc_ne(), 0xDEADu16);

        cpu.set_sp(0xDEADu16);
        assert_eq!(cpu.get_sp_ne(), 0xDEADu16);
    }
}