#[macro_use] mod utility;

pub const FLAG_ZERO:u8 = 0b1000_0000;
pub const FLAG_SUBT:u8 = 0b0100_0000;
pub const FLAG_HALF:u8 = 0b0010_0000;
pub const FLAG_CARR:u8 = 0b0001_0000;

pub const OAM_TABLE:u16 = 0xFE00; // Object Attribute Memory
pub const P1  :u16 = 0xFF00; // Pad IO
    pub const P1_RIGHT:u8 = 0b0001_0001; // Joypad right
    pub const P1_LEFT :u8 = 0b0001_0010; // Joypad left
    pub const P1_UP   :u8 = 0b0001_0100; // Joypad up
    pub const P1_DOWN :u8 = 0b0001_1000; // Joypad down
    pub const P1_ABUTT:u8 = 0b0010_0001; // Joypad A button
    pub const P1_BBUTT:u8 = 0b0010_0010; // Joypad B button
    pub const P1_SELEC:u8 = 0b0010_0100; // Joypad Select button
    pub const P1_START:u8 = 0b0010_1000; // Joypad Start button
pub const SB  :u16 = 0xFF01; // Serial transfer data
pub const SC  :u16 = 0xFF02; // Serial IO control
pub const DIV :u16 = 0xFF04; // Divider
pub const TIMA:u16 = 0xFF05; // Timer counter
pub const TMA :u16 = 0xFF06; // Timer modulo
pub const TAC :u16 = 0xFF07; // Timer control
pub const IF  :u16 = 0xFF0F; // Interrupt flag
    pub const IF_VBLNK:u8 = 0b0000_0001; // V-Blank start
    pub const IF_LSTAT:u8 = 0b0000_0010; // LCD status
    pub const IF_TIMER:u8 = 0b0000_0100; // Timer
    pub const IF_SRIAL:u8 = 0b0000_1000; // Serial complete
    pub const IF_JYPAD:u8 = 0b0001_0000; // Joypad event
    pub const IF_ALL  :u8 = 0b0001_1111;
pub const NR10:u16 = 0xFF10; // Sound 1 (sweep)
pub const NR11:u16 = 0xFF11; // Sound 1 (wave pattern duty)
pub const NR12:u16 = 0xFF12; // Sound 1 (envelope)
pub const NR13:u16 = 0xFF13; // Sound 1 (freq low)
pub const NR14:u16 = 0xFF14; // Sound 1 (freq high)
pub const NR21:u16 = 0xFF16; // Sound 2 (wave pattern duty)
pub const NR22:u16 = 0xFF17; // Sound 2 (envelope)
pub const NR23:u16 = 0xFF18; // Sound 2 (freq low)
pub const NR24:u16 = 0xFF19; // Sound 2 (freq high)
pub const NR30:u16 = 0xFF1A; // Sound 3 (on/off)
pub const NR31:u16 = 0xFF1B; // Sound 3 (length)
pub const NR32:u16 = 0xFF1C; // Sound 3 (output level)
pub const NR33:u16 = 0xFF1D; // Sound 3 (freq low)
pub const NR34:u16 = 0xFF1E; // Sound 3 (freq high)
pub const NR41:u16 = 0xFF20; // Sound 4 (length)
pub const NR42:u16 = 0xFF21; // Sound 4 (envelope)
pub const NR43:u16 = 0xFF22; // Sound 4 (polynomial counter)
pub const NR44:u16 = 0xFF23; // Sound 4 (counter/consecutive)
pub const NR50:u16 = 0xFF24; // Channel control
pub const NR51:u16 = 0xFF25; // Sound output terminal
pub const NR52:u16 = 0xFF26; // Sound on/off
pub const LCDC:u16 = 0xFF40; // LCD control
    pub const LCDC_BGP:u8 = 0b0000_0001; // BG/Window Display/Priority
    pub const LCDC_ODE:u8 = 0b0000_0010; // OBJ (Sprite) Display Enable
    pub const LCDC_OSZ:u8 = 0b0000_0100; // OBJ (Sprite) Size
    pub const LCDC_BDS:u8 = 0b0000_1000; // BG Tile Map Display Select
    pub const LCDC_BTS:u8 = 0b0001_0000; // BG & Window Tile Data Select
    pub const LCDC_WDE:u8 = 0b0010_0000; // Window Display Enable
    pub const LCDC_WDS:u8 = 0b0100_0000; // Window Tile Map Display Select
    pub const LCDC_ENA:u8 = 0b1000_0000; // LCD Display Enable
pub const STAT:u16 = 0xFF41; // LCDC status
    pub const STAT_MFH:u8 = 0b0000_0000; // Mode 0: MF = H-Blank
    pub const STAT_MFV:u8 = 0b0000_0001; // Mode 1: MF = V-Blank
    pub const STAT_MFO:u8 = 0b0000_0010; // Mode 2: MF = OAM Search
    pub const STAT_MFT:u8 = 0b0000_0011; // Mode 3: MF = Transfer to LCD
    pub const STAT_COF:u8 = 0b0000_0100; // Coincidence Flag LY/LYC: 0 - different, 1 - equal
    pub const STAT_HBI:u8 = 0b0000_1000; // H-Blank Interrupt
    pub const STAT_VBI:u8 = 0b0001_0000; // V-Blank Interrupt
    pub const STAT_OAM:u8 = 0b0010_0000; // OAM Interrupt
    pub const STAT_COI:u8 = 0b0100_0000; // LYC=LY Coincidence Interrupt
pub const SCY :u16 = 0xFF42; // Scroll Y
pub const SCX :u16 = 0xFF43; // Scroll X
pub const LY  :u16 = 0xFF44; // LCDC Y
pub const LYC :u16 = 0xFF45; // LCDC LY-compare
pub const DMA :u16 = 0xFF46; // DMA transfer & start address
pub const BGP :u16 = 0xFF47; // Background/Window palette data: 00 - white, 01 - lgrey, 10 - dgrey, 11 - black
    pub const BGP_COL1:u8 = 0b0000_0011; // Color 1
    pub const BGP_COL2:u8 = 0b0000_1100; // Color 2
    pub const BGP_COL3:u8 = 0b0011_0000; // Color 3
    pub const BGP_COL4:u8 = 0b1100_0000; // Color 4
pub const OBP0:u16 = 0xFF48; // Object palette 0 data
    pub const OP0_COL2:u8 = 0b0000_1100; // Color 2
    pub const OP0_COL3:u8 = 0b0011_0000; // Color 3
    pub const OP0_COL4:u8 = 0b1100_0000; // Color 4
pub const OBP1:u16 = 0xFF49; // Object palette 1 data
    pub const OP1_COL2:u8 = 0b0000_1100; // Color 2
    pub const OP1_COL3:u8 = 0b0011_0000; // Color 3
    pub const OP1_COL4:u8 = 0b1100_0000; // Color 4
pub const WY  :u16 = 0xFF4A; // Window Y position
pub const WX  :u16 = 0xFF4B; // Window X position
pub const IE  :u16 = 0xFFFF; // Interrupt enable
    pub const IE_VBLNK:u8 = 0b0000_0001; // V-Blank start
    pub const IE_LSTAT:u8 = 0b0000_0010; // LCD Status
    pub const IE_TIMER:u8 = 0b0000_0100; // Timer
    pub const IE_SRIAL:u8 = 0b0000_1000; // Serial complete
    pub const IE_JYPAD:u8 = 0b0001_0000; // Joypad event
    pub const IE_ALL  :u8 = 0b0001_1111;

pub struct LR35902 {
    cpu_registers: Registers,
    pub oam_ram: [u8; 0xA0],
    pub flags: [u8; 0x7F],
    pub ram: [u8; 0x7F],
    pub ie: u8,
    pub pending_cycles: usize
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
    pub fn init() -> Self {
        let mut cpu: LR35902 = LR35902 { 
            cpu_registers:Registers::init(),
            oam_ram:[0; 0xA0],
            flags: [0; 0x7F],
            ram:[0; 0x7F],
            ie:0,
            pending_cycles:0
        };

        cpu.flags[0x10] = 0x80;
        cpu.flags[0x11] = 0x88;
        cpu.flags[0x12] = 0xF3;
        cpu.flags[0x14] = 0xBF;
        cpu.flags[0x16] = 0x3F;
        cpu.flags[0x19] = 0xBF;
        cpu.flags[0x1A] = 0x7F;
        cpu.flags[0x1B] = 0xFF;
        cpu.flags[0x1C] = 0x9F;
        cpu.flags[0x1E] = 0xBF;
        cpu.flags[0x20] = 0xFF;
        cpu.flags[0x23] = 0xBF;
        cpu.flags[0x24] = 0x77;
        cpu.flags[0x25] = 0xF3;
        cpu.flags[0x26] = 0xF1;
        cpu.flags[0x40] = 0x91;
        cpu.flags[0x47] = 0xFC;
        cpu.flags[0x48] = 0xFF;
        cpu.flags[0x49] = 0xFF;

        return cpu;
    }

    ///////////////
    // Interface //
    ///////////////

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
    pub fn get_sp(&self) -> u16 {
        u16::from_le(self.cpu_registers.sp)
    }

    /// Set SP register (value must be native-endian).
    pub fn set_sp(&mut self, val:u16) {
        self.cpu_registers.sp = val.to_le();
    }

    /// Increment SP register.
    pub fn increment_sp(&mut self) {
        let old: u16 = self.get_sp();
        self.set_sp(old.wrapping_add(1));
    }

    /// Decrement SP register.
    pub fn decrement_sp(&mut self) {
        let old: u16 = self.get_sp();
        self.set_sp(old.wrapping_sub(1));
    }

    /// Get PC register in little-endian format.
    pub fn get_pc_le(&self) -> u16 {
        self.cpu_registers.pc
    }

    /// Get PC registers in native-endian format.
    pub fn get_pc(&self) -> u16 {
        u16::from_le(self.cpu_registers.pc)
    }

    /// Set SP register (value must be native-endian).
    pub fn set_pc(&mut self, val:u16) {
        self.cpu_registers.pc = val.to_le();
    }

    /// Get current PC in native-endian, then advance.
    /// Equivalent to something like cpu->pc++ in C.
    pub fn get_pc_advance(&mut self) -> u16 {
        let ret: u16 = self.get_pc();

        self.set_pc(ret+1);

        return ret;
    }

    /// Gets Zero flag
    pub fn get_zero_flag(&mut self) -> u8 {
        self.get_f() & FLAG_ZERO
    }

    /// Gets Subtract flag
    pub fn get_subt_flag(&mut self) -> u8 {
        self.get_f() & FLAG_SUBT
    }

    /// Gets Half-carry flag
    pub fn get_half_flag(&mut self) -> u8 {
        self.get_f() & FLAG_HALF
    }

    /// Gets Carry flag
    pub fn get_carr_flag(&mut self) -> u8 {
        self.get_f() & FLAG_CARR
    }

    /// Sets Zero flag
    pub fn set_zero_flag(&mut self) {
        let old: u8 = self.get_f();
        self.set_f(old | FLAG_ZERO);
    }

    /// Sets Subtract flag
    pub fn set_subt_flag(&mut self) {
        let old: u8 = self.get_f();
        self.set_f(old | FLAG_SUBT);
    }

    /// Sets Half-carry flag
    pub fn set_half_flag(&mut self) {
        let old: u8 = self.get_f();
        self.set_f(old | FLAG_HALF);
    }

    /// Sets Carry flag
    pub fn set_carr_flag(&mut self) {
        let old: u8 = self.get_f();
        self.set_f(old | FLAG_CARR);
    }

    /// Resets Zero flag
    pub fn reset_zero_flag(&mut self) {
        let old: u8 = self.get_f();
        self.set_f(old & !FLAG_ZERO);
    }

    /// Resets Subtract flag
    pub fn reset_subt_flag(&mut self) {
        let old: u8 = self.get_f();
        self.set_f(old & !FLAG_SUBT);
    }

    /// Resets Half-carry flag
    pub fn reset_half_flag(&mut self) {
        let old: u8 = self.get_f();
        self.set_f(old & !FLAG_HALF);
    }

    /// Resets Carry flag
    pub fn reset_carr_flag(&mut self) {
        let old: u8 = self.get_f();
        self.set_f(old & !FLAG_CARR);
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
            assert_eq!(MERGE_U8!(reg.split[1], reg.split[0]), 0xFF00u16);
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

    /// Test for init_cpu function and register methods.
    #[test]
    fn registers() {
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
        assert_eq!(cpu.get_sp(), 0xFFFEu16);
        assert_eq!(cpu.get_pc_le(), (0x0001u16).to_le());
        assert_eq!(cpu.get_pc(), 0x0001u16);

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
        assert_eq!(cpu.get_pc(), 0xDEADu16);

        cpu.set_sp(0xDEADu16);
        assert_eq!(cpu.get_sp(), 0xDEADu16);
    }

    #[test]
    fn get_pc_advance_test() {
        let mut cpu:LR35902 = LR35902::init();
        let pc: u16 = cpu.get_pc_advance();

        assert_eq!(pc, 0x0001u16);
        assert_eq!(cpu.get_pc(), 0x0002u16);
    }
}