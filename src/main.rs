use emu::GBEmu;

#[path = "./mods/emu.rs"] mod emu;
#[path = "./mods/gb.rs"] mod gb;
#[path = "./mods/cpu.rs"] mod cpu;
#[path = "./mods/aux_ram.rs"] mod aux_ram;
#[path = "./mods/gamepak.rs"] mod gamepak;
#[path = "./mods/graphics.rs"] mod graphics;
#[path = "./mods/peripheral.rs"] mod peripheral;
#[path = "./mods/utility.rs"] mod utility;

fn main() {
    let mut emu: GBEmu = GBEmu::init();
    emu.run();
}