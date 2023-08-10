use gb::GB;

#[path = "./mods/gb.rs"] mod gb;
#[path = "./mods/gb_cpu.rs"] mod gb_cpu;
#[path = "./mods/aux_ram.rs"] mod aux_ram;
#[path = "./mods/gamepak.rs"] mod gamepak;
#[path = "./mods/utility.rs"] mod utility;

fn main() {
    let mut gb: GB = GB::init();
}