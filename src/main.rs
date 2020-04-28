#[path = "./mods/system.rs"] mod system;
#[path = "./mods/cpu.rs"] mod cpu;
#[path = "./mods/memory.rs"] mod memory;

use system::System;

fn main() {
    let mut gb_system = System::init();
}