#![allow(non_camel_case_types)]

#[path = "./mods/system.rs"] mod system;
#[path = "./mods/cpu.rs"] mod cpu;
#[path = "./mods/memory.rs"] mod memory;

use system::System;

fn main() {
    let mut gb_system = System::init();

    gb_system.system_loop()
}