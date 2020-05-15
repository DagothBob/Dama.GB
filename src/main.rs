#![allow(non_camel_case_types)]

#[path = "./mods/system.rs"] mod system;
#[path = "./mods/cpu.rs"] mod cpu;
#[path = "./mods/memory.rs"] mod memory;
#[path = "./mods/display.rs"] mod display;

use std::env;
use std::fs::File;
use std::path::Path;
use std::vec::Vec;

use system::System;

fn main() {
    let args:Vec<String> = env::args().collect();
    let file_path = Path::new(&args[1]);
    let file = match File::open(file_path) {
        Ok(f) => f,
        Err(e) => panic!("Failed to open file: {}", e)
    };
    let mut gb_system = System::init(file);

    gb_system.system_loop()
}