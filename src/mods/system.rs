use crate::cpu;
use crate::memory;

pub struct System {
    gb_cpu:cpu::CPU,
    gb_memory:memory::MemMap
}

impl System {
    pub fn init() -> System {
        System {
            gb_cpu:cpu::CPU::init(),
            gb_memory:memory::MemMap::init()
        }
    }
}
