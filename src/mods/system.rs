use crate::cpu;
use crate::memory;

pub struct System {
    gb_cpu:cpu::CPU,
    gb_memory:memory::MemMap
}
