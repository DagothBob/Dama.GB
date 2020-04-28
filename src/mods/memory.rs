
pub struct MemMap {
    mem:[u8;0x10000]
}

impl MemMap {
    pub fn init() -> MemMap {
        MemMap {
            mem:[0; 0x10000]
        }
    }
}
