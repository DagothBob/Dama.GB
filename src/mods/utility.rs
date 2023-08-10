/// Concatenate two bytes as a u16.
/// First arg is most significant, second arg is least.
/// 
/// Big Endian is superior. #MotorolaWasRobbed
#[macro_export]
macro_rules! MERGE_U8 {
    ($up:expr, $dn:expr) => (u16::from_be_bytes([$up, $dn]));
}

/// Split a u16 into bytes (big-endian order)
#[macro_export]
macro_rules! SPLIT_U16 {
    ($word:expr) => ($word.to_be_bytes());
}

////////////////
// Unit Tests //
////////////////

#[cfg(test)]
mod tests {
    /// Test MERGE_U8 macro.
    #[test]
    fn merge() {
        let up: u8 = 0xFF;
        let down: u8 = 0x11;
        let merge: u16 = MERGE_U8!(up, down);

        assert_eq!(merge, 0xFF11);
    }

    /// Test SPLIT_U16 macro.
    #[test]
    fn split() {
        let word: u16 = 0xFF11;
        let split: [u8; 2] = SPLIT_U16!(word);

        assert_eq!(split[0], 0xFF);
        assert_eq!(split[1], 0x11);
    }
}