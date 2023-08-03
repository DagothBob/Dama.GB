/// Concatenate two bytes as a u16.
/// First arg is most significant, second arg is least.
/// 
/// Big Endian is superior. #MotorolaWasRobbed
macro_rules! merge_u8 {
    ($up:expr, $dn:expr) => (u16::from_be(((($up as u16) << 8) | ($dn as u16)) as u16));
}