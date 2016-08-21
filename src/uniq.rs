use std::fmt;

/// A unique number given to identifiers.
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct Uniq(u32);

// Uniques are 32-bit wide. First 7-bit is used as an ascii character when
// showing.

const UNIQ_MASK : u32 = 0x01FFFFFF;

impl fmt::Debug for Uniq {
    fn fmt(&self, f : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}_{}", ((self.0 >> 25) as u8) as char, self.0 & UNIQ_MASK)
    }
}

pub fn bump_uniq(uniq : Uniq) -> Uniq {
    // check for overflows
    debug_assert!(uniq.0 & UNIQ_MASK < UNIQ_MASK);
    Uniq(uniq.0 + 1)
}

/// Create a uniq counter for a given tag. A tag should be used once, and is
/// 7-bit wide! Use with caution.
pub fn init_uniq(tag : u8) -> Uniq {
    debug_assert!(tag <= 0b01111111);
    // TODO: assert that tag is not used before! bugs would be very hard to find
    Uniq((tag as u32) << 25)
}

#[test]
fn uniq_test_1() {
    let uniq = init_uniq(b'l');
    assert_eq!("l_0", format!("{:?}", uniq));
    assert_eq!("l_1", format!("{:?}", bump_uniq(uniq)));
}
