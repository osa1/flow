use std::fmt;
use std;

/// A unique number given to identifiers.
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub struct Uniq(u32);

pub const ENTRY_UNIQ : Uniq = Uniq(0);
pub const VARARG_UNIQ: Uniq = Uniq(1);

// Uniques are 32-bit wide. First 7-bit is used as an ascii character when
// showing.

const UNIQ_MASK : u32 = 0x01FFFFFF;

fn fmt_tagged_uniq(u : u32, f : &mut fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
    write!(f, "{}_{}", ((u >> 25) as u8) as char, u & UNIQ_MASK)
}

impl fmt::Debug for Uniq {
    fn fmt(&self, f : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_tagged_uniq(self.0, f)
    }
}

/// Uniq generator.
pub struct UniqCounter(u32);

impl fmt::Debug for UniqCounter {
    fn fmt(&self, f : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_tagged_uniq(self.0, f)
    }
}

impl UniqCounter {
    /// Create a uniq counter for a given tag. A tag should be used once, and is
    /// 7-bit wide! Use with caution.
    pub fn new(tag : u8) -> UniqCounter {
        debug_assert!(tag <= 0b01111111);
        // TODO: assert that tag is not used before! bugs would be very hard to find
        UniqCounter((tag as u32) << 25)
    }

    pub fn fresh(&mut self) -> Uniq {
        // check for overflows
        debug_assert!(self.0 & UNIQ_MASK < UNIQ_MASK);
        let ret = Uniq(self.0 + 1);
        self.0 += 1;
        ret
    }
}

#[test]
fn uniq_test_1() {
    let mut uniq = UniqCounter::new(b'l');
    assert_eq!("l_0", format!("{:?}", uniq));
    assert_eq!("l_1", format!("{:?}", uniq.fresh()));
}
