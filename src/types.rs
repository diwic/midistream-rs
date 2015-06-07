use std::convert::From;
use std::ops::Deref;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct U5(u8);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct U7(u8);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct U14(u16);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Channel(u8);

impl From<u8> for U7 {
    fn from(i: u8) -> U7 {
        debug_assert!(i & 0x80 == 0);
        U7(i & 0x7f)
    }
}

impl Deref for U7 {
    type Target = u8;

    #[inline]
    fn deref(&self) -> &u8 { &self.0 }
}

impl From<u8> for U5 {
    fn from(i: u8) -> U5 {
        debug_assert!(i & 0xe0 == 0);
        U5(i & 0x1f)
    }
}

impl Deref for U5 {
    type Target = u8;

    #[inline]
    fn deref(&self) -> &u8 { &self.0 }
}

impl From<u16> for U14 {
    fn from(i: u16) -> U14 {
        debug_assert!(i & 0xc000 == 0);
        U14(i & 0x3fff)
    }
}

impl From<(U7, U7)> for U14 {
    fn from(i: (U7, U7)) -> U14 {
        U14(*i.0 as u16 + ((*i.1 as u16) << 7))
    }
}

impl From<U14> for (U7, U7) {
    fn from(i: U14) -> (U7, U7) {
        (i.lsb(), i.msb())
    }
}

impl U14 {
    #[inline]
    pub fn lsb(&self) -> U7 { U7((self.0 & 0x7f) as u8) }
    #[inline]
    pub fn msb(&self) -> U7 { U7((self.0 >> 7) as u8) }
}

impl Deref for U14 {
    type Target = u16;

    #[inline]
    fn deref(&self) -> &u16 { &self.0 }
}

impl From<u8> for Channel {
    fn from(i: u8) -> Channel {
        debug_assert!(i & 0xf0 == 0);
        Channel(i & 0x0f)
    }
}

impl Deref for Channel {
    type Target = u8;

    #[inline]
    fn deref(&self) -> &u8 { &self.0 }
}

