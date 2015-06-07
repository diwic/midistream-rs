use super::types::*;
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Note {
    pub channel: Channel,
    pub note: U7,
    pub value: U7,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Control {
    pub channel: Channel,
    pub control: U7,
    pub value: U7,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Control14 {
    pub channel: Channel,
    pub control: U5,
    pub value: U14,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct ChannelValue {
    pub channel: Channel,
    pub value: U7,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct PitchBend {
    pub channel: Channel,
    pub value: U14,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Parameter {
    pub channel: Channel,
    pub parameter: U14,
    pub value: U14,
}

/// A standard, short MIDI message.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum SimpleMsg {
    // Names are same as http://www.midi.org/dtds/MIDIEvents10.dtd.html

    NoteOff(Note),
    NoteOn(Note),
    PolyKeyPressure(Note),
    ControlChange(Control),
    ProgramChange(ChannelValue),
    ChannelKeyPressure(ChannelValue),
    PitchBendChange(PitchBend),

    MTCQuarterFrame(U7),
    SongPositionPointer(U14),
    SongSelect(U7),
    TuneRequest,
    TimingClock,
    Start,
    Continue,
    Stop,
    ActiveSensing,
    SystemReset,
}

/// These message encode into more than one SimpleMsg.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum ComplexMsg {
    ControlChange14(Control14),
    RPNChange(Parameter),
    NRPNChange(Parameter),
}

pub const SYSEXMSG_LEN: usize = 128;

/// Sysex messages can be of any length. To ensure no
/// memory allocation, messages longer than SYSEXMSG_LEN will
/// be split up in more than one SysexMsg.
pub struct SysexMsg {
    buf: [u8; SYSEXMSG_LEN], // Buf includes 0xf0 and 0xf7 bytes, for efficiency
    len: usize,
}

#[derive(Debug, Clone)]
pub enum Msg {
    Simple(SimpleMsg),
    Complex(ComplexMsg),
    Sysex(SysexMsg),
}

impl SimpleMsg {
    pub fn encode<R, F: FnOnce(&[u8]) -> R>(&self, f: F) -> R {
        use self::SimpleMsg::*;
        match self {
            &NoteOff(ref n) => f(&[0x80 + *n.channel, *n.note, *n.value]),
            &NoteOn(ref n) => f(&[0x90 + *n.channel, *n.note, *n.value]),
            &PolyKeyPressure(ref n) => f(&[0xa0 + *n.channel, *n.note, *n.value]),
            &ControlChange(ref n) => f(&[0xb0 + *n.channel, *n.control, *n.value]),
            &ProgramChange(ref n) => f(&[0xc0 + *n.channel, *n.value]),
            &ChannelKeyPressure(ref n) => f(&[0xd0 + *n.channel, *n.value]),
            &PitchBendChange(ref n) => f(&[0xe0 + *n.channel, *n.value.lsb(), *n.value.msb()]),

            &MTCQuarterFrame(ref n) => f(&[0xf1, **n]),
            &SongPositionPointer(ref n) => f(&[0xf2, *n.lsb(), *n.msb()]),
            &SongSelect(ref n) => f(&[0xf3, **n]),
            &TuneRequest => f(&[0xf6]),
            &TimingClock => f(&[0xf8]),
            &Start => f(&[0xfa]),
            &Continue => f(&[0xfb]),
            &Stop => f(&[0xfc]),
            &ActiveSensing => f(&[0xfe]),
            &SystemReset => f(&[0xff]),
        }
    }

    fn control(c: Channel, z: u8, v: u8) -> SimpleMsg {
        SimpleMsg::ControlChange(Control { channel: c, control: z.into(), value: v.into() })
    }

    pub fn all_sound_off(c: Channel) -> SimpleMsg { SimpleMsg::control(c, 0x78, 0) }
    pub fn reset_all_controllers(c: Channel) -> SimpleMsg { SimpleMsg::control(c, 0x79, 0) }
    pub fn local_control(c: Channel, value: bool) -> SimpleMsg {
        SimpleMsg::control(c, 0x7a, if value { 0x7f } else { 0 })
    }
    pub fn all_notes_off(c: Channel) -> SimpleMsg { SimpleMsg::control(c, 0x7b, 0) }
    pub fn omni_off(c: Channel) -> SimpleMsg { SimpleMsg::control(c, 0x7c, 0) }
    pub fn omni_on(c: Channel) -> SimpleMsg { SimpleMsg::control(c, 0x7d, 0) }
    pub fn mono_mode(c: Channel, value: U7) -> SimpleMsg { SimpleMsg::control(c, 0x7e, *value) }
    pub fn poly_mode(c: Channel) -> SimpleMsg { SimpleMsg::control(c, 0x7f, 0) }
}

impl ComplexMsg {
    pub fn encode<R, F: FnOnce(&[SimpleMsg]) -> R>(&self, f: F) -> R {
        use self::ComplexMsg::*;
        match self {
            &ControlChange14(ref n) => f(&[
                 SimpleMsg::control(n.channel, *n.control, *n.value.msb()),
                 SimpleMsg::control(n.channel, (*n.control + 0x20).into(), *n.value.lsb()),
            ]),
            &RPNChange(ref n) => f(&[
                 SimpleMsg::control(n.channel, 0x65, *n.parameter.msb()),
                 SimpleMsg::control(n.channel, 0x64, *n.parameter.lsb()),
                 SimpleMsg::control(n.channel, 0x6, *n.value.msb()),
                 SimpleMsg::control(n.channel, 0x26, *n.value.lsb()),
            ]),
            &NRPNChange(ref n) => f(&[
                 SimpleMsg::control(n.channel, 0x63, *n.parameter.msb()),
                 SimpleMsg::control(n.channel, 0x62, *n.parameter.lsb()),
                 SimpleMsg::control(n.channel, 0x6, *n.value.msb()),
                 SimpleMsg::control(n.channel, 0x26, *n.value.lsb()),
            ]),
        }
    }
}

impl SysexMsg {
    // Assumes b <= SYSEXMSG_LEN-2, will panic otherwise
    pub fn new_short(b: &[U7]) -> SysexMsg {
        let mut z = SysexMsg { buf: [0xf0; SYSEXMSG_LEN], len: 1 };
        for a in (&mut z.buf[1..]).iter_mut().zip(b) { *a.0 = **a.1; z.len += 1 };
        z.buf[z.len] = 0xf7;
        z.len += 1;
        z
    }

    pub fn encode<R, F: FnOnce(&[u8]) -> R>(&self, f: F) -> R {
        f(&self.buf[..self.len])
    }

    pub fn mmc_command(device: U7, command: U7) -> SysexMsg {
        SysexMsg::new_short(&[0x7f.into(), device, 0x06.into(), command])
    }
}

impl Clone for SysexMsg {
    fn clone(&self) -> SysexMsg {
        SysexMsg { buf: self.buf, len: self.len }
    }
}

impl fmt::Debug for SysexMsg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "SysexMsg(0x{:x}", self.buf[0]));
        for a in 1..self.len {
            try!(write!(f, ",0x{:x}", self.buf[a]));
        }
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct SimpleMsgEncoder<I: Iterator<Item=SimpleMsg>> {
    source: I,
    running_status: Option<Option<u8>>,
    buf: [u8; 3],
    bufpos: usize,
    buflen: usize,
}

impl<I: Iterator<Item=SimpleMsg>> SimpleMsgEncoder<I> {
    pub fn new(i: I, use_running_status: bool) -> SimpleMsgEncoder<I> {
        SimpleMsgEncoder {
            source: i,
            running_status: if use_running_status { Some(None) } else { None },
            buf: [0, 0, 0],
            buflen: 0,
            bufpos: 0,
        }
    }

    pub fn drop_running_status(&mut self) {
        if self.running_status.is_some() {
           self.running_status = Some(None);
        }
    }
}

impl<I: Iterator<Item=SimpleMsg>> Iterator for SimpleMsgEncoder<I> {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        self.from_buf().or_else(|| {
            self.to_buf();
            self.from_buf()
        })
    }
}

impl<I: Iterator<Item=SimpleMsg>> SimpleMsgEncoder<I> {
    fn from_buf(&mut self) -> Option<u8> {
        if self.bufpos >= self.buflen { return None; }
        let z = self.buf[self.bufpos];
        self.bufpos += 1;
        Some(z)
    }

    fn to_buf(&mut self) {
        let i = if let Some(i) = self.source.next() { i } else { return };
        self.bufpos = 0;
        self.buflen = 0;
        i.encode(|b| for a in self.buf.iter_mut().zip(b) {
            *a.0 = *a.1;
            self.buflen += 1
        });
        if self.buflen == 0 || self.running_status.is_none() { return }
        if self.running_status == Some(Some(self.buf[0])) { self.bufpos = 1 }
        self.running_status = Some(match self.buf[0] {
            0x80...0xef => Some(self.buf[0]),
            _ => None,
        });
    }
}

struct MsgEncoderHelper([Option<SimpleMsg>; 4], usize);

pub struct MsgEncoder<I: Iterator<Item=Msg>> {
    source: I,
    simple: SimpleMsgEncoder<MsgEncoderHelper>,
    buf: [u8; SYSEXMSG_LEN],
    bufpos: usize,
    buflen: usize,
}

impl<I: Iterator<Item=Msg>> MsgEncoder<I> {
    pub fn new(i: I, use_running_status: bool) -> MsgEncoder<I> {
        MsgEncoder {
            source: i,
            simple: SimpleMsgEncoder::new(MsgEncoderHelper([None; 4], 0), use_running_status),
            buf: [0; SYSEXMSG_LEN],
            buflen: 0,
            bufpos: 0,
        }
    }
}

impl Iterator for MsgEncoderHelper {
    type Item = SimpleMsg;
    fn next(&mut self) -> Option<SimpleMsg> {
        if self.1 >= self.0.len() { None } else {
            self.1 += 1;
            self.0[self.1 - 1].take()
        }
    }
}

impl<I: Iterator<Item=Msg>> Iterator for MsgEncoder<I> {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        self.from_buf().or_else(|| {
            self.to_buf();
            self.from_buf()
        })
    }
}

impl<I: Iterator<Item=Msg>> MsgEncoder<I> {
    fn from_buf(&mut self) -> Option<u8> {
        if self.bufpos >= self.buflen { return self.simple.next(); }
        let z = self.buf[self.bufpos];
        self.bufpos += 1;
        Some(z)
    }

    fn copy<'a, B: Iterator<Item=&'a u8>>(&mut self, b: B) {
        for a in self.buf[self.bufpos..].iter_mut().zip(b) {
            *a.0 = *a.1;
            self.buflen += 1
        }
    }

    fn to_buf(&mut self) {
        let i = if let Some(i) = self.source.next() { i } else { return };
        self.bufpos = 0;
        self.buflen = 0;
        match i {
            Msg::Sysex(n) => {
                self.simple.drop_running_status();
                n.encode(|b| self.copy(b.iter()));
            },
            Msg::Complex(n) => n.encode(|s| {
                let mut f = s.iter().map(|x| *x);
                self.simple.source = MsgEncoderHelper([f.next(), f.next(), f.next(), f.next()], 0);
            }),
            Msg::Simple(n) => self.simple.source = MsgEncoderHelper([Some(n), None, None, None], 0),
        }
    }
}


#[derive(Debug)]
pub struct SimpleMsgDecoder<I: Iterator<Item=u8>> {
    source: I,
    running_status: Option<u8>,
    lost_status: Option<u8>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum MidiDecoderError {
    UnknownData(u8),
    Malformed,
    UnexpectedEOF,
}

impl<I: Iterator<Item=u8>> SimpleMsgDecoder<I> {

    fn read_u8(&mut self) -> Result<u8, MidiDecoderError> {
        (self.lost_status.take().or_else(|| self.source.next())
            .ok_or(MidiDecoderError::UnexpectedEOF))
    }

    fn current_channel(&self) -> Channel {
        ((self.running_status.unwrap() & 0xf) as u8).into()
    }

    fn read_u7(&mut self) -> Result<U7, MidiDecoderError> {
        match try!(self.read_u8()) {
            i @ 0x00...0x7f => Ok(i.into()),
            i @ _ => { self.running_status = None; self.lost_status = Some(i); Err(MidiDecoderError::Malformed) }
        }
    }

    fn read_note(&mut self) -> Result<Note, MidiDecoderError> {
        Ok(Note {
            channel: self.current_channel(),
            note: try!(self.read_u7()),
            value: try!(self.read_u7()),
        })
    }

    fn read_channelvalue(&mut self) -> Result<ChannelValue, MidiDecoderError> {
        Ok(ChannelValue {
            channel: self.current_channel(),
            value: try!(self.read_u7()),
        })
    }

    fn read_msg(&mut self, s: u8) -> Result<SimpleMsg, MidiDecoderError> {
        use self::SimpleMsg::*;
        match s {
            0x00...0x7f => return self.running_status.ok_or(MidiDecoderError::UnknownData(s))
                .and_then(|s| self.read_msg(s)),
            0x80...0xef => self.running_status = Some(s),
            0xf0...0xff => self.running_status = None,
            _ => unreachable!(),
        };
        Ok(match s {
            0x80...0x8f => NoteOff(try!(self.read_note())),
            0x90...0x9f => NoteOn(try!(self.read_note())),
            0xa0...0xaf => PolyKeyPressure(try!(self.read_note())),
            0xb0...0xbf => ControlChange(Control {
                channel: self.current_channel(),
                control: try!(self.read_u7()),
                value: try!(self.read_u7()),
            }),
            0xc0...0xcf => ProgramChange(try!(self.read_channelvalue())),
            0xd0...0xdf => ChannelKeyPressure(try!(self.read_channelvalue())),
            0xe0...0xef => PitchBendChange(PitchBend {
                channel: self.current_channel(),
                value: (try!(self.read_u7()), try!(self.read_u7())).into(),
            }),
            0xf1 => MTCQuarterFrame(try!(self.read_u7())),
            0xf2 => SongPositionPointer((try!(self.read_u7()), try!(self.read_u7())).into()),
            0xf3 => SongSelect(try!(self.read_u7())),
            0xf6 => TuneRequest,
            0xf8 => TimingClock,
            0xfa => Start,
            0xfb => Continue,
            0xfc => Stop,
            0xfe => ActiveSensing,
            0xff => SystemReset,
            _ => return Err(MidiDecoderError::UnknownData(s)),
        })
    }

    pub fn new(i: I) -> SimpleMsgDecoder<I> {
        SimpleMsgDecoder { source: i, running_status: None, lost_status: None }
    }
}

impl<I: Iterator<Item=u8>> Iterator for SimpleMsgDecoder<I> {
    type Item = Result<SimpleMsg, MidiDecoderError>; // For now (to handle more complex stuff later)

    fn next(&mut self) -> Option<Self::Item> {
        match self.read_u8() {
            Err(MidiDecoderError::UnexpectedEOF) => None,
            Err(e) => Some(Err(e)),
            Ok(b) => Some(self.read_msg(b)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_decode_noteon() {
        let n = [0x93u8, 0x40, 0x64];
        let d = SimpleMsgDecoder::new(n.iter().map(|x| *x));
        let events: Vec<SimpleMsg> = d.map(|e| e.unwrap()).collect();
        assert_eq!(&events, &[SimpleMsg::NoteOn(Note { channel: 3.into(), note: 0x40.into(), value: 0x64.into() })]);
    }

    #[test]
    fn test_encode_pitchbend() {
        let n = SimpleMsg::PitchBendChange(PitchBend { channel: 5.into(), value: 8192.into() });
        n.encode(|v| assert_eq!(v, &[0xe5, 0x00, 0x40]));
    }

    #[test]
    fn test_sysex() {
        let n = SysexMsg::mmc_command(0x7f.into(), 0x01.into());
        // println!("{:?}", n);
        n.encode(|v| assert_eq!(v, &[0xf0, 0x7f, 0x7f, 0x06, 0x01, 0xf7]));
    }

    #[test]
    fn test_encode_stream() {
        let n = [
            SimpleMsg::PitchBendChange(PitchBend { channel: 3.into(), value: 8192.into() }),
            SimpleMsg::NoteOn(Note { channel: 3.into(), note: 0x60.into(), value: 0x64.into() }),
            SimpleMsg::NoteOn(Note { channel: 3.into(), note: 0x60.into(), value: 0x00.into() })];
        let enc = SimpleMsgEncoder::new(n.iter().map(|x| *x), true);
        let v: Vec<u8> = enc.collect();
        assert_eq!(v, &[0xe3, 0x00, 0x40, 0x93, 0x60, 0x64, 0x60, 0x00]);
    }

    #[test]
    fn test_stream() {
        let n = [
            Msg::Simple(SimpleMsg::PitchBendChange(PitchBend { channel: 3.into(), value: 8192.into() })),
            Msg::Simple(SimpleMsg::NoteOn(Note { channel: 3.into(), note: 0x60.into(), value: 0x64.into() })),
            Msg::Simple(SimpleMsg::NoteOn(Note { channel: 3.into(), note: 0x60.into(), value: 0x00.into() })),
            Msg::Complex(ComplexMsg::ControlChange14(Control14 { channel: 3.into(), control: 7.into(), value: 0x2000.into() })),
        ];

        let enc = MsgEncoder::new(n.iter().map(|x| x.clone()), true);
        let v: Vec<u8> = enc.collect();
        assert_eq!(v, &[
            0xe3, 0x00, 0x40,
            0x93, 0x60, 0x64,
            0x60, 0x00,
            0xb3, 0x07, 0x40,
            0x27, 0x00,
        ]);
    }
}
