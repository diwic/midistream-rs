use super::types::*;
use std::io::{Read, Write};

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

impl SimpleMsg {
    pub fn encode<W: Write>(&self, w: &mut W) -> ::std::io::Result<()> {
        use self::SimpleMsg::*;
        match self {
            &NoteOff(ref n) => w.write_all(&[0x80 + *n.channel, *n.note, *n.value]),
            &NoteOn(ref n) => w.write_all(&[0x90 + *n.channel, *n.note, *n.value]),
            &PolyKeyPressure(ref n) => w.write_all(&[0xa0 + *n.channel, *n.note, *n.value]),
            &ControlChange(ref n) => w.write_all(&[0xb0 + *n.channel, *n.control, *n.value]),
            &ProgramChange(ref n) => w.write_all(&[0xc0 + *n.channel, *n.value]),
            &ChannelKeyPressure(ref n) => w.write_all(&[0xd0 + *n.channel, *n.value]),
            &PitchBendChange(ref n) => w.write_all(&[0xe0 + *n.channel, *n.value.lsb(), *n.value.msb()]),

            &MTCQuarterFrame(ref n) => w.write_all(&[0xf1, **n]),
            &SongPositionPointer(ref n) => w.write_all(&[0xf2, *n.lsb(), *n.msb()]),
            &SongSelect(ref n) => w.write_all(&[0xf3, **n]),
            &TuneRequest => w.write_all(&[0xf6]),
            &TimingClock => w.write_all(&[0xf8]),
            &Start => w.write_all(&[0xfa]),
            &Continue => w.write_all(&[0xfb]),
            &Stop => w.write_all(&[0xfc]),
            &ActiveSensing => w.write_all(&[0xfe]),
            &SystemReset => w.write_all(&[0xff]),
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
        self.buflen = {
            let mut c = ::std::io::Cursor::new(&mut self.buf[..]);
            i.encode(&mut c).unwrap();
            c.position() as usize
        };

        if self.buflen == 0 || self.running_status.is_none() { return }
        if self.running_status == Some(Some(self.buf[0])) { self.bufpos = 1 }
        self.running_status = Some(match self.buf[0] {
            0x80...0xef => Some(self.buf[0]),
            _ => None,
        });
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
        let mut v: Vec<u8> = vec!();
        n.encode(&mut v).unwrap();
        assert_eq!(v, &[0xe5, 0x00, 0x40]);
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
}
