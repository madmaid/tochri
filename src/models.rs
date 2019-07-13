use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Default, Debug, PartialEq, Clone, Copy)]
pub struct MSF(pub u32, pub u32, pub u32);

#[allow(dead_code)]
impl MSF {
    pub fn as_frame(&self) -> u32 {
        //(self.minutes * 60 + self.seconds) * 75 + self.frames
        (self.0 * 60 + self.1) * 75 + self.2
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CDType {
    #[allow(non_camel_case_types)]
    CD_DA,
    #[allow(non_camel_case_types)]
    CD_ROM,
    #[allow(non_camel_case_types)]
    CD_ROM_XA,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ISRC {
    pub country: String,
    pub owner: String,
    pub year: String,
    pub serial: String,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FIFOLength {
    MSF(MSF),
    Byte(u32),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FIFO {
    pub path: PathBuf,
    pub length: FIFOLength,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TrackModeType {
    Audio,
    ModeOne,
    ModeOneRaw,
    ModeTwo,
    ModeTwoFormOne,
    ModeTwoFormTwo,
    ModeTwoFormMix,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Sequence {
    File {
        filename: PathBuf,
        start: MSF,
        length: Option<MSF>,
    },
    DataFile {
        filename: PathBuf,
        length: Option<MSF>,
    },
    Pregap {
        length: MSF,
    },
    Zero {
        length: MSF,
    },
    Start,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LanguageValue {
    Binary(Vec<u8>),
    String(String),
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct CdText {
    pub language_map: HashMap<u8, String>,
    pub language: HashMap<u8, HashMap<String, LanguageValue>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Track {
    Audio {
        mode: TrackMode,

        sequences: Vec<Sequence>,
        copy: bool,
        pre_emphasis: bool,
        channels: u8,
        isrc: Option<ISRC>,
        text: Option<CdText>,
        indices: Vec<MSF>,
    },
    Data {
        mode: TrackMode,
        sequences: Vec<Sequence>,
        copy: bool,
        text: Option<CdText>,
        fifo: Option<FIFO>,
        indices: Vec<MSF>,
    },
}

#[derive(Debug, Default)]
pub(crate) struct AudioTrackBuilder<TrackModeState> {
    mode: TrackModeState,
    sequences: Vec<Sequence>,
    copy: bool,
    pre_emphasis: bool,

    channels: u8,
    isrc: Option<ISRC>,
    text: Option<CdText>,
    indices: Vec<MSF>,
}

impl AudioTrackBuilder<()> {
    pub fn new() -> Self {
        AudioTrackBuilder {
            mode: (),
            channels: 2, // as default
            ..Default::default()
        }
    }
}

impl AudioTrackBuilder<TrackMode> {
    pub fn build(self) -> Track {
        Track::Audio {
            mode: self.mode,
            sequences: self.sequences,
            copy: self.copy,
            pre_emphasis: self.pre_emphasis,
            channels: self.channels,
            isrc: self.isrc,
            text: self.text,
            indices: self.indices,
        }
    }
}

impl<TrackModeState> AudioTrackBuilder<TrackModeState> {
    pub fn mode(self, mode: TrackMode) -> AudioTrackBuilder<TrackMode> {
        AudioTrackBuilder {
            mode: mode,
            sequences: self.sequences,
            copy: self.copy,
            pre_emphasis: self.pre_emphasis,
            channels: self.channels,
            isrc: self.isrc,
            text: self.text,
            indices: self.indices,
        }
    }
}
impl AudioTrackBuilder<TrackMode> {
    pub fn sequences(mut self, seqs: Vec<Sequence>) -> Self {
        self.sequences = seqs;
        self
    }
    pub fn copy(&mut self, copy: bool) -> &mut Self {
        self.copy = copy;
        self
    }
    pub fn pre_emphasis(&mut self, pre_emphasis: bool) -> &mut Self {
        self.pre_emphasis = pre_emphasis;
        self
    }
    pub fn channels(&mut self, channels: u8) -> &mut Self {
        self.channels = channels;
        self
    }

    pub fn isrc(&mut self, isrc: Option<ISRC>) -> &mut Self {
        self.isrc = isrc;
        self
    }

    pub fn text(&mut self, text: Option<CdText>) -> &mut Self {
        self.text = text;
        self
    }

    pub fn indices(mut self, indices: Vec<MSF>) -> Self {
        self.indices = indices;
        self
    }
}

#[derive(Debug, Default)]
pub(crate) struct DataTrackBuilder<TrackModeState> {
    mode: TrackModeState,
    sequences: Vec<Sequence>,
    copy: bool,
    fifo: Option<FIFO>,
    text: Option<CdText>,
    indices: Vec<MSF>,
}

impl DataTrackBuilder<()> {
    pub fn new() -> Self {
        DataTrackBuilder {
            mode: (),
            ..Default::default()
        }
    }
}

impl DataTrackBuilder<TrackMode> {
    pub fn build(self) -> Track {
        Track::Data {
            mode: self.mode,
            sequences: self.sequences,
            copy: self.copy,
            fifo: self.fifo,
            text: self.text,
            indices: self.indices,
        }
    }
}

impl<TrackModeState> DataTrackBuilder<TrackModeState> {
    pub fn mode(self, mode: TrackMode) -> DataTrackBuilder<TrackMode> {
        DataTrackBuilder {
            mode: mode,
            sequences: self.sequences,
            copy: self.copy,
            fifo: self.fifo,
            text: self.text,
            indices: self.indices,
        }
    }
}
impl DataTrackBuilder<TrackMode> {
    pub fn sequences(mut self, seqs: Vec<Sequence>) -> Self {
        self.sequences = seqs;
        self
    }
    pub fn copy(&mut self, copy: bool) -> &mut Self {
        self.copy = copy;
        self
    }

    pub fn fifo(&mut self, fifo: Option<FIFO>) -> &mut Self {
        self.fifo = fifo;
        self
    }

    pub fn text(&mut self, text: Option<CdText>) -> &mut Self {
        self.text = text;
        self
    }

    pub fn indices(mut self, indices: Vec<MSF>) -> Self {
        self.indices = indices;
        self
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SubChannelMode {
    #[allow(non_camel_case_types)]
    RW,
    #[allow(non_camel_case_types)]
    RW_RAW,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TrackMode {
    pub mode: TrackModeType,
    pub subchannel_mode: Option<SubChannelMode>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TOC {
    pub cdtype: CDType,
    pub tracks: Vec<Track>,
    pub catalog: Option<String>, // JAN
    pub text: Option<CdText>,
}
