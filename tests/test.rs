extern crate tochri;

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use tochri::*;
    #[test]
    fn parses_toc() {
        let text = r#"
CD_DA

CATALOG "0000000000000"

// Track 1
TRACK AUDIO
NO COPY
NO PRE_EMPHASIS
TWO_CHANNEL_AUDIO
FILE "data.wav" 0 03:41:65


// Track 2
TRACK AUDIO
NO COPY
NO PRE_EMPHASIS
TWO_CHANNEL_AUDIO
FILE "data.wav" 03:41:65 06:20:35
"#;
        let result = parse(text);
        assert_eq!(
            result,
            Ok(TOC {
                cdtype: CDType::CD_DA,
                catalog: Some("0000000000000".to_owned()),
                text: None,
                tracks: vec!(
                    Track::Audio {
                        mode: TrackMode {
                            mode: TrackModeType::Audio,
                            subchannel_mode: None,
                        },
                        sequences: vec!(Sequence::File {
                            filename: PathBuf::from("data.wav"),
                            start: MSF(0, 0, 0),
                            length: Some(MSF(3, 41, 65)),
                        }),
                        copy: false,
                        pre_emphasis: false,
                        channels: 2,
                        isrc: None,
                        text: None,
                        indices: Vec::new(),
                    },
                    Track::Audio {
                        mode: TrackMode {
                            mode: TrackModeType::Audio,
                            subchannel_mode: None,
                        },
                        sequences: vec!(Sequence::File {
                            filename: PathBuf::from("data.wav"),
                            start: MSF(3, 41, 65),
                            length: Some(MSF(6, 20, 35)),
                        }),
                        copy: false,
                        pre_emphasis: false,
                        channels: 2,
                        isrc: None,
                        text: None,
                        indices: Vec::new(),
                    },
                ),
            })
        )
    }
}
