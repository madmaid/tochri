use std::collections::HashMap;
use std::path::PathBuf;

use combine::char::{char, digit, space, spaces, string};
use combine::error::ParseError;
use combine::parser::combinator::{no_partial, opaque};
use combine::{
    attempt, between, choice, from_str, many, many1, optional, satisfy, sep_by, skip_many,
    skip_many1, token, Parser, Stream,
};

use crate::models::{
    AudioTrackBuilder,
    CDType,
    CdText,
    DataTrackBuilder,
    FIFOLength,
    //Language,
    LanguageValue,
    Sequence,
    SubChannelMode,
    Track,
    TrackMode,
    TrackModeType,
    FIFO,
    ISRC,
    MSF,
    TOC,
};

pub(crate) fn whitespace<I>() -> impl Parser<Input = I>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let comment = (string("//"), skip_many(satisfy(|c| c != '\n'))).map(|_| ());
    skip_many(skip_many1(space()).or(comment))
}

pub(crate) fn double_quote<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(token('"'), token('"'), many(satisfy(|c| c != '"')))
}

pub(crate) fn curly_bracket<I, O>(
    parser: impl Parser<Input = I, Output = O>,
) -> impl Parser<Input = I, Output = O>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(token('{').skip(spaces()), token('}'), parser.skip(spaces()))
}

pub(crate) fn language_map_pair<I>() -> impl Parser<Input = I, Output = (u8, String)>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        digits().skip(spaces()),
        token(':').skip(spaces()),
        many(satisfy(|c| c != '}' && c != '\n')),
    )
        .map(|(k, _, v)| (k as u8, v))
}

pub(crate) fn language_map<I>() -> impl Parser<Input = I, Output = HashMap<u8, String>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        string("LANGUAGE_MAP").skip(spaces()),
        curly_bracket(many1(language_map_pair().skip(spaces()))),
    )
        .map(|(_, v)| v)
}

pub(crate) fn language_pairs<I>() -> impl Parser<Input = I, Output = HashMap<String, LanguageValue>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let key = (
        many1::<String, _>(satisfy(|c| {
            c != ' ' && c != '{' && c != '}' && c != '{' && c != '\n'
        })),
        spaces(),
    )
        .map(|(k, _)| k);
    let value = choice((
        attempt(double_quote().map(LanguageValue::String)),
        attempt(sep_by(digits().map(|v| v as u8), token(',')).map(LanguageValue::Binary)),
    ));

    many1::<HashMap<String, LanguageValue>, _>((key, value).skip(spaces()))
}

pub(crate) fn language<I>() -> impl Parser<Input = I, Output = (u8, HashMap<String, LanguageValue>)>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        string("LANGUAGE").skip(spaces()),
        digits().map(|v| v as u8).skip(spaces()),
        curly_bracket(language_pairs()),
    )
        .map(|(_, k, v)| (k, v))
}

pub(crate) fn cd_text<I>() -> impl Parser<Input = I, Output = CdText>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        string("CD_TEXT").skip(spaces()),
        curly_bracket((
            language_map().skip(spaces()),
            many1::<HashMap<u8, HashMap<String, LanguageValue>>, _>(language().skip(spaces())),
        )),
    )
        .map(|(_, (lang_map, lang))| CdText {
            language_map: lang_map,
            language: lang,
        })
}

pub(crate) fn subchannel_mode<I>() -> impl Parser<Input = I, Output = SubChannelMode>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let rw_raw = string("RW_RAW").map(|_| SubChannelMode::RW_RAW);
    let rw = string("RW").map(|_| SubChannelMode::RW);
    attempt(rw_raw).or(attempt(rw))
}

pub(crate) fn no<I>(flag: &'static str) -> impl Parser<Input = I, Output = bool>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (optional(string("NO")), optional(spaces()), string(flag))
        .skip(whitespace())
        .map(|(no, _, _)| no.is_none())
}

pub(crate) fn channels<I>() -> impl Parser<Input = I, Output = u8>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        string("TWO_CHANNEL_AUDIO").map(|_| 2),
        string("FOUR_CHANNEL_AUDIO").map(|_| 4),
    ))
    .skip(whitespace())
}

pub(crate) fn build_isrc(isrc: String) -> ISRC {
    let splitten: Vec<&str> = isrc.split('-').collect();
    if splitten[0] == isrc.as_str() {
        // '-' is not included in input
        let s: Vec<char> = isrc.chars().collect();
        return ISRC {
            country: s[0..2].into_iter().collect(),
            owner: s[2..5].into_iter().collect(),
            year: s[5..7].into_iter().collect(),
            serial: s[7..12].into_iter().collect(),
        };
    }
    ISRC {
        country: splitten[0].to_owned(),
        owner: splitten[1].to_owned(),
        year: splitten[2].to_owned(),
        serial: splitten[3].to_owned(),
    }
}

pub(crate) fn isrc<I>() -> impl Parser<Input = I, Output = ISRC>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (string("ISRC"), whitespace(), double_quote())
        .skip(whitespace())
        .map(|(_, _, value)| build_isrc(value))
}
pub(crate) fn digits<I>() -> impl Parser<Input = I, Output = u32>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    from_str(many1::<String, _>(digit()))
}

pub(crate) fn timecode<I>() -> impl Parser<Input = I, Output = MSF>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(
        (digits(), token(':'), digits(), token(':'), digits())
            .map(|(minutes, _, seconds, _, frames)| MSF(minutes, seconds, frames)),
    )
    .or(char('0').map(|_| MSF(0, 0, 0)))
}

pub(crate) fn filepath<I>(s: &'static str) -> impl Parser<Input = I, Output = PathBuf>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (string(s), spaces(), double_quote()).map(|(_, _, path)| PathBuf::from(path))
}

pub(crate) fn file<I>() -> impl Parser<Input = I, Output = Sequence>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        filepath("FILE").or(filepath("AUDIOFILE")),
        spaces(),
        timecode(),
        optional((spaces(), timecode()).map(|(_, length)| length)),
    )
        .skip(whitespace())
        .map(|(path, _, start, length)| Sequence::File {
            filename: path,
            start: start,
            length: length,
        })
}

pub(crate) fn datafile<I>() -> impl Parser<Input = I, Output = Sequence>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (filepath("DATAFILE"), spaces(), optional(timecode()))
        .skip(whitespace())
        .map(|(filename, _, length)| Sequence::DataFile {
            filename: filename,
            length: length,
        })
}

pub(crate) fn fifo<I>() -> impl Parser<Input = I, Output = FIFO>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        filepath("FIFO"),
        spaces(),
        attempt(timecode().map(FIFOLength::MSF))
            .or(from_str(many1::<String, _>(digit())).map(FIFOLength::Byte)),
    )
        .skip(whitespace())
        .map(|(path, _, length)| FIFO {
            path: path,
            length: length,
        })
}

pub(crate) fn gap<I>(flag: &'static str) -> impl Parser<Input = I, Output = MSF>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (string(flag), spaces(), timecode())
        .skip(whitespace())
        .map(|(_, _, tc)| tc)
}

pub(crate) fn zero<I>() -> impl Parser<Input = I, Output = Sequence>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    gap("SILENCE")
        .or(gap("ZERO"))
        .skip(whitespace())
        .map(|tc| Sequence::Zero { length: tc })
}

pub(crate) fn index<I>() -> impl Parser<Input = I, Output = MSF>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (string("INDEX"), spaces(), timecode())
        .skip(whitespace())
        .map(|(_, _, tc)| tc)
}

pub(crate) fn track_mode<I>() -> impl Parser<Input = I, Output = TrackModeType>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let audio = string("AUDIO").map(|_| TrackModeType::Audio);
    let mode1 = string("MODE1").map(|_| TrackModeType::ModeOne);
    let mode1_raw = string("MODE1_RAW").map(|_| TrackModeType::ModeOneRaw);
    let mode2 = string("MODE2").map(|_| TrackModeType::ModeTwo);
    let mode2_form1 = string("MODE2_FORM1").map(|_| TrackModeType::ModeTwoFormOne);
    let mode2_form2 = string("MODE2_FORM2").map(|_| TrackModeType::ModeTwoFormTwo);
    let mode2_form_mix = string("MODE2_FORM_MIX").map(|_| TrackModeType::ModeTwoFormMix);

    choice((
        attempt(audio),
        attempt(mode1_raw), // ordered by longer
        attempt(mode1),
        attempt(mode2_form1),
        attempt(mode2_form2),
        attempt(mode2_form_mix),
        attempt(mode2),
    ))
}
pub(crate) fn track_header<I>() -> impl Parser<Input = I, Output = TrackMode>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        string("TRACK"),
        spaces(),
        track_mode(),
        optional(spaces()),
        optional(subchannel_mode()),
    )
        .skip(whitespace())
        .map(|(_, _, mode, _, subchannel_mode)| TrackMode {
            mode: mode,
            subchannel_mode: subchannel_mode,
        })
}

pub(crate) fn sequences<I>() -> impl Parser<Input = I, Output = Vec<Sequence>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many::<Vec<Sequence>, _>(
        choice((
            attempt(file()),
            attempt(datafile()),
            attempt(string("START").map(|_| Sequence::Start)),
            attempt(gap("PREGAP").map(|tc| Sequence::Pregap { length: tc })),
            attempt(zero()),
        ))
        .skip(whitespace()),
    )
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum AudioFlag {
    Copy(bool),
    PreEmphasis(bool),
    Channels(u8),
    Isrc(ISRC),
    Text(CdText),
}
pub(crate) fn audio_flag<I>() -> impl Parser<Input = I, Output = AudioFlag>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        attempt(no("COPY")).map(AudioFlag::Copy),
        attempt(no("PRE_EMPHASIS")).map(AudioFlag::PreEmphasis),
        attempt(cd_text()).map(AudioFlag::Text),
        attempt(isrc()).map(AudioFlag::Isrc),
        attempt(channels()).map(AudioFlag::Channels),
    ))
}

pub(crate) fn audio_flags<I>() -> impl Parser<Input = I, Output = Vec<AudioFlag>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many::<Vec<AudioFlag>, _>(audio_flag())
}

pub(crate) fn audio_track<I>(mode: TrackMode) -> impl Parser<Input = I, Output = Track>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (audio_flags(), sequences(), many(index()))
        .skip(whitespace())
        .map(move |(flags, seqs, indices)| {
            let mut builder = AudioTrackBuilder::new().mode(mode.clone());
            for f in flags {
                match f {
                    AudioFlag::Copy(v) => builder.copy(v),
                    AudioFlag::PreEmphasis(v) => builder.pre_emphasis(v),
                    AudioFlag::Channels(v) => builder.channels(v),
                    AudioFlag::Isrc(v) => builder.isrc(Some(v)),
                    AudioFlag::Text(v) => builder.text(Some(v)),
                };
            }
            builder.sequences(seqs).indices(indices).build()
        })
}

pub(crate) enum DataFlag {
    Copy(bool),
    Text(CdText),
    Fifo(FIFO),
}

pub(crate) fn data_flag<I>() -> impl Parser<Input = I, Output = DataFlag>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        attempt(no("COPY").map(DataFlag::Copy)),
        attempt(fifo()).map(DataFlag::Fifo),
        attempt(cd_text().map(DataFlag::Text)),
    ))
}

pub(crate) fn data_flags<I>() -> impl Parser<Input = I, Output = Vec<DataFlag>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many::<Vec<_>, _>(data_flag().skip(whitespace()))
}
pub(crate) fn data_track<I>(mode: TrackMode) -> impl Parser<Input = I, Output = Track>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (data_flags(), sequences(), many(index()))
        .skip(whitespace())
        .map(move |(flags, seqs, indices)| {
            let mut builder = DataTrackBuilder::new().mode(mode.clone());
            for f in flags {
                match f {
                    DataFlag::Copy(v) => builder.copy(v),
                    DataFlag::Fifo(v) => builder.fifo(Some(v)),
                    DataFlag::Text(v) => builder.text(Some(v)),
                };
            }
            builder.sequences(seqs).indices(indices).build()
        })
}

pub(crate) fn track<I>() -> impl Parser<Input = I, Output = Track>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    track_header().then(|header| {
        opaque(move |f| {
            if header.mode == TrackModeType::Audio {
                f(&mut no_partial(audio_track(header.clone())))
            } else {
                f(&mut no_partial(data_track(header.clone())))
            }
        })
    })
}

pub(crate) fn tracks<I>() -> impl Parser<Input = I, Output = Vec<Track>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1::<Vec<_>, _>(track()).skip(whitespace())
}

pub(crate) fn toc_header<I>() -> impl Parser<Input = I, Output = CDType>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let cdrom_xa = string("CD_ROM_XA").map(|_| CDType::CD_ROM_XA);
    let cdrom = string("CD_ROM").map(|_| CDType::CD_ROM);
    let cdda = string("CD_DA").map(|_| CDType::CD_DA);

    choice((attempt(cdrom_xa), attempt(cdrom), attempt(cdda))).skip(whitespace())
}

pub(crate) fn catalog<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (string("CATALOG"), spaces(), double_quote())
        .skip(whitespace())
        .map(|(_, _, value)| value)
}

#[allow(dead_code)]
pub fn toc_parser<I>() -> impl Parser<Input = I, Output = TOC>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        whitespace(),
        toc_header(),
        optional(attempt(cd_text())),
        optional(attempt(catalog())),
        tracks(),
    )
        .skip(whitespace())
        .map(|(_, cdtype, text, catalog, tracks)| TOC {
            cdtype: cdtype,
            text: text,
            catalog: catalog,
            tracks: tracks,
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_parses_string() {
        let mut parser = toc_header();
        let text = "CD_ROM_XA";
        let toc = parser.easy_parse(text).map(|result| result.0);

        let expected = CDType::CD_ROM_XA;
        assert_eq!(toc, Ok(expected));
    }

    #[test]
    fn timecode_parses_zero() {
        let mut parser = timecode();
        let text = "0";
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(result, Ok(MSF(0, 0, 0)));
    }
    #[test]
    fn timecode_parses_single_digit() {
        let mut parser = timecode();
        let text = "1:0:0";
        let result = parser.easy_parse(text).map(|result| result.0);

        assert_eq!(result, Ok(MSF(1, 0, 0)));
    }

    #[test]
    fn timecode_parses_double_digits() {
        let mut parser = timecode();
        let text = "03:41:65";
        let result = parser.easy_parse(text).map(|result| result.0);

        assert_eq!(result, Ok(MSF(3, 41, 65)));
    }

    #[test]
    fn double_quote_parses_result_file() {
        let mut parser = double_quote();
        let value = r#""it's a test value.""#;
        let result = parser.easy_parse(value).map(|result| result.0);

        let expected = "it's a test value.";
        assert_eq!(result, Ok(expected.to_string()));
    }

    #[test]
    fn catalog_parses_str() {
        let mut parser = catalog();
        let catalog = r#"CATALOG "0000000000000""#;
        let result = parser.easy_parse(catalog).map(|result| result.0);

        let expected = "0000000000000";
        assert_eq!(result, Ok(expected.to_string()));
    }

    #[test]
    fn track_mode_parses_str() {
        let mut parser = track_mode();
        let text = "MODE2_FORM2";
        let result = parser.easy_parse(text).map(|result| result.0);

        let expected = TrackModeType::ModeTwoFormTwo;
        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn no_parses_str() {
        let mut parser = no("PRE_EMPHASIS");
        let text = "PRE_EMPHASIS";
        let result = parser.easy_parse(text).map(|result| result.0);

        assert_eq!(result, Ok(true));
    }
    #[test]
    fn no_parses_denial() {
        let mut parser = no("COPY");
        let text = "NO COPY";
        let result = parser.easy_parse(text).map(|result| result.0);

        assert_eq!(result, Ok(false));
    }

    #[test]
    fn channels_parses_str() {
        let mut parser = channels();
        let text = "TWO_CHANNEL_AUDIO";
        let result = parser.easy_parse(text).map(|result| result.0);

        assert_eq!(result, Ok(2));
    }
    #[test]
    fn file_parses_str() {
        let mut parser = file();
        let text = r#"FILE "data.wav" 03:41:65"#;
        let result = parser.easy_parse(text).map(|result| result.0);

        assert_eq!(
            result,
            Ok(Sequence::File {
                filename: PathBuf::from("data.wav"),
                start: MSF(3, 41, 65),
                length: None
            })
        );
    }

    #[test]
    fn file_parses_length() {
        let mut parser = file();
        let text = r#"FILE "data.wav" 03:41:65 06:20:35"#;
        let result = parser.easy_parse(text).map(|result| result.0);

        assert_eq!(
            result,
            Ok(Sequence::File {
                filename: PathBuf::from("data.wav"),
                start: MSF(3, 41, 65),
                length: Some(MSF(6, 20, 35)),
            })
        );
    }

    #[test]
    fn isrc_parses_str() {
        let mut parser = isrc();
        let text = r#"ISRC "JPAA00123456""#;
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(
            result,
            Ok(ISRC {
                country: "JP".to_string(),
                owner: "AA0".to_string(),
                year: "01".to_string(),
                serial: "23456".to_string(),
            })
        );
    }

    #[test]
    fn language_pairs_in_curly_bracket() {
        let mut parser = curly_bracket(language_pairs());
        let text = r#"{PERFORMER "Performer"}"#;
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(
            result,
            Ok([(
                "PERFORMER".to_owned(),
                LanguageValue::String("Performer".to_owned())
            )]
            .iter()
            .cloned()
            .collect())
        )
    }
    #[test]
    fn language_pairs_parses_pair() {
        let mut parser = language_pairs();
        let text = r#"TITLE "CD Title""#;
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(
            result,
            Ok([(
                "TITLE".to_owned(),
                LanguageValue::String("CD Title".to_owned())
            )]
            .iter()
            .cloned()
            .collect())
        )
    }
    #[test]
    fn language_pairs_parses_pairs() {
        let mut parser = language_pairs();
        let text = r#"TITLE "CD Title"
        PERFORMER "Performer"
    "#;
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(
            result,
            Ok([
                (
                    "TITLE".to_owned(),
                    LanguageValue::String("CD Title".to_owned())
                ),
                (
                    "PERFORMER".to_owned(),
                    LanguageValue::String("Performer".to_owned())
                )
            ]
            .iter()
            .cloned()
            .collect())
        )
    }

    #[test]
    fn language_parses_str() {
        let mut parser = language();
        let text = r#"LANGUAGE 0 {
            PERFORMER "Performer"
        }"#;
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(
            result,
            Ok((
                0,
                [(
                    "PERFORMER".to_owned(),
                    LanguageValue::String("Performer".to_owned())
                )]
                .iter()
                .cloned()
                .collect()
            ))
        )
    }

    #[test]
    fn cd_text_parses_str() {
        let mut parser = cd_text();

        let text = r#"CD_TEXT {
      LANGUAGE_MAP {
        0 : EN
      }

      LANGUAGE 0 {
        TITLE "CD Title"
        PERFORMER "Performer"
        DISC_ID "XY12345"
        UPC_EAN ""
      }
    }"#;
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(
            result,
            Ok(CdText {
                language_map: [(0, "EN".to_owned())].iter().cloned().collect(),
                language: [(
                    0,
                    [
                        (
                            "TITLE".to_owned(),
                            LanguageValue::String("CD Title".to_owned())
                        ),
                        (
                            "PERFORMER".to_owned(),
                            LanguageValue::String("Performer".to_owned())
                        ),
                        (
                            "DISC_ID".to_owned(),
                            LanguageValue::String("XY12345".to_owned())
                        ),
                        ("UPC_EAN".to_owned(), LanguageValue::String("".to_owned())),
                    ]
                    .iter()
                    .cloned()
                    .collect()
                )]
                .iter()
                .cloned()
                .collect(),
            })
        );
    }

    #[test]
    fn audio_flag_parses_str() {
        let mut parser = audio_flag();
        let text = r#"PRE_EMPHASIS
        "#;
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(result, Ok(AudioFlag::PreEmphasis(true)))
    }

    #[test]
    fn audio_flags_parses_empty() {
        let mut parser = audio_flags();
        let text = "";
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(result, Ok(Vec::new()))
    }

    #[test]
    fn audio_flags_parses_multiple_flags() {
        let mut parser = attempt(audio_flags());
        let text = r#"NO COPY
        NO PRE_EMPHASIS"#;
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(
            result,
            Ok(vec![AudioFlag::Copy(false), AudioFlag::PreEmphasis(false)])
        )
    }

    #[test]
    fn audio_track_parses_str() {
        let mode = TrackMode {
            mode: TrackModeType::Audio,
            subchannel_mode: None,
        };
        let mut parser = audio_track(mode);
        let text = r#"COPY
    NO PRE_EMPHASIS
    TWO_CHANNEL_AUDIO
    FILE "data.wav" 0 03:41:65
    "#;
        let result = parser.easy_parse(text).map(|result| result.0);
        assert_eq!(
            result,
            Ok(Track::Audio {
                mode: TrackMode {
                    mode: TrackModeType::Audio,
                    subchannel_mode: None,
                },

                sequences: vec!(Sequence::File {
                    filename: PathBuf::from("data.wav"),
                    start: MSF(0, 0, 0),
                    length: Some(MSF(3, 41, 65)),
                }),
                copy: true,
                pre_emphasis: false,
                channels: 2,
                isrc: None,
                text: None,
                indices: Vec::new(),
            })
        )
    }

    #[test]
    fn toc_parser_parses_multiple_audio_tracks() {
        let mut parser = toc_parser();
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
        let result = parser
            .easy_parse(text)
            .map_err(|e| e.map_position(|p| p.translate_position(text)))
            .map(|result| result.0);
        assert_eq!(
            result,
            Ok(TOC {
                cdtype: CDType::CD_DA,
                catalog: Some("0000000000000".to_string()),
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
