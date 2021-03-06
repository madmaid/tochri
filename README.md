# tochri
**WORKING IN PROGRESS**
Compact Disc TOC file Parser written in Rust

## What is this
parses TOC file generated by CD ripper program.
this helps you to access meta-datum like:
 - cue time of track
 - length of each tracks
 - number of source channels

## Installation
add this to your `Cargo.toml`:

```toml
[dependencies]
tochri = {git = "https://github.com/madmaid/tochri.git"}
```

## Example 
```rust

extern crate tochri;

fn main() {
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
    tochri::parse(text)

}

```


## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
