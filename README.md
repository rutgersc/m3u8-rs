# m3u8-rs
A Rust library for parsing m3u8 playlists (HTTP Live Streaming) [link](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19).
Uses the [`nom` library](https://github.com/Geal/nom) for all of the parsing.

# Installation
To use this library, add the following dependency to `Cargo.toml`:

```toml
[dependencies]
m3u8-rs = "1.0.0"
```

And add the crate to `lib.rs`

```rust
extern crate m3u8_rs;
```

Also available on [crates.io]()

# Documentation

Available [here]()

# Examples

A simple example of parsing a playlist:

```rust
use m3u8_rs::playlist::Playlist;
use std::io::Read;

let mut file = std::fs::File::open("playlist.m3u8").unwrap();
let mut bytes: Vec<u8> = Vec::new();
file.read_to_end(&mut bytes).unwrap();

let parsed = m3u8_rs::parse_playlist_res(&bytes);

match parsed {
    Ok(Playlist::MasterPlaylist(pl)) => println!("Master playlist:\n{}", pl),
    Ok(Playlist::MediaPlaylist(pl)) => println!("Media playlist:\n{}", pl),
    Err(e) => println!("Error: {:?}", e)
}

```

In the example above, `parse_playlist_res(&bytes)` returns a `Result<Playlist, IResult>`. It uses
the output of `parse_playlist(&bytes)` behind the scenes and just converts the `IResult` to a `Result`.
Here is an example of using the `parse_playlist(&bytes)` with `IResult` directly:

```rust
use m3u8_rs::playlist::Playlist;
use std::io::Read;
use nom::IResult;

let mut file = std::fs::File::open("playlist.m3u8").unwrap();
let mut bytes: Vec<u8> = Vec::new();
file.read_to_end(&mut bytes).unwrap();

let parsed = m3u8::parse_playlist(&bytes);

match parsed {
    IResult::Done(i, Playlist::MasterPlaylist(pl)) => println!("Master playlist:\n{}", pl),
    IResult::Done(i, Playlist::MediaPlaylist(pl)) => println!("Media playlist:\n{}", pl),
    IResult::Error(e) =>  panic!("Parsing error: \n{}", e),
    IResult::Incomplete(e) => panic!("Parsing error: \n{:?}", e),
}
```

Currently the parser will succeed even if REQUIRED attributes/tags are missing from a playlist (such as the `#EXT-X-VERSION` tag).
The option to abort parsing when attributes/tags are missing may be something to add later on.

# Structure Summary

All of the details about the structs are taken from https://tools.ietf.org/html/draft-pantos-http-live-streaming-19.


```rust

// Short summary of the important structs in playlist.rs:
//
pub enum Playlist {
    MasterPlaylist(MasterPlaylist),
    MediaPlaylist(MediaPlaylist),
}

pub struct MasterPlaylist {
    pub version: usize,
    pub variants: Vec<VariantStream>,
    pub session_data: Option<SessionData>,
    pub session_key: Option<SessionKey>,
    pub start: Option<Start>,
    pub independent_segments: bool,
}

pub struct MediaPlaylist {
    pub version: usize,
    pub target_duration: f32,
    pub media_sequence: i32,
    pub segments: Vec<MediaSegment>,
    pub discontinuity_sequence: i32,
    pub end_list: bool,
    pub playlist_type: MediaPlaylistType,
    pub i_frames_only: bool,
    pub start: Option<Start>,
    pub independent_segments: bool,
}

pub struct VariantStream {
    pub is_i_frame: bool,
    pub uri: String,
    pub bandwidth: String,
    pub average_bandwidth: Option<String>,
    pub codecs: String,
    pub resolution: Option<String>,
    pub frame_rate: Option<String>,
    pub audio: Option<String>,
    pub video: Option<String>,
    pub subtitles: Option<String>,
    pub closed_captions: Option<String>,
    pub alternatives: Vec<AlternativeMedia>,
}

pub struct MediaSegment {
    pub uri: String,
    pub duration: f32,
    pub title: Option<String>,
    pub byte_range: Option<ByteRange>,
    pub discontinuity: bool,
    pub key: Option<Key>,
    pub map: Option<Map>,
    pub program_date_time: Option<String>,
    pub daterange: Option<String>,
}

```
