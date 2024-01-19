//! A library to parse m3u8 playlists [HTTP Live Streaming](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19).
//!
//! # Examples
//!
//! Parsing a playlist and let the parser figure out if it's a media or master playlist.
//!
//! ```
//! use m3u8_rs::Playlist;
//! use nom::IResult;
//! use std::io::Read;
//!
//! let mut file = std::fs::File::open("playlist.m3u8").unwrap();
//! let mut bytes: Vec<u8> = Vec::new();
//! file.read_to_end(&mut bytes).unwrap();
//!
//! match m3u8_rs::parse_playlist(&bytes) {
//!     Result::Ok((i, Playlist::MasterPlaylist(pl))) => println!("Master playlist:\n{:?}", pl),
//!     Result::Ok((i, Playlist::MediaPlaylist(pl))) => println!("Media playlist:\n{:?}", pl),
//!     Result::Err(e) =>  panic!("Parsing error: \n{}", e),
//! }
//! ```
//!
//! Parsing a master playlist directly
//!
//! ```
//! use std::io::Read;
//! use nom::IResult;
//!
//! let mut file = std::fs::File::open("masterplaylist.m3u8").unwrap();
//! let mut bytes: Vec<u8> = Vec::new();
//! file.read_to_end(&mut bytes).unwrap();
//!
//! if let Result::Ok((_, pl)) = m3u8_rs::parse_master_playlist(&bytes) {
//!     println!("{:?}", pl);
//! }
//! ```
//!
//! Creating a playlist and writing it back to a vec/file
//!
//! ```
//! use m3u8_rs::{MediaPlaylist, MediaPlaylistType, MediaSegment};
//!
//! let playlist = MediaPlaylist {
//!     version: Some(6),
//!     target_duration: 3,
//!     media_sequence: 338559,
//!     discontinuity_sequence: 1234,
//!     end_list: true,
//!     playlist_type: Some(MediaPlaylistType::Vod),
//!     segments: vec![
//!         MediaSegment {
//!             uri: "20140311T113819-01-338559live.ts".into(),
//!             duration: 2.002,
//!             title: Some("title".into()),
//!             ..Default::default()
//!         },
//!     ],
//!     ..Default::default()
//! };
//!
//! //let mut v: Vec<u8> = Vec::new();
//! //playlist.write_to(&mut v).unwrap();
//!
//! //let mut file = std::fs::File::open("playlist.m3u8").unwrap();
//! //playlist.write_to(&mut file).unwrap();
//! ```
//!
//! Controlling the output precision for floats, such as #EXTINF (default is unset)
//!
//! ```
//! use std::sync::atomic::Ordering;
//! use m3u8_rs::{WRITE_OPT_FLOAT_PRECISION, MediaPlaylist, MediaSegment};
//!
//! WRITE_OPT_FLOAT_PRECISION.store(5, Ordering::Relaxed);
//!
//! let playlist = MediaPlaylist {
//!     target_duration: 3,
//!     segments: vec![
//!         MediaSegment {
//!             duration: 2.9,
//!             title: Some("title".into()),
//!             ..Default::default()
//!         },
//!     ],
//!     ..Default::default()
//! };
//!
//! let mut v: Vec<u8> = Vec::new();
//!
//! playlist.write_to(&mut v).unwrap();
//! let m3u8_str: &str = std::str::from_utf8(&v).unwrap();
//! assert!(m3u8_str.contains("#EXTINF:2.90000,title"));

mod playlist;
pub use playlist::*;

#[cfg(feature = "parser")]
mod parser;

#[cfg(feature = "parser")]
pub use self::parser::*;
