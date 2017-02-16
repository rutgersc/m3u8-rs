//! A library to parse m3u8 playlists (HTTP Live Streaming) [link]
//! (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19).
//!
//! #Examples
//!
//! Parsing a playlist and let the parser figure out if it's a media or master playlist.
//!
//! ```
//! extern crate m3u8_rs;
//! extern crate nom;
//! use m3u8_rs::playlist::Playlist;
//! use nom::IResult;
//! use std::io::Read;
//!
//! let mut file = std::fs::File::open("playlist.m3u8").unwrap();
//! let mut bytes: Vec<u8> = Vec::new();
//! file.read_to_end(&mut bytes).unwrap();
//!
//! // Option 1: fn parse_playlist_res(input) -> Result<Playlist, _>
//! match m3u8_rs::parse_playlist_res(&bytes) {
//!     Ok(Playlist::MasterPlaylist(pl)) => println!("Master playlist:\n{}", pl),
//!     Ok(Playlist::MediaPlaylist(pl)) => println!("Media playlist:\n{}", pl),
//!     Err(e) => println!("Error: {:?}", e)
//! }
//!
//! // Option 2: fn parse_playlist(input) -> IResult<_, Playlist, _>
//! match m3u8_rs::parse_playlist(&bytes) {
//!     IResult::Done(i, Playlist::MasterPlaylist(pl)) => println!("Master playlist:\n{}", pl),
//!     IResult::Done(i, Playlist::MediaPlaylist(pl)) => println!("Media playlist:\n{}", pl),
//!     IResult::Error(e) =>  panic!("Parsing error: \n{}", e),
//!     IResult::Incomplete(e) => panic!("Parsing error: \n{:?}", e),
//! }
//! ```
//!
//! Parsing a master playlist directly
//!
//! ```
//! extern crate m3u8_rs;
//! extern crate nom;
//! use std::io::Read;
//! use nom::IResult;
//!
//! let mut file = std::fs::File::open("masterplaylist.m3u8").unwrap();
//! let mut bytes: Vec<u8> = Vec::new();
//! file.read_to_end(&mut bytes).unwrap();
//!
//! if let IResult::Done(_, pl) = m3u8_rs::parse_master_playlist(&bytes) {
//!     println!("{}", pl);
//! }
//!
//! ```

#[macro_use]
extern crate nom;

pub mod playlist;

use nom::*;
use std::str;
use std::f32;
use std::string;
use std::str::FromStr;
use std::result::Result;
use std::collections::HashMap;
use playlist::*;

// -----------------------------------------------------------------------------------------------
// Playlist parser
// -----------------------------------------------------------------------------------------------

/// Parse a m3u8 playlist.
///
/// #Examples
///
/// let mut file = std::fs::File::open("playlist.m3u8").unwrap();
/// let mut bytes: Vec<u8> = Vec::new();
/// file.read_to_end(&mut bytes).unwrap();
///
/// let parsed = m3u8_rs::parse_playlist(&bytes);
///
/// let playlist = match parsed {
///     IResult::Done(i, playlist) => playlist,
///     IResult::Error(e) =>  panic!("Parsing error: \n{}", e),
///     IResult::Incomplete(e) => panic!("Parsing error: \n{:?}", e),
/// };
///
/// match playlist {
///     Playlist::MasterPlaylist(pl) => println!("Master playlist:\n{}", pl),
///     Playlist::MediaPlaylist(pl) => println!("Media playlist:\n{}", pl),
/// }
pub fn parse_playlist(input: &[u8]) -> IResult<&[u8], Playlist> {
    match is_master_playlist(input) {
        true => parse_master_playlist(input).map(Playlist::MasterPlaylist),
        false => parse_media_playlist(input).map(Playlist::MediaPlaylist),
    }
}

/// Parse a m3u8 playlist just like `parse_playlist`. This returns a Result<PLaylist,_>.
///
/// #Examples
///
/// ```
/// use m3u8_rs::playlist::{Playlist};
/// use std::io::Read;
///
/// let mut file = std::fs::File::open("playlist.m3u8").unwrap();
/// let mut bytes: Vec<u8> = Vec::new();
/// file.read_to_end(&mut bytes).unwrap();
///
/// let parsed = m3u8_rs::parse_playlist_res(&bytes);
///
/// match parsed {
///     Ok(Playlist::MasterPlaylist(pl)) => println!("Master playlist:\n{}", pl),
///     Ok(Playlist::MediaPlaylist(pl)) => println!("Media playlist:\n{}", pl),
///     Err(e) => println!("Error: {:?}", e)
/// }
/// ```
pub fn parse_playlist_res(input: &[u8]) -> Result<Playlist, IResult<&[u8], Playlist>> {
    let parse_result = parse_playlist(input);
    match parse_result {
        IResult::Done(_, playlist) => Ok(playlist),
        _ => Err(parse_result),
    }
}

/// Parse input as a master playlist
pub fn parse_master_playlist(input: &[u8]) -> IResult<&[u8], MasterPlaylist> {
    parse_master_playlist_tags(input).map(MasterPlaylist::from_tags)
}

/// Parse input as a media playlist
pub fn parse_media_playlist(input: &[u8]) -> IResult<&[u8], MediaPlaylist> {
    parse_media_playlist_tags(input).map(MediaPlaylist::from_tags)
}

/// When a media tag or no master tag is found, this returns false.
pub fn is_master_playlist(input: &[u8]) -> bool {
    // Assume it's not a master playlist
    contains_master_tag(input).map(|t| t.0).unwrap_or(false)
}

/// Scans input looking for either a master or media `#EXT` tag.
///
/// Returns `Some(true/false)` when a master/media tag is found. Otherwise returns `None`.
///
/// - None: Unkown tag or empty line
/// - Some(true, tagstring): Line contains a master playlist tag
/// - Some(false, tagstring): Line contains a media playlist tag
pub fn contains_master_tag(input: &[u8]) -> Option<(bool, String)> {

    let mut is_master_opt = None;
    let mut current_input: &[u8] = input;

    while is_master_opt == None {
        match is_master_playlist_tag_line(current_input) {
            IResult::Done(rest, result) => {
                current_input = rest;
                is_master_opt = result; // result can be None (no media or master tag found)
            }
            _ => break, // Parser error encountered, can't read any more lines.
        }
    }

    is_master_opt
}

named!(pub is_master_playlist_tag_line(&[u8]) -> Option<(bool, String)>,
    chain!(
        tag: opt!(alt!(
                  map!(tag!("#EXT-X-STREAM-INF"),         |t| (true, t))
                | map!(tag!("#EXT-X-I-FRAME-STREAM-INF"), |t| (true, t))
                | map!(tag!("#EXT-X-MEDIA"),              |t| (true, t))
                | map!(tag!("#EXT-X-SESSION-KEY"),        |t| (true, t))
                | map!(tag!("#EXT-X-SESSION-DATA"),       |t| (true, t))

                | map!(tag!("#EXT-X-TARGETDURATION"),         |t| (false, t))
                | map!(tag!("#EXT-X-MEDIA-SEQUENCE"),         |t| (false, t))
                | map!(tag!("#EXT-X-DISCONTINUITY-SEQUENCE"), |t| (false, t))
                | map!(tag!("#EXT-X-ENDLIST"),                |t| (false, t))
                | map!(tag!("#EXT-X-PLAYLIST-TYPE"),          |t| (false, t))
                | map!(tag!("#EXT-X-I-FRAMES-ONLY"),          |t| (false, t))

                | map!(tag!("#EXTINF"),                       |t| (false, t))
                | map!(tag!("#EXT-X-BYTERANGE"),              |t| (false, t))
                | map!(tag!("#EXT-X-DISCONTINUITY"),          |t| (false, t))
                | map!(tag!("#EXT-X-KEY"),                    |t| (false, t))
                | map!(tag!("#EXT-X-MAP"),                    |t| (false, t))
                | map!(tag!("#EXT-X-PROGRAM-DATE-TIME"),      |t| (false, t))
                | map!(tag!("#EXT-X-DATERANGE"),              |t| (false, t))
        ))
        ~ consume_line
        , || {
            tag.map(|(a,b)| (a, from_utf8_slice(b).unwrap()))
        }
    )
);

// -----------------------------------------------------------------------------------------------
// Master Playlist Tags
// -----------------------------------------------------------------------------------------------

pub fn parse_master_playlist_tags(input: &[u8]) -> IResult<&[u8], Vec<MasterPlaylistTag>> {
    chain!(input,
        mut tags: many0!(chain!(m:master_playlist_tag ~ multispace?, || m)) ~ eof?,
        || { tags.reverse(); tags }
    )
}

/// Contains all the tags required to parse a master playlist.
#[derive(Debug)]
pub enum MasterPlaylistTag {
    M3U(String),
    Version(usize),
    VariantStream(VariantStream),
    AlternativeMedia(AlternativeMedia),
    SessionData(SessionData),
    SessionKey(SessionKey),
    Start(Start),
    IndependentSegments,
    Unknown(ExtTag),
    Comment(String),
    Uri(String),
}

pub fn master_playlist_tag(input: &[u8]) -> IResult<&[u8], MasterPlaylistTag> {
    alt!(input,
          map!(m3u_tag, MasterPlaylistTag::M3U)
        | map!(version_tag, MasterPlaylistTag::Version)

        | map!(variant_stream_tag, MasterPlaylistTag::VariantStream)
        | map!(variant_i_frame_stream_tag, MasterPlaylistTag::VariantStream)
        | map!(alternative_media_tag, MasterPlaylistTag::AlternativeMedia)
        | map!(session_data_tag, MasterPlaylistTag::SessionData)
        | map!(session_key_tag, MasterPlaylistTag::SessionKey)
        | map!(start_tag, MasterPlaylistTag::Start)
        | map!(tag!("#EXT-X-INDEPENDENT-SEGMENTS"), |_| MasterPlaylistTag::IndependentSegments)

        | map!(ext_tag, MasterPlaylistTag::Unknown)
        | map!(comment_tag, MasterPlaylistTag::Comment)

        | map!(consume_line, MasterPlaylistTag::Uri)
    )
}

named!(pub variant_stream_tag<VariantStream>,
    chain!(tag!("#EXT-X-STREAM-INF:") ~ attributes: key_value_pairs,
           || VariantStream::from_hashmap(attributes, false))
);

named!(pub variant_i_frame_stream_tag<VariantStream>,
    chain!( tag!("#EXT-X-I-FRAME-STREAM-INF:") ~ attributes: key_value_pairs,
            || VariantStream::from_hashmap(attributes, true))
);

named!(pub alternative_media_tag<AlternativeMedia>,
    chain!( tag!("#EXT-X-MEDIA:") ~ attributes: key_value_pairs,
            || AlternativeMedia::from_hashmap(attributes))
);

named!(pub session_data_tag<SessionData>,
    chain!( tag!("#EXT-X-SESSION-DATA:") ~ attributes: key_value_pairs,
            || SessionData::from_hashmap(attributes))
);

named!(pub session_key_tag<SessionKey>,
    chain!( tag!("#EXT-X-SESSION-KEY:") ~ session_key: map!(key, SessionKey),
            || session_key)
);

// -----------------------------------------------------------------------------------------------
// Media Playlist
// -----------------------------------------------------------------------------------------------

pub fn parse_media_playlist_tags(input: &[u8]) -> IResult<&[u8], Vec<MediaPlaylistTag>> {
    chain!(input,
        mut tags: many0!(chain!(m:media_playlist_tag ~ multispace?, || m)) ~ eof?,
        || { tags.reverse(); tags }
    )
}

/// Contains all the tags required to parse a media playlist.
#[derive(Debug)]
pub enum MediaPlaylistTag {
    M3U(String),
    Version(usize),
    Segment(SegmentTag),
    TargetDuration(f32),
    MediaSequence(i32),
    DiscontinuitySequence(i32),
    EndList,
    PlaylistType(MediaPlaylistType),
    IFramesOnly,
    Start(Start),
    IndependentSegments,
}

pub fn media_playlist_tag(input: &[u8]) -> IResult<&[u8], MediaPlaylistTag> {
    alt!(input,
      map!(m3u_tag, MediaPlaylistTag::M3U)
    | map!(version_tag, MediaPlaylistTag::Version)

    | map!(chain!(tag!("#EXT-X-TARGETDURATION:") ~ n:float,||n), MediaPlaylistTag::TargetDuration)
    | map!(chain!(tag!("#EXT-X-MEDIA-SEQUENCE:") ~ n:number,||n), MediaPlaylistTag::MediaSequence)
    | map!(chain!(tag!("#EXT-X-DISCONTINUITY-SEQUENCE:") ~ n:number,||n), MediaPlaylistTag::DiscontinuitySequence)
    | map!(playlist_type_tag, MediaPlaylistTag::PlaylistType)
    | map!(tag!("#EXT-X-I-FRAMES-ONLY"), |_| MediaPlaylistTag::IFramesOnly)
    | map!(start_tag, MediaPlaylistTag::Start)
    | map!(tag!("#EXT-X-INDEPENDENT-SEGMENTS"), |_| MediaPlaylistTag::IndependentSegments)

    | map!(media_segment_tag, MediaPlaylistTag::Segment)
    )
}

named!(pub playlist_type_tag<MediaPlaylistType>,
    map_res!(
        map_res!(tag!("#EXT-X-PLAYLIST-TYPE:"), str::from_utf8),
        MediaPlaylistType::from_str)
);

// -----------------------------------------------------------------------------------------------
// Media Segment
// -----------------------------------------------------------------------------------------------

/// All possible media segment tags.
#[derive(Debug)]
pub enum SegmentTag {
    Extinf(f32, Option<String>),
    ByteRange(ByteRange),
    Discontinuity,
    Key(Key),
    Map(Map),
    ProgramDateTime(String),
    DateRange(String),
    Unknown(ExtTag),
    Comment(String),
    Uri(String),
}

pub fn media_segment_tag(input: &[u8]) -> IResult<&[u8], SegmentTag> {
    alt!(input,
      map!(chain!(tag!("#EXTINF:") ~ e:duration_title_tag,||e), |(a,b)| SegmentTag::Extinf(a,b))
    | map!(chain!(tag!("#EXT-X-BYTERANGE:") ~ r:byterange_val, || r), SegmentTag::ByteRange)
    | map!(tag!("#EXT-X-DISCONTINUITY"), |_| SegmentTag::Discontinuity)
    | map!(chain!(tag!("#EXT-X-KEY:") ~ k:key, || k), SegmentTag::Key)
    | map!(chain!(tag!("#EXT-X-MAP:") ~ m:map, || m), SegmentTag::Map)
    | map!(chain!(tag!("#EXT-X-PROGRAM-DATE-TIME:") ~ t:consume_line, || t), SegmentTag::ProgramDateTime)
    | map!(chain!(tag!("#EXT-X-DATE-RANGE:") ~ t:consume_line, || t), SegmentTag::DateRange)

    | map!(ext_tag, SegmentTag::Unknown)
    | map!(comment_tag, SegmentTag::Comment)

    | map!(consume_line, SegmentTag::Uri)
    )
}

named!(pub duration_title_tag<(f32, Option<String>)>,
    chain!(
          duration: float
        ~ tag!(",")?
        ~ title: opt!(map_res!(take_until_and_consume!("\r\n"), from_utf8_slice))
        ~ tag!(",")?
        ,
        || (duration, title)
    )
);

named!(pub key<Key>, map!(key_value_pairs, Key::from_hashmap));

named!(pub map<Map>,
    chain!(
        uri: quoted ~ range: opt!(chain!(char!(',') ~ b:byterange_val,||b )),
        || Map { uri: uri, byterange: range }
    )
);

// -----------------------------------------------------------------------------------------------
// Basic tags
// -----------------------------------------------------------------------------------------------

named!(pub m3u_tag<String>,
    map_res!(tag!("#EXTM3U"), from_utf8_slice)
);

named!(pub version_tag<usize>,
    chain!(
        tag!("#EXT-X-VERSION:") ~ version: map_res!(digit, str::from_utf8),
        || version.parse().unwrap_or_default()
    )
);

named!(pub start_tag<Start>,
    chain!(tag!("#EXT-X-START:") ~ attributes:key_value_pairs, || Start::from_hashmap(attributes))
);

named!(pub ext_tag<ExtTag>,
    chain!(
          tag!("#EXT-")
        ~ tag: map_res!(take_until_and_consume!(":"), from_utf8_slice)
        ~ rest: map_res!(take_until_either_and_consume!("\r\n"), from_utf8_slice)
        ,
        || ExtTag { tag: tag, rest: rest }
    )
);

named!(pub comment_tag<String>,
    chain!(
        tag!("#") ~ text: map_res!(take_until_either_and_consume!("\r\n"), from_utf8_slice),
        || text
    )
);

// -----------------------------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------------------------

named!(pub key_value_pairs(&[u8]) -> HashMap<String, String>,
    map!(
        many0!(chain!(space? ~ k:key_value_pair,|| k))
        ,
        |pairs: Vec<(String, String)>| {
            pairs.into_iter().collect()
        }
    )
);

named!(pub key_value_pair(&[u8]) -> (String, String),
    chain!(
          peek!(none_of!("\r\n"))
        ~ left: map_res!(take_until_and_consume!("="), from_utf8_slice)
        ~ right: alt!(quoted | unquoted)
        ~ char!(',')?
        ,
        || (left, right)
    )
);

named!(pub quoted<String>,
    delimited!(char!('\"'), map_res!(is_not!("\""), from_utf8_slice), char!('\"'))
);

named!(pub unquoted<String>,
    map_res!(take_until_either!(",\r\n"), from_utf8_slice)
);

named!(pub consume_line<String>,
    map_res!(take_until_either_and_consume!("\r\n"), from_utf8_slice)
);

named!(pub number<i32>,
    map_res!(map_res!(digit, str::from_utf8), str::FromStr::from_str)
);

named!(pub byterange_val<ByteRange>,
    chain!(
          n: number
        ~ o: opt!(chain!(char!('@') ~ n:number,||n))
        ,
        || ByteRange { length: n, offset: o }
    )
);

named!(pub float<f32>,
    chain!(
          left: map_res!(digit, str::from_utf8)
        ~ right_opt: opt!(chain!(char!('.') ~ d:map_res!(digit, str::from_utf8),|| d )),
        ||
        match right_opt {
            Some(right) => {
                let mut num = String::from(left);
                num.push('.');
                num.push_str(right);
                num.parse().unwrap()
            },
            None => left.parse().unwrap(),
        }
    )
);

pub fn from_utf8_slice(s: &[u8]) -> Result<String, string::FromUtf8Error> {
    String::from_utf8(s.to_vec())
}

pub fn from_utf8_slice2(s: &[u8]) -> Result<String, str::Utf8Error> {
    str::from_utf8(s).map(String::from)
}
