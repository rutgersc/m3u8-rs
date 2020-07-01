//! A library to parse m3u8 playlists (HTTP Live Streaming) [link]
//! (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19).
//!
//! # Examples
//!
//! Parsing a playlist and let the parser figure out if it's a media or master playlist.
//!
//! ```
//! extern crate nom;
//! extern crate m3u8_rs;
//! use m3u8_rs::playlist::Playlist;
//! use nom::IResult;
//! use std::io::Read;
//!
//! fn main() {
//!     let mut file = std::fs::File::open("playlist.m3u8").unwrap();
//!     let mut bytes: Vec<u8> = Vec::new();
//!     file.read_to_end(&mut bytes).unwrap();
//!
//!     match m3u8_rs::parse_playlist(&bytes) {
//!         Result::Ok((i, Playlist::MasterPlaylist(pl))) => println!("Master playlist:\n{:?}", pl),
//!         Result::Ok((i, Playlist::MediaPlaylist(pl))) => println!("Media playlist:\n{:?}", pl),
//!         Result::Err(e) =>  panic!("Parsing error: \n{}", e),
//!     }
//! }
//! ```
//!
//! Parsing a master playlist directly
//!
//! ```
//! extern crate nom;
//! extern crate m3u8_rs;
//! use std::io::Read;
//! use nom::IResult;
//!
//! fn main() {
//!     let mut file = std::fs::File::open("masterplaylist.m3u8").unwrap();
//!     let mut bytes: Vec<u8> = Vec::new();
//!     file.read_to_end(&mut bytes).unwrap();
//!
//!     if let Result::Ok((_, pl)) = m3u8_rs::parse_master_playlist(&bytes) {
//!         println!("{:?}", pl);
//!     }
//! }
//!
//! ```
//!
//! Creating a playlist and writing it back to a vec/file
//!
//! ```
//! extern crate m3u8_rs;
//! use m3u8_rs::playlist::{MediaPlaylist, MediaPlaylistType, MediaSegment};
//!
//! fn main() {
//!     let playlist = MediaPlaylist {
//!         version: 6,
//!         target_duration: 3.0,
//!         media_sequence: 338559,
//!         discontinuity_sequence: 1234,
//!         end_list: true,
//!         playlist_type: Some(MediaPlaylistType::Vod),
//!         segments: vec![
//!             MediaSegment {
//!                 uri: "20140311T113819-01-338559live.ts".into(),
//!                 duration: 2.002,
//!                 title: Some("title".into()),
//!                 ..Default::default()
//!             },
//!         ],
//!         ..Default::default()
//!     };
//!
//!     //let mut v: Vec<u8> = Vec::new();
//!     //playlist.write_to(&mut v).unwrap();
//!
//!     //let mut file = std::fs::File::open("playlist.m3u8").unwrap();
//!     //playlist.write_to(&mut file).unwrap();
//! }
//!
//! ```

extern crate nom;

pub mod playlist;

use nom::character::complete::{digit1, multispace0, space0 };
use nom::{IResult};
use nom::{ delimited,none_of,peek,is_not,complete,terminated,tag,
           alt,do_parse,opt,named,map,map_res,eof,many0,take,take_until,char};
use nom::combinator::map;
use nom::character::complete::{line_ending};
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

/// Parse an m3u8 playlist.
///
/// # Examples
///
/// ```
/// use std::io::Read;
/// use m3u8_rs::playlist::{Playlist};
/// 
/// let mut file = std::fs::File::open("playlist.m3u8").unwrap();
/// let mut bytes: Vec<u8> = Vec::new();
/// file.read_to_end(&mut bytes).unwrap();
///
/// let parsed = m3u8_rs::parse_playlist(&bytes);
///
/// let playlist = match parsed {
///     Result::Ok((i, playlist)) => playlist,
///     Result::Err(e) => panic!("Parsing error: \n{}", e),
/// };
///
/// match playlist {
///     Playlist::MasterPlaylist(pl) => println!("Master playlist:\n{:?}", pl),
///     Playlist::MediaPlaylist(pl) => println!("Media playlist:\n{:?}", pl),
/// }
/// ```
pub fn parse_playlist(input: &[u8]) -> IResult<&[u8], Playlist> {
    match is_master_playlist(input) {
        true => map(parse_master_playlist, Playlist::MasterPlaylist)(input),
        false =>map(parse_media_playlist, Playlist::MediaPlaylist)(input),
    }
}

/// Parses an m3u8 playlist just like `parse_playlist`, except that this returns an [std::result::Result](std::result::Result) instead of a [nom::IResult](https://docs.rs/nom/1.2.3/nom/enum.IResult.html).
/// However, since [nom::IResult](nom::IResult) is now an [alias to Result](https://github.com/Geal/nom/blob/master/doc/upgrading_to_nom_5.md), this is no longer needed. 
/// 
/// # Examples
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
///     Ok(Playlist::MasterPlaylist(pl)) => println!("Master playlist:\n{:?}", pl),
///     Ok(Playlist::MediaPlaylist(pl)) => println!("Media playlist:\n{:?}", pl),
///     Err(e) => println!("Error: {:?}", e)
/// }
/// ```
pub fn parse_playlist_res(input: &[u8]) -> Result<Playlist, IResult<&[u8], Playlist>> {
    let parse_result = parse_playlist(input);
    match parse_result {
        IResult::Ok((_, playlist)) => Ok(playlist),
        _ => Err(parse_result),
    }
}

/// Parse input as a master playlist
pub fn parse_master_playlist(input: &[u8]) -> IResult<&[u8], MasterPlaylist> {
    map(parse_master_playlist_tags, MasterPlaylist::from_tags)(input)
}

/// Parse input as a master playlist
pub fn parse_master_playlist_res(input: &[u8]) -> Result<MasterPlaylist, IResult<&[u8], MasterPlaylist>> {
    let parse_result = parse_master_playlist(input);
    match parse_result {
        IResult::Ok((_, playlist)) => Ok(playlist),
        _ => Err(parse_result),
    }
}

/// Parse input as a media playlist
pub fn parse_media_playlist(input: &[u8]) -> IResult<&[u8], MediaPlaylist> {
    map(parse_media_playlist_tags, MediaPlaylist::from_tags)(input)
}

/// Parse input as a media playlist
pub fn parse_media_playlist_res(input: &[u8]) -> Result<MediaPlaylist, IResult<&[u8], MediaPlaylist>> {
    let parse_result = parse_media_playlist(input);
    match parse_result {
        IResult::Ok((_, playlist)) => Ok(playlist),
        _ => Err(parse_result),
    }
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
            IResult::Ok((rest, result)) => {
                current_input = rest;
                is_master_opt = result; // result can be None (no media or master tag found)
            }
            _ => break, // Parser error encountered, can't read any more lines.
        }
    }

    is_master_opt
}

named!(pub is_master_playlist_tag_line(&[u8]) -> Option<(bool, String)>,
    do_parse!(
        tag: opt!(alt!(
                  map!(tag!("#EXT-X-STREAM-INF"),         |t| (true, t))
                | map!(tag!("#EXT-X-I-FRAME-STREAM-INF"), |t| (true, t))
                | map!(terminated!(tag!("#EXT-X-MEDIA"), is_not!("-")), |t| (true, t)) // terminated!() to prevent matching with #EXT-X-MEDIA-SEQUENCE for which we have a separate pattern below
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
        >> consume_line
        >>
        ( {
            tag.map(|(a,b)| (a, from_utf8_slice(b).unwrap()))
        } )
    )
);

// -----------------------------------------------------------------------------------------------
// Master Playlist Tags
// -----------------------------------------------------------------------------------------------

pub fn parse_master_playlist_tags(input: &[u8]) ->  IResult<&[u8], Vec<MasterPlaylistTag>> {
    do_parse!(input,
         tags: many0!(complete!(do_parse!( m : master_playlist_tag >> multispace0 >> (m) )))
     >>  opt!(eof!())
     >>
        ( {let mut tags_rev: Vec<MasterPlaylistTag> = tags; tags_rev.reverse(); tags_rev } )
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
    do_parse!(tag!("#EXT-X-STREAM-INF:") >> attributes: key_value_pairs >>
           ( VariantStream::from_hashmap(attributes, false)))
);

named!(pub variant_i_frame_stream_tag<VariantStream>,
    do_parse!( tag!("#EXT-X-I-FRAME-STREAM-INF:") >> attributes: key_value_pairs >>
           ( VariantStream::from_hashmap(attributes, true)))
);

named!(pub alternative_media_tag<AlternativeMedia>,
    do_parse!( tag!("#EXT-X-MEDIA:") >> attributes: key_value_pairs >>
           ( AlternativeMedia::from_hashmap(attributes)))
);

named!(pub session_data_tag<SessionData>,
    do_parse!( tag!("#EXT-X-SESSION-DATA:") >> attributes: key_value_pairs >>
            ( SessionData::from_hashmap(attributes)))
);

named!(pub session_key_tag<SessionKey>,
    do_parse!( tag!("#EXT-X-SESSION-KEY:") >> session_key: map!(key, SessionKey) >>
            ( session_key))
);

// -----------------------------------------------------------------------------------------------
// Media Playlist
// -----------------------------------------------------------------------------------------------

pub fn parse_media_playlist_tags(input: &[u8]) -> IResult<&[u8], Vec<MediaPlaylistTag>> {
    do_parse!(input,
        tags: many0!(complete!(do_parse!(m:media_playlist_tag >> multispace0 >> (m) ))) >> opt!(eof!())
     >>
        ( {let mut tags_rev: Vec<MediaPlaylistTag> = tags; tags_rev.reverse(); tags_rev } )
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

    | map!(do_parse!(tag!("#EXT-X-TARGETDURATION:") >> n:float >> (n)), MediaPlaylistTag::TargetDuration)
    | map!(do_parse!(tag!("#EXT-X-MEDIA-SEQUENCE:") >> n:number >> (n)), MediaPlaylistTag::MediaSequence)
    | map!(do_parse!(tag!("#EXT-X-DISCONTINUITY-SEQUENCE:") >> n:number >> (n)), MediaPlaylistTag::DiscontinuitySequence)
    | map!(do_parse!(tag!("#EXT-X-PLAYLIST-TYPE:") >> t:playlist_type >> (t)), MediaPlaylistTag::PlaylistType)
    | map!(tag!("#EXT-X-I-FRAMES-ONLY"), |_| MediaPlaylistTag::IFramesOnly)
    | map!(start_tag, MediaPlaylistTag::Start)
    | map!(tag!("#EXT-X-INDEPENDENT-SEGMENTS"), |_| MediaPlaylistTag::IndependentSegments)
    | map!(tag!("#EXT-X-ENDLIST"), |_| MediaPlaylistTag::EndList)

    | map!(media_segment_tag, MediaPlaylistTag::Segment)
    )
}

named!(pub playlist_type<MediaPlaylistType>,
    map_res!(
        do_parse!(
            p: map_res!(is_not!("\r\n"), str::from_utf8)
            >> take!(1)
            >> (p)
        ),
        MediaPlaylistType::from_str
    )
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
      map!(do_parse!(tag!("#EXTINF:") >> e:duration_title_tag >> (e)), |(a,b)| SegmentTag::Extinf(a,b))
    | map!(do_parse!(tag!("#EXT-X-BYTERANGE:") >> r:byte_range_val >> (r)), SegmentTag::ByteRange)
    | map!(tag!("#EXT-X-DISCONTINUITY"), |_| SegmentTag::Discontinuity)
    | map!(do_parse!(tag!("#EXT-X-KEY:") >> k: key >> (k)), SegmentTag::Key)
    | map!(do_parse!(tag!("#EXT-X-MAP:") >> m: extmap >> (m)), SegmentTag::Map)
    | map!(do_parse!(tag!("#EXT-X-PROGRAM-DATE-TIME:") >> t:consume_line >> (t)), SegmentTag::ProgramDateTime)
    | map!(do_parse!(tag!("#EXT-X-DATE-RANGE:") >> t:consume_line >> (t)), SegmentTag::DateRange)

    | map!(ext_tag, SegmentTag::Unknown)
    | map!(comment_tag, SegmentTag::Comment)

    | map!(consume_line, SegmentTag::Uri)
    )
}

named!(pub duration_title_tag<(f32, Option<String>)>,
    do_parse!(
           duration: float
        >> opt!(tag!(","))
        >> title: opt!(map_res!(is_not!("\r\n,"), from_utf8_slice))
        >> take!(1)
        >> opt!(tag!(","))
        >>
        (duration, title)
    )
);

named!(pub key<Key>, map!(key_value_pairs, Key::from_hashmap));

named!(pub extmap<Map>, map!(key_value_pairs, Map::from_hashmap));

// -----------------------------------------------------------------------------------------------
// Basic tags
// -----------------------------------------------------------------------------------------------

named!(pub m3u_tag<String>,
     map_res!(tag!("#EXTM3U"), from_utf8_slice)
);

named!(pub version_tag<usize>,
    do_parse!(
        tag!("#EXT-X-VERSION:") >> version: map_res!(digit1, str::from_utf8) >>
        (version.parse().unwrap_or_default())
    )
);

named!(pub start_tag<Start>,
    do_parse!(tag!("#EXT-X-START:") >> attributes:key_value_pairs >>
        (Start::from_hashmap(attributes))
     )
);

named!(pub ext_tag<ExtTag>,
    do_parse!(
          tag!("#EXT-")
        >> tag: map_res!(take_until!(":"), from_utf8_slice)
        >> take!(1)
        >> rest: map_res!(is_not!("\r\n"), from_utf8_slice)
        >> take!(1)
        >>
        (ExtTag { tag: tag, rest: rest })
    )
);

named!(pub comment_tag<String>,
    do_parse!(
        tag!("#") >> text: map_res!(is_not!("\r\n"), from_utf8_slice)
        >> take!(1)
        >> (text)
    )
);

// -----------------------------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------------------------

named!(pub key_value_pairs(&[u8]) -> HashMap<String, String>,
    map!(
        many0!(do_parse!(space0 >> k:key_value_pair >> (k) ))
        ,
        |pairs: Vec<(String, String)>| {
            pairs.into_iter().collect()
        }
    )
);

named!(pub key_value_pair(&[u8]) -> (String, String),
    do_parse!(
          peek!(none_of!("\r\n"))
       >> left: map_res!(take_until!("="), from_utf8_slice)
       >> take!(1)
       >> right: alt!(quoted | unquoted)
       >> opt!(char!(','))
       >>
       (left, right)
    )
);

named!(pub quoted<String>,
    delimited!(char!('\"'), map_res!(is_not!("\""), from_utf8_slice), char!('\"'))
);

named!(pub unquoted<String>,
    map_res!(is_not!(",\r\n"), from_utf8_slice)
);

named!(pub consume_line<String>,
    do_parse!(
        line: map_res!(is_not!("\r\n"), from_utf8_slice)
        >> line_ending
        >> (line)
    )
);

named!(pub number<i32>,
    map_res!(map_res!(digit1, str::from_utf8), str::FromStr::from_str)
);

named!(pub byte_range_val<ByteRange>,
    do_parse!(
          n: number
       >> o: opt!(do_parse!(char!('@') >> n:number >> (n) )) >>
        (ByteRange { length: n, offset: o })
    )
);

named!(pub float<f32>,
    do_parse!(
               left: map_res!(digit1, str::from_utf8)
       >> right_opt: opt!(do_parse!(char!('.') >> d:map_res!(digit1, str::from_utf8) >> (d) ))
       >>
        (
        match right_opt {
            Some(right) => {
                let mut num = String::from(left);
                num.push('.');
                num.push_str(right);
                num.parse().unwrap()
            },
            None => left.parse().unwrap(),
        })
    )
);

pub fn from_utf8_slice(s: &[u8]) -> Result<String, string::FromUtf8Error> {
    String::from_utf8(s.to_vec())
}

pub fn from_utf8_slice2(s: &[u8]) -> Result<String, str::Utf8Error> {
    str::from_utf8(s).map(String::from)
}
