//! A library to parse m3u8 playlists (HTTP Live Streaming) [link]
//! (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19).
//!
//! # Examples
//!
//! Parsing a playlist and let the parser figure out if it's a media or master playlist.
//!
//! ```
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

use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take, take_until, take_while1};
use nom::character::complete::{
    char, digit1, line_ending, multispace0, none_of, not_line_ending, space0,
};
use nom::character::is_digit;
use nom::combinator::{complete, eof, map, map_res, opt, peek};
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};

use crate::playlist::*;
use nom::IResult;
use std::collections::HashMap;
use std::f32;
use std::result::Result;
use std::str;
use std::str::FromStr;
use std::string;

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
        false => map(parse_media_playlist, Playlist::MediaPlaylist)(input),
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
    map(
        pair(
            complete(pair(m3u_tag, multispace0)),
            parse_master_playlist_tags,
        ),
        |(_, tags)| master_playlist_from_tags(tags),
    )(input)
}

/// Parse input as a master playlist
pub fn parse_master_playlist_res(
    input: &[u8],
) -> Result<MasterPlaylist, IResult<&[u8], MasterPlaylist>> {
    let parse_result = parse_master_playlist(input);
    match parse_result {
        IResult::Ok((_, playlist)) => Ok(playlist),
        _ => Err(parse_result),
    }
}

/// Parse input as a media playlist
pub fn parse_media_playlist(input: &[u8]) -> IResult<&[u8], MediaPlaylist> {
    map(
        pair(
            complete(pair(m3u_tag, multispace0)),
            parse_media_playlist_tags,
        ),
        |(_, tags)| media_playlist_from_tags(tags),
    )(input)
}

/// Parse input as a media playlist
pub fn parse_media_playlist_res(
    input: &[u8],
) -> Result<MediaPlaylist, IResult<&[u8], MediaPlaylist>> {
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

pub fn is_master_playlist_tag_line(i: &[u8]) -> IResult<&[u8], Option<(bool, String)>> {
    map(
        tuple((
            opt(is_a("\r\n")),
            opt(alt((
                map(tag("#EXT-X-STREAM-INF"), |t| (true, t)),
                map(tag("#EXT-X-I-FRAME-STREAM-INF"), |t| (true, t)),
                map(terminated(tag("#EXT-X-MEDIA"), is_not("-")), |t| (true, t)), // terminated() to prevent matching with #EXT-X-MEDIA-SEQUENCE for which we have a separate pattern below
                map(tag("#EXT-X-SESSION-KEY"), |t| (true, t)),
                map(tag("#EXT-X-SESSION-DATA"), |t| (true, t)),
                map(tag("#EXT-X-TARGETDURATION"), |t| (false, t)),
                map(tag("#EXT-X-MEDIA-SEQUENCE"), |t| (false, t)),
                map(tag("#EXT-X-DISCONTINUITY-SEQUENCE"), |t| (false, t)),
                map(tag("#EXT-X-ENDLIST"), |t| (false, t)),
                map(tag("#EXT-X-PLAYLIST-TYPE"), |t| (false, t)),
                map(tag("#EXT-X-I-FRAMES-ONLY"), |t| (false, t)),
                map(tag("#EXTINF"), |t| (false, t)),
                map(tag("#EXT-X-BYTERANGE"), |t| (false, t)),
                map(tag("#EXT-X-DISCONTINUITY"), |t| (false, t)),
                map(tag("#EXT-X-KEY"), |t| (false, t)),
                map(tag("#EXT-X-MAP"), |t| (false, t)),
                map(tag("#EXT-X-PROGRAM-DATE-TIME"), |t| (false, t)),
                map(tag("#EXT-X-DATERANGE"), |t| (false, t)),
            ))),
            consume_line,
        )),
        |(_, tag, _)| tag.map(|(a, b)| (a, from_utf8_slice(b).unwrap())),
    )(i)
}

// -----------------------------------------------------------------------------------------------
// Master Playlist Tags
// -----------------------------------------------------------------------------------------------

pub fn parse_master_playlist_tags(i: &[u8]) -> IResult<&[u8], Vec<MasterPlaylistTag>> {
    map(
        tuple((
            many0(complete(map(
                pair(master_playlist_tag, multispace0),
                |(tag, _)| tag,
            ))),
            opt(eof),
        )),
        |(tags, _)| {
            let mut tags_rev: Vec<MasterPlaylistTag> = tags;
            tags_rev.reverse();
            tags_rev
        },
    )(i)
}

/// Contains all the tags required to parse a master playlist.
#[derive(Debug)]
pub enum MasterPlaylistTag {
    Version(usize),
    VariantStream(VariantStream),
    AlternativeMedia(AlternativeMedia),
    SessionData(SessionData),
    SessionKey(SessionKey),
    Start(Start),
    IndependentSegments,
    Comment(String),
    Uri(String),
    Unknown(ExtTag),
}

pub fn master_playlist_tag(i: &[u8]) -> IResult<&[u8], MasterPlaylistTag> {
    // Don't accept empty inputs here
    peek(take(1usize))(i)?;

    alt((
        map(version_tag, MasterPlaylistTag::Version),
        map(variant_stream_tag, MasterPlaylistTag::VariantStream),
        map(variant_i_frame_stream_tag, MasterPlaylistTag::VariantStream),
        map(alternative_media_tag, MasterPlaylistTag::AlternativeMedia),
        map(session_data_tag, MasterPlaylistTag::SessionData),
        map(session_key_tag, MasterPlaylistTag::SessionKey),
        map(start_tag, MasterPlaylistTag::Start),
        map(tag("#EXT-X-INDEPENDENT-SEGMENTS"), |_| {
            MasterPlaylistTag::IndependentSegments
        }),
        map(ext_tag, MasterPlaylistTag::Unknown),
        map(comment_tag, MasterPlaylistTag::Comment),
        map(consume_line, MasterPlaylistTag::Uri),
    ))(i)
}

pub fn master_playlist_from_tags(mut tags: Vec<MasterPlaylistTag>) -> MasterPlaylist {
    let mut master_playlist = MasterPlaylist::default();

    while let Some(tag) = tags.pop() {
        match tag {
            MasterPlaylistTag::Version(v) => {
                master_playlist.version = v;
            }
            MasterPlaylistTag::AlternativeMedia(v) => {
                master_playlist.alternatives.push(v);
            }
            MasterPlaylistTag::VariantStream(stream) => {
                master_playlist.variants.push(stream);
            }
            MasterPlaylistTag::Uri(uri) => {
                if let Some(stream) = master_playlist.get_newest_variant() {
                    stream.uri = uri;
                }
            }
            MasterPlaylistTag::SessionData(data) => {
                master_playlist.session_data.push(data);
            }
            MasterPlaylistTag::SessionKey(key) => {
                master_playlist.session_key.push(key);
            }
            MasterPlaylistTag::Start(s) => {
                master_playlist.start = Some(s);
            }
            MasterPlaylistTag::IndependentSegments => {
                master_playlist.independent_segments = true;
            }
            MasterPlaylistTag::Unknown(unknown) => {
                master_playlist.unknown_tags.push(unknown);
            }
            _ => (),
        }
    }

    master_playlist
}

pub fn variant_stream_tag(i: &[u8]) -> IResult<&[u8], VariantStream> {
    map(
        pair(tag("#EXT-X-STREAM-INF:"), key_value_pairs),
        |(_, attributes)| VariantStream::from_hashmap(attributes, false),
    )(i)
}

pub fn variant_i_frame_stream_tag(i: &[u8]) -> IResult<&[u8], VariantStream> {
    map(
        pair(tag("#EXT-X-I-FRAME-STREAM-INF:"), key_value_pairs),
        |(_, attributes)| VariantStream::from_hashmap(attributes, true),
    )(i)
}

pub fn alternative_media_tag(i: &[u8]) -> IResult<&[u8], AlternativeMedia> {
    map(pair(tag("#EXT-X-MEDIA:"), key_value_pairs), |(_, media)| {
        AlternativeMedia::from_hashmap(media)
    })(i)
}

pub fn session_data_tag(i: &[u8]) -> IResult<&[u8], SessionData> {
    map_res(
        pair(tag("#EXT-X-SESSION-DATA:"), key_value_pairs),
        |(_, session_data)| SessionData::from_hashmap(session_data),
    )(i)
}

pub fn session_key_tag(i: &[u8]) -> IResult<&[u8], SessionKey> {
    map(pair(tag("#EXT-X-SESSION-KEY:"), key), |(_, key)| {
        SessionKey(key)
    })(i)
}

// -----------------------------------------------------------------------------------------------
// Media Playlist
// -----------------------------------------------------------------------------------------------

pub fn parse_media_playlist_tags(i: &[u8]) -> IResult<&[u8], Vec<MediaPlaylistTag>> {
    map(
        tuple((
            many0(complete(map(
                pair(media_playlist_tag, multispace0),
                |(tag, _)| tag,
            ))),
            opt(eof),
        )),
        |(tags, _)| {
            let mut tags_rev: Vec<MediaPlaylistTag> = tags;
            tags_rev.reverse();
            tags_rev
        },
    )(i)
}

/// Contains all the tags required to parse a media playlist.
#[derive(Debug)]
pub enum MediaPlaylistTag {
    Version(usize),
    Segment(SegmentTag),
    TargetDuration(f32),
    MediaSequence(i32),
    DiscontinuitySequence(i32),
    EndList,
    PlaylistType(MediaPlaylistType),
    IFramesOnly,
    Start(Start),
    Unknown(ExtTag),
    IndependentSegments,
}

pub fn media_playlist_tag(i: &[u8]) -> IResult<&[u8], MediaPlaylistTag> {
    // Don't accept empty inputs here
    peek(take(1usize))(i)?;

    alt((
        map(version_tag, MediaPlaylistTag::Version),
        map(
            pair(tag("#EXT-X-TARGETDURATION:"), float),
            |(_, duration)| MediaPlaylistTag::TargetDuration(duration),
        ),
        map(
            pair(tag("#EXT-X-MEDIA-SEQUENCE:"), number),
            |(_, sequence)| MediaPlaylistTag::MediaSequence(sequence),
        ),
        map(
            pair(tag("#EXT-X-DISCONTINUITY-SEQUENCE:"), number),
            |(_, sequence)| MediaPlaylistTag::DiscontinuitySequence(sequence),
        ),
        map(
            pair(tag("#EXT-X-PLAYLIST-TYPE:"), playlist_type),
            |(_, typ)| MediaPlaylistTag::PlaylistType(typ),
        ),
        map(tag("#EXT-X-I-FRAMES-ONLY"), |_| {
            MediaPlaylistTag::IFramesOnly
        }),
        map(start_tag, MediaPlaylistTag::Start),
        map(tag("#EXT-X-INDEPENDENT-SEGMENTS"), |_| {
            MediaPlaylistTag::IndependentSegments
        }),
        map(tag("#EXT-X-ENDLIST"), |_| MediaPlaylistTag::EndList),
        map(media_segment_tag, MediaPlaylistTag::Segment),
    ))(i)
}

pub fn media_playlist_from_tags(mut tags: Vec<MediaPlaylistTag>) -> MediaPlaylist {
    let mut media_playlist = MediaPlaylist::default();
    let mut next_segment = MediaSegment::empty();
    let mut encryption_key = None;
    let mut map = None;

    while let Some(tag) = tags.pop() {
        match tag {
            MediaPlaylistTag::Version(v) => {
                media_playlist.version = v;
            }
            MediaPlaylistTag::TargetDuration(d) => {
                media_playlist.target_duration = d;
            }
            MediaPlaylistTag::MediaSequence(n) => {
                media_playlist.media_sequence = n;
            }
            MediaPlaylistTag::DiscontinuitySequence(n) => {
                media_playlist.discontinuity_sequence = n;
            }
            MediaPlaylistTag::EndList => {
                media_playlist.end_list = true;
            }
            MediaPlaylistTag::PlaylistType(t) => {
                media_playlist.playlist_type = Some(t);
            }
            MediaPlaylistTag::IFramesOnly => {
                media_playlist.i_frames_only = true;
            }
            MediaPlaylistTag::Start(s) => {
                media_playlist.start = Some(s);
            }
            MediaPlaylistTag::IndependentSegments => {
                media_playlist.independent_segments = true;
            }
            MediaPlaylistTag::Segment(segment_tag) => match segment_tag {
                SegmentTag::Extinf(d, t) => {
                    next_segment.duration = d;
                    next_segment.title = t;
                }
                SegmentTag::ByteRange(b) => {
                    next_segment.byte_range = Some(b);
                }
                SegmentTag::Discontinuity => {
                    next_segment.discontinuity = true;
                }
                SegmentTag::Key(k) => {
                    encryption_key = Some(k);
                }
                SegmentTag::Map(m) => {
                    map = Some(m);
                }
                SegmentTag::ProgramDateTime(d) => {
                    next_segment.program_date_time = Some(d);
                }
                SegmentTag::DateRange(d) => {
                    next_segment.daterange = Some(d);
                }
                SegmentTag::Unknown(t) => {
                    next_segment.unknown_tags.push(t);
                }
                SegmentTag::Uri(u) => {
                    next_segment.key = encryption_key.clone();
                    next_segment.map = map.clone();
                    next_segment.uri = u;
                    media_playlist.segments.push(next_segment);
                    next_segment = MediaSegment::empty();
                    encryption_key = None;
                    map = None;
                }
                _ => (),
            },
            _ => (),
        }
    }
    media_playlist
}

pub fn playlist_type(i: &[u8]) -> IResult<&[u8], MediaPlaylistType> {
    map_res(
        tuple((map_res(is_not("\r\n"), str::from_utf8), take(1usize))),
        |(typ, _)| MediaPlaylistType::from_str(typ),
    )(i)
}

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

pub fn media_segment_tag(i: &[u8]) -> IResult<&[u8], SegmentTag> {
    alt((
        map(
            pair(tag("#EXTINF:"), duration_title_tag),
            |(_, (duration, title))| SegmentTag::Extinf(duration, title),
        ),
        map(
            pair(tag("#EXT-X-BYTERANGE:"), byte_range_val),
            |(_, range)| SegmentTag::ByteRange(range),
        ),
        map(tag("#EXT-X-DISCONTINUITY"), |_| SegmentTag::Discontinuity),
        map(pair(tag("#EXT-X-KEY:"), key), |(_, key)| {
            SegmentTag::Key(key)
        }),
        map(pair(tag("#EXT-X-MAP:"), extmap), |(_, map)| {
            SegmentTag::Map(map)
        }),
        map(
            pair(tag("#EXT-X-PROGRAM-DATE-TIME:"), consume_line),
            |(_, line)| SegmentTag::ProgramDateTime(line),
        ),
        map(
            pair(tag("#EXT-X-DATE-RANGE:"), consume_line),
            |(_, line)| SegmentTag::DateRange(line),
        ),
        map(ext_tag, SegmentTag::Unknown),
        map(comment_tag, SegmentTag::Comment),
        map(consume_line, SegmentTag::Uri),
    ))(i)
}

pub fn duration_title_tag(i: &[u8]) -> IResult<&[u8], (f32, Option<String>)> {
    map(
        tuple((
            float,
            opt(char(',')),
            opt(map_res(is_not("\r\n,"), from_utf8_slice)),
            take(1usize),
            opt(char(',')),
        )),
        |(duration, _, title, _, _)| (duration, title),
    )(i)
}

pub fn key(i: &[u8]) -> IResult<&[u8], Key> {
    map(key_value_pairs, Key::from_hashmap)(i)
}

pub fn extmap(i: &[u8]) -> IResult<&[u8], Map> {
    map_res(key_value_pairs, |attrs| -> Result<Map, &str> {
        let uri = attrs.get("URI").cloned().unwrap_or_default();
        let byte_range = attrs
            .get("BYTERANGE")
            .map(|range| match byte_range_val(range.as_bytes()) {
                IResult::Ok((_, range)) => Ok(range),
                IResult::Err(_) => Err("invalid byte range"),
            })
            .transpose()?;

        Ok(Map { uri, byte_range })
    })(i)
}

// -----------------------------------------------------------------------------------------------
// Basic tags
// -----------------------------------------------------------------------------------------------

pub fn m3u_tag(i: &[u8]) -> IResult<&[u8], ()> {
    map(tag("#EXTM3U"), |_| ())(i)
}

pub fn version_tag(i: &[u8]) -> IResult<&[u8], usize> {
    map(
        pair(tag("#EXT-X-VERSION:"), map_res(digit1, str::from_utf8)),
        |(_, version)| version.parse().unwrap_or_default(),
    )(i)
}

pub fn start_tag(i: &[u8]) -> IResult<&[u8], Start> {
    map(
        pair(tag("#EXT-X-START:"), key_value_pairs),
        |(_, attributes)| Start::from_hashmap(attributes),
    )(i)
}

pub fn ext_tag(i: &[u8]) -> IResult<&[u8], ExtTag> {
    map(
        tuple((
            tag("#EXT-"),
            map_res(is_not("\r\n:"), from_utf8_slice),
            opt(char(':')),
            opt(map_res(is_not("\r\n"), from_utf8_slice)),
            take(1usize),
        )),
        |(_, tag, _, rest, _)| ExtTag { tag, rest },
    )(i)
}

pub fn comment_tag(i: &[u8]) -> IResult<&[u8], String> {
    map(
        pair(
            preceded(char('#'), map_res(is_not("\r\n"), from_utf8_slice)),
            take(1usize),
        ),
        |(text, _)| text,
    )(i)
}

// -----------------------------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------------------------

pub fn key_value_pairs(i: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    fold_many0(
        preceded(space0, key_value_pair),
        HashMap::new,
        |mut acc: HashMap<_, _>, (left, right)| {
            acc.insert(left, right);
            acc
        },
    )(i)
}

pub fn key_value_pair(i: &[u8]) -> IResult<&[u8], (String, String)> {
    map(
        tuple((
            peek(none_of("\r\n")),
            map_res(take_until("="), from_utf8_slice),
            char('='),
            alt((quoted, unquoted)),
            opt(char(',')),
        )),
        |(_, left, _, right, _)| (left, right),
    )(i)
}

pub fn quoted(i: &[u8]) -> IResult<&[u8], String> {
    delimited(
        char('\"'),
        map_res(is_not("\""), from_utf8_slice),
        char('\"'),
    )(i)
}

pub fn unquoted(i: &[u8]) -> IResult<&[u8], String> {
    map_res(is_not(",\r\n"), from_utf8_slice)(i)
}

pub fn consume_line(i: &[u8]) -> IResult<&[u8], String> {
    map(
        pair(map_res(not_line_ending, from_utf8_slice), opt(line_ending)),
        |(line, _)| line,
    )(i)
}

pub fn number(i: &[u8]) -> IResult<&[u8], i32> {
    map_res(take_while1(is_digit), |s| {
        // Can't fail because we validated it above already
        let s = str::from_utf8(s).unwrap();
        str::parse::<i32>(s)
    })(i)
}

pub fn byte_range_val(i: &[u8]) -> IResult<&[u8], ByteRange> {
    map(pair(number, opt(preceded(char('@'), number))), |(n, o)| {
        ByteRange {
            length: n,
            offset: o,
        }
    })(i)
}

pub fn float(i: &[u8]) -> IResult<&[u8], f32> {
    map_res(
        pair(
            take_while1(is_digit),
            opt(preceded(char('.'), take_while1(is_digit))),
        ),
        |(left, right): (&[u8], Option<&[u8]>)| match right {
            Some(right) => {
                let n = &i[..(left.len() + right.len() + 1)];
                // Can't fail because we validated it above already
                let n = str::from_utf8(n).unwrap();
                n.parse()
            }
            None => {
                // Can't fail because we validated it above already
                let left = str::from_utf8(left).unwrap();
                left.parse()
            }
        },
    )(i)
}

pub fn from_utf8_slice(s: &[u8]) -> Result<String, string::FromUtf8Error> {
    String::from_utf8(s.to_vec())
}
