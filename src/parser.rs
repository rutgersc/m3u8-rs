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
use std::fmt;
use std::fmt::Display;
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
/// use m3u8_rs::Playlist;
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
    m3u_tag(input)?;

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
/// use m3u8_rs::Playlist;
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
pub fn parse_playlist_res(input: &[u8]) -> Result<Playlist, nom::Err<nom::error::Error<&[u8]>>> {
    let parse_result = parse_playlist(input);
    match parse_result {
        IResult::Ok((_, playlist)) => Ok(playlist),
        IResult::Err(err) => Err(err),
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
) -> Result<MasterPlaylist, nom::Err<nom::error::Error<&[u8]>>> {
    let parse_result = parse_master_playlist(input);
    match parse_result {
        IResult::Ok((_, playlist)) => Ok(playlist),
        IResult::Err(err) => Err(err),
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
) -> Result<MediaPlaylist, nom::Err<nom::error::Error<&[u8]>>> {
    let parse_result = parse_media_playlist(input);
    match parse_result {
        IResult::Ok((_, playlist)) => Ok(playlist),
        IResult::Err(err) => Err(err),
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
/// - None: Unknown tag or empty line
/// - Some(true, tagstring): Line contains a master playlist tag
/// - Some(false, tagstring): Line contains a media playlist tag
fn contains_master_tag(input: &[u8]) -> Option<(bool, String)> {
    let (input, _) = m3u_tag(input).ok()?;
    let mut is_master_opt = None;
    let mut current_input: &[u8] = input;

    while is_master_opt.is_none() && !current_input.is_empty() {
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

fn is_master_playlist_tag_line(i: &[u8]) -> IResult<&[u8], Option<(bool, String)>> {
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

fn parse_master_playlist_tags(i: &[u8]) -> IResult<&[u8], Vec<MasterPlaylistTag>> {
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
#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
enum MasterPlaylistTag {
    Version(usize),
    VariantStream(VariantStream),
    AlternativeMedia(AlternativeMedia),
    SessionData(SessionData),
    SessionKey(SessionKey),
    Start(Start),
    IndependentSegments,
    Comment(Option<String>),
    Uri(String),
    Unknown(ExtTag),
}

fn master_playlist_tag(i: &[u8]) -> IResult<&[u8], MasterPlaylistTag> {
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

fn master_playlist_from_tags(mut tags: Vec<MasterPlaylistTag>) -> MasterPlaylist {
    let mut master_playlist = MasterPlaylist::default();

    while let Some(tag) = tags.pop() {
        match tag {
            MasterPlaylistTag::Version(v) => {
                master_playlist.version = Some(v);
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

fn variant_stream_tag(i: &[u8]) -> IResult<&[u8], VariantStream> {
    map_res(
        pair(tag("#EXT-X-STREAM-INF:"), key_value_pairs),
        |(_, attributes)| VariantStream::from_hashmap(attributes, false),
    )(i)
}

fn variant_i_frame_stream_tag(i: &[u8]) -> IResult<&[u8], VariantStream> {
    map_res(
        pair(tag("#EXT-X-I-FRAME-STREAM-INF:"), key_value_pairs),
        |(_, attributes)| VariantStream::from_hashmap(attributes, true),
    )(i)
}

fn alternative_media_tag(i: &[u8]) -> IResult<&[u8], AlternativeMedia> {
    map_res(pair(tag("#EXT-X-MEDIA:"), key_value_pairs), |(_, media)| {
        AlternativeMedia::from_hashmap(media)
    })(i)
}

fn session_data_tag(i: &[u8]) -> IResult<&[u8], SessionData> {
    map_res(
        pair(tag("#EXT-X-SESSION-DATA:"), key_value_pairs),
        |(_, session_data)| SessionData::from_hashmap(session_data),
    )(i)
}

fn session_key_tag(i: &[u8]) -> IResult<&[u8], SessionKey> {
    map(pair(tag("#EXT-X-SESSION-KEY:"), key), |(_, key)| {
        SessionKey(key)
    })(i)
}

// -----------------------------------------------------------------------------------------------
// Media Playlist
// -----------------------------------------------------------------------------------------------

fn parse_media_playlist_tags(i: &[u8]) -> IResult<&[u8], Vec<MediaPlaylistTag>> {
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
enum MediaPlaylistTag {
    Version(usize),
    Segment(SegmentTag),
    TargetDuration(u64),
    MediaSequence(u64),
    DiscontinuitySequence(u64),
    EndList,
    PlaylistType(MediaPlaylistType),
    IFramesOnly,
    Start(Start),
    IndependentSegments,
}

fn media_playlist_tag(i: &[u8]) -> IResult<&[u8], MediaPlaylistTag> {
    // Don't accept empty inputs here
    peek(take(1usize))(i)?;

    alt((
        map(version_tag, MediaPlaylistTag::Version),
        map(
            pair(tag("#EXT-X-TARGETDURATION:"), number),
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

fn media_playlist_from_tags(mut tags: Vec<MediaPlaylistTag>) -> MediaPlaylist {
    let mut media_playlist = MediaPlaylist::default();
    let mut next_segment = MediaSegment::empty();
    let mut encryption_key = None;
    let mut map = None;

    while let Some(tag) = tags.pop() {
        match tag {
            MediaPlaylistTag::Version(v) => {
                media_playlist.version = Some(v);
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
                SegmentTag::Map(mut m) => {
                    m.after_key = encryption_key.is_some();
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
        }
    }
    media_playlist
}

fn playlist_type(i: &[u8]) -> IResult<&[u8], MediaPlaylistType> {
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
enum SegmentTag {
    Extinf(f32, Option<String>),
    ByteRange(ByteRange),
    Discontinuity,
    Key(Key),
    Map(Map),
    ProgramDateTime(chrono::DateTime<chrono::FixedOffset>),
    DateRange(DateRange),
    Unknown(ExtTag),
    Comment(Option<String>),
    Uri(String),
}

fn media_segment_tag(i: &[u8]) -> IResult<&[u8], SegmentTag> {
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
            pair(tag("#EXT-X-PROGRAM-DATE-TIME:"), program_date_time),
            |(_, pdt)| SegmentTag::ProgramDateTime(pdt),
        ),
        map(pair(tag("#EXT-X-DATERANGE:"), daterange), |(_, range)| {
            SegmentTag::DateRange(range)
        }),
        map(ext_tag, SegmentTag::Unknown),
        map(comment_tag, SegmentTag::Comment),
        map(consume_line, SegmentTag::Uri),
    ))(i)
}

fn duration_title_tag(i: &[u8]) -> IResult<&[u8], (f32, Option<String>)> {
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

fn key(i: &[u8]) -> IResult<&[u8], Key> {
    map_res(key_value_pairs, Key::from_hashmap)(i)
}

fn program_date_time(i: &[u8]) -> IResult<&[u8], chrono::DateTime<chrono::FixedOffset>> {
    map_res(consume_line, |s| chrono::DateTime::parse_from_rfc3339(&s))(i)
}

fn daterange(i: &[u8]) -> IResult<&[u8], DateRange> {
    map_res(key_value_pairs, DateRange::from_hashmap)(i)
}

fn extmap(i: &[u8]) -> IResult<&[u8], Map> {
    map_res(key_value_pairs, |mut attrs| -> Result<Map, &str> {
        let uri = match attrs.remove("URI") {
            Some(QuotedOrUnquoted::Quoted(s)) => Ok(s),
            Some(QuotedOrUnquoted::Unquoted(_)) => {
                Err("Can't create URI attribute from unquoted string")
            }
            None => Err("URI is empty"),
        }?;
        let byte_range = match attrs.remove("BYTERANGE") {
            Some(QuotedOrUnquoted::Quoted(s)) => match byte_range_val(s.as_bytes()) {
                IResult::Ok((_, range)) => Ok(Some(range)),
                IResult::Err(_) => Err("Invalid byte range"),
            },
            Some(QuotedOrUnquoted::Unquoted(_)) => {
                Err("Can't create BYTERANGE attribute from unquoted string")
            }
            None => Ok(None),
        }?;

        Ok(Map {
            uri,
            byte_range,
            other_attributes: attrs,
            // Still not sure
            after_key: false,
        })
    })(i)
}

// -----------------------------------------------------------------------------------------------
// Basic tags
// -----------------------------------------------------------------------------------------------

fn m3u_tag(i: &[u8]) -> IResult<&[u8], ()> {
    map(tag("#EXTM3U"), |_| ())(i)
}

fn version_tag(i: &[u8]) -> IResult<&[u8], usize> {
    map(
        pair(tag("#EXT-X-VERSION:"), map_res(digit1, str::from_utf8)),
        |(_, version)| version.parse().unwrap_or_default(),
    )(i)
}

fn start_tag(i: &[u8]) -> IResult<&[u8], Start> {
    map_res(
        pair(tag("#EXT-X-START:"), key_value_pairs),
        |(_, attributes)| Start::from_hashmap(attributes),
    )(i)
}

fn ext_tag(i: &[u8]) -> IResult<&[u8], ExtTag> {
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

fn comment_tag(i: &[u8]) -> IResult<&[u8], Option<String>> {
    map(
        pair(
            preceded(char('#'), opt(map_res(is_not("\r\n"), from_utf8_slice))),
            take(1usize),
        ),
        |(text, _)| text,
    )(i)
}

// -----------------------------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------------------------

fn key_value_pairs(i: &[u8]) -> IResult<&[u8], HashMap<String, QuotedOrUnquoted>> {
    fold_many0(
        preceded(space0, key_value_pair),
        HashMap::new,
        |mut acc: HashMap<_, _>, (left, right)| {
            acc.insert(left, right);
            acc
        },
    )(i)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum QuotedOrUnquoted {
    Unquoted(String),
    Quoted(String),
}

impl Default for QuotedOrUnquoted {
    fn default() -> Self {
        QuotedOrUnquoted::Quoted(String::new())
    }
}

impl QuotedOrUnquoted {
    pub fn as_str(&self) -> &str {
        match self {
            QuotedOrUnquoted::Quoted(s) => s.as_str(),
            QuotedOrUnquoted::Unquoted(s) => s.as_str(),
        }
    }

    pub fn as_unquoted(&self) -> Option<&str> {
        match self {
            QuotedOrUnquoted::Unquoted(s) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn as_quoted(&self) -> Option<&str> {
        match self {
            QuotedOrUnquoted::Quoted(s) => Some(s.as_str()),
            _ => None,
        }
    }
}

impl From<&str> for QuotedOrUnquoted {
    fn from(s: &str) -> Self {
        if s.starts_with('"') && s.ends_with('"') {
            return QuotedOrUnquoted::Quoted(
                s.strip_prefix('"')
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or_default()
                    .to_string(),
            );
        }
        QuotedOrUnquoted::Unquoted(s.to_string())
    }
}

impl Display for QuotedOrUnquoted {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            QuotedOrUnquoted::Unquoted(s) => write!(f, "{}", s),
            QuotedOrUnquoted::Quoted(u) => write!(f, "\"{}\"", u),
        }
    }
}

fn key_value_pair(i: &[u8]) -> IResult<&[u8], (String, QuotedOrUnquoted)> {
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

fn quoted(i: &[u8]) -> IResult<&[u8], QuotedOrUnquoted> {
    delimited(
        char('\"'),
        map_res(is_not("\""), quoted_from_utf8_slice),
        char('\"'),
    )(i)
}

fn unquoted(i: &[u8]) -> IResult<&[u8], QuotedOrUnquoted> {
    map_res(is_not(",\r\n"), unquoted_from_utf8_slice)(i)
}

fn consume_line(i: &[u8]) -> IResult<&[u8], String> {
    map(
        pair(map_res(not_line_ending, from_utf8_slice), opt(line_ending)),
        |(line, _)| line,
    )(i)
}

fn number(i: &[u8]) -> IResult<&[u8], u64> {
    map_res(take_while1(is_digit), |s| {
        // Can't fail because we validated it above already
        let s = str::from_utf8(s).unwrap();
        str::parse::<u64>(s)
    })(i)
}

fn byte_range_val(i: &[u8]) -> IResult<&[u8], ByteRange> {
    map(pair(number, opt(preceded(char('@'), number))), |(n, o)| {
        ByteRange {
            length: n,
            offset: o,
        }
    })(i)
}

fn float(i: &[u8]) -> IResult<&[u8], f32> {
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

fn from_utf8_slice(s: &[u8]) -> Result<String, string::FromUtf8Error> {
    String::from_utf8(s.to_vec())
}

fn quoted_from_utf8_slice(s: &[u8]) -> Result<QuotedOrUnquoted, string::FromUtf8Error> {
    match String::from_utf8(s.to_vec()) {
        Ok(q) => Ok(QuotedOrUnquoted::Quoted(q)),
        Err(e) => Err(e),
    }
}

fn unquoted_from_utf8_slice(s: &[u8]) -> Result<QuotedOrUnquoted, string::FromUtf8Error> {
    match String::from_utf8(s.to_vec()) {
        Ok(q) => Ok(QuotedOrUnquoted::Unquoted(q)),
        Err(e) => Err(e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::AsBytes;

    // -----------------------------------------------------------------------------------------------
    // Variant

    #[test]
    fn variant_stream() {
        let input = b"#EXT-X-STREAM-INF:BANDWIDTH=300000,CODECS=\"xxx\"\n";
        assert_eq!(
            variant_stream_tag(input),
            Result::Ok((
                "\n".as_bytes(),
                VariantStream {
                    is_i_frame: false,
                    uri: "".into(),
                    bandwidth: 300000,
                    average_bandwidth: None,
                    codecs: Some("xxx".into()),
                    resolution: None,
                    frame_rate: None,
                    hdcp_level: None,
                    audio: None,
                    video: None,
                    subtitles: None,
                    closed_captions: None,
                    other_attributes: Default::default(),
                }
            ))
        );
    }

    // -----------------------------------------------------------------------------------------------
    // Other

    #[test]
    fn test_key_value_pairs_trailing_equals() {
        assert_eq!(
            key_value_pairs(b"BANDWIDTH=395000,CODECS=\"avc1.4d001f,mp4a.40.2\"\r\nrest="),
            Result::Ok((
                "\r\nrest=".as_bytes(),
                vec![
                    ("BANDWIDTH", "395000"),
                    ("CODECS", "\"avc1.4d001f,mp4a.40.2\"")
                ]
                .into_iter()
                .map(|(k, v)| (String::from(k), v.into()))
                .collect::<HashMap<_, _>>(),
            )),
        );
    }

    #[test]
    fn test_key_value_pairs_multiple_quoted_values() {
        assert_eq!(
            key_value_pairs(b"BANDWIDTH=86000,URI=\"low/iframe.m3u8\",PROGRAM-ID=1,RESOLUTION=\"1x1\",VIDEO=1\nrest"),
            Result::Ok((
                "\nrest".as_bytes(),
                vec![
                    ("BANDWIDTH", "86000"),
                    ("URI", "\"low/iframe.m3u8\""),
                    ("PROGRAM-ID", "1"),
                    ("RESOLUTION", "\"1x1\""),
                    ("VIDEO", "1")
                ].into_iter()
                    .map(|(k, v)| (String::from(k), v.into()))
                    .collect::<HashMap<_,_>>()
            ))
        );
    }

    #[test]
    fn test_key_value_pairs_quotes() {
        assert_eq!(
            key_value_pairs(b"BANDWIDTH=300000,CODECS=\"avc1.42c015,mp4a.40.2\"\r\nrest"),
            Result::Ok((
                "\r\nrest".as_bytes(),
                vec![
                    ("BANDWIDTH", "300000"),
                    ("CODECS", "\"avc1.42c015,mp4a.40.2\"")
                ]
                .into_iter()
                .map(|(k, v)| (String::from(k), v.into()))
                .collect::<HashMap<_, _>>()
            ))
        );
    }

    #[test]
    fn test_key_value_pairs() {
        assert_eq!(
            key_value_pairs(b"BANDWIDTH=300000,RESOLUTION=22x22,VIDEO=1\r\nrest="),
            Result::Ok((
                "\r\nrest=".as_bytes(),
                vec![
                    ("BANDWIDTH", "300000"),
                    ("RESOLUTION", "22x22"),
                    ("VIDEO", "1")
                ]
                .into_iter()
                .map(|(k, v)| (String::from(k), v.into()))
                .collect::<HashMap<_, _>>()
            ))
        );
    }

    #[test]
    fn test_key_value_pair() {
        assert_eq!(
            key_value_pair(b"PROGRAM-ID=1,rest"),
            Result::Ok(("rest".as_bytes(), ("PROGRAM-ID".to_string(), "1".into())))
        );
    }

    #[test]
    fn ext_with_value() {
        assert_eq!(
            ext_tag(b"#EXT-X-CUE-OUT:DURATION=30\nxxx"),
            Result::Ok((
                b"xxx".as_bytes(),
                ExtTag {
                    tag: "X-CUE-OUT".into(),
                    rest: Some("DURATION=30".into())
                }
            ))
        );
    }

    #[test]
    fn ext_without_value() {
        assert_eq!(
            ext_tag(b"#EXT-X-CUE-IN\nxxx"),
            Result::Ok((
                b"xxx".as_bytes(),
                ExtTag {
                    tag: "X-CUE-IN".into(),
                    rest: None
                }
            ))
        );
    }

    #[test]
    fn comment() {
        assert_eq!(
            comment_tag(b"#Hello\nxxx"),
            Result::Ok(("xxx".as_bytes(), Some("Hello".to_string())))
        );
    }

    #[test]
    fn empty_comment() {
        assert_eq!(comment_tag(b"#\nxxx"), Result::Ok(("xxx".as_bytes(), None)));
    }

    #[test]
    fn quotes() {
        assert_eq!(
            quoted(b"\"value\"rest"),
            Result::Ok(("rest".as_bytes(), "\"value\"".into()))
        );
    }

    #[test]
    fn consume_line_empty() {
        let expected = Result::Ok(("rest".as_bytes(), "".to_string()));
        let actual = consume_line(b"\r\nrest");
        assert_eq!(expected, actual);
    }

    #[test]
    fn consume_line_n() {
        assert_eq!(
            consume_line(b"before\nrest"),
            Result::Ok(("rest".as_bytes(), "before".into()))
        );
    }

    #[test]
    fn consume_line_rn() {
        assert_eq!(
            consume_line(b"before\r\nrest"),
            Result::Ok(("rest".as_bytes(), "before".into()))
        );
    }

    #[test]
    fn float_() {
        assert_eq!(
            float(b"33.22rest"),
            Result::Ok(("rest".as_bytes(), 33.22f32))
        );
    }

    #[test]
    fn float_no_decimal() {
        assert_eq!(float(b"33rest"), Result::Ok(("rest".as_bytes(), 33f32)));
    }

    #[test]
    fn float_should_ignore_trailing_dot() {
        assert_eq!(float(b"33.rest"), Result::Ok((".rest".as_bytes(), 33f32)));
    }

    #[test]
    fn parse_duration_title() {
        assert_eq!(
            duration_title_tag(b"2.002,title\nrest"),
            Result::Ok(("rest".as_bytes(), (2.002f32, Some("title".to_string()))))
        );
    }

    #[test]
    fn incomplete_manifest() {
        assert!(!is_master_playlist(
            "#EXTM3U\n#EXT-X-VERSION:5\n#EXT-X-TARGETDU".as_bytes()
        ));
    }
}
