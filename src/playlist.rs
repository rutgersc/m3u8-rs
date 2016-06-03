//! Contains all the structs used for parsing.
//!
//! The main type here is the `Playlist` enum.
//! Which is either a `MasterPlaylist` or a `MediaPlaylist`.

use std::collections::HashMap;
use std::str::FromStr;
use std::fmt;
use super::*;
use std::f32;

/// [Playlist](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.1),
/// can either be a `MasterPlaylist` or a `MediaPlaylist`.
///
/// A Playlist is a Media Playlist if all URI lines in the Playlist
/// identify Media Segments.  A Playlist is a Master Playlist if all URI
/// lines in the Playlist identify Media Playlists.  A Playlist MUST be
/// either a Media Playlist or a Master Playlist; all other Playlists are invalid.
#[derive(Debug)]
pub enum Playlist {
    MasterPlaylist(MasterPlaylist),
    MediaPlaylist(MediaPlaylist),
}

// -----------------------------------------------------------------------------------------------
// Master Playlist
// -----------------------------------------------------------------------------------------------

/// A [Master Playlist]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4)
/// provides a set of Variant Streams, each of which
/// describes a different version of the same content.
#[derive(Debug, Default)]
pub struct MasterPlaylist {
    pub version: usize,
    pub variants: Vec<VariantStream>,
    pub session_data: Option<SessionData>,
    pub session_key: Option<SessionKey>,
    pub start: Option<Start>,
    pub independent_segments: bool,
}

impl MasterPlaylist {
    pub fn from_tags(mut tags: Vec<MasterPlaylistTag>) -> MasterPlaylist {
        let mut master_playlist = MasterPlaylist::default();
        let mut alternatives = vec![];

        // println!("Creating master playlist from:", );
        while let Some(tag) = tags.pop() {
            // println!(" {:?}", tag );
            match tag {
                MasterPlaylistTag::Version(v) => {
                    master_playlist.version = v;
                }
                MasterPlaylistTag::AlternativeMedia(v) => {
                    alternatives.push(v);
                }
                MasterPlaylistTag::VariantStream(mut stream) => {
                    stream.alternatives = alternatives;
                    alternatives = vec![];
                    master_playlist.variants.push(stream);
                }
                MasterPlaylistTag::Uri(uri) => {
                    if let Some(stream) = master_playlist.get_newest_variant() {
                        stream.uri = uri;
                    }
                }
                MasterPlaylistTag::SessionData(data) => {
                    master_playlist.session_data = Some(data);
                }
                MasterPlaylistTag::SessionKey(key) => {
                    master_playlist.session_key = Some(key);
                }
                MasterPlaylistTag::Start(s) => {
                    master_playlist.start = Some(s);
                }
                MasterPlaylistTag::IndependentSegments => {
                    master_playlist.independent_segments = true;
                }
                MasterPlaylistTag::Unknown(_) => {
                    // println!("Unknown master tag \n{:?}\n", t);
                }
                _ => (),
            }
        }

        master_playlist
    }

    fn get_newest_variant(&mut self) -> Option<&mut VariantStream> {
        self.variants.iter_mut().rev().find(|v| !v.is_i_frame)
    }
}

/// [`#EXT-X-STREAM-INF:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.2)
/// [`#EXT-X-I-FRAME-STREAM-INF:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.3)
///
/// A Variant Stream includes a Media Playlist that specifies media
/// encoded at a particular bit rate, in a particular format, and at a
/// particular resolution for media containing video.
///
/// A Variant Stream can also specify a set of Renditions.  Renditions
/// are alternate versions of the content, such as audio produced in
/// different languages or video recorded from different camera angles.
///
/// Clients should switch between different Variant Streams to adapt to
/// network conditions.  Clients should choose Renditions based on user
/// preferences.
#[derive(Debug, Default)]
pub struct VariantStream {
    pub is_i_frame: bool,
    pub uri: String,

    // <attribute-list>
    pub bandwidth: String,
    pub average_bandwidth: Option<String>,
    pub codecs: String,
    pub resolution: Option<String>,
    pub frame_rate: Option<String>,
    pub audio: Option<String>,
    pub video: Option<String>,
    pub subtitles: Option<String>,
    pub closed_captions: Option<String>,
    // PROGRAM-ID tag was removed in protocol version 6
    pub alternatives: Vec<AlternativeMedia>, // EXT-X-MEDIA tags
}

impl VariantStream {
    pub fn from_hashmap(mut attrs: HashMap<String, String>, is_i_frame: bool) -> VariantStream {
        VariantStream {
            is_i_frame: is_i_frame,
            uri: attrs.remove("URI").unwrap_or_else(String::new),
            bandwidth: attrs.remove("BANDWIDTH").unwrap_or_else(String::new),
            average_bandwidth: attrs.remove("AVERAGE-BANDWIDTH"),
            codecs: attrs.remove("CODECS").unwrap_or_else(String::new),
            resolution: attrs.remove("RESOLUTION"),
            frame_rate: attrs.remove("FRAME-RATE"),
            audio: attrs.remove("AUDIO"),
            video: attrs.remove("VIDEO"),
            subtitles: attrs.remove("SUBTITLES"),
            closed_captions: attrs.remove("CLOSED-CAPTIONS"),
            alternatives: vec![],
        }
    }
}

/// [`#EXT-X-MEDIA:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.1)
///
/// The EXT-X-MEDIA tag is used to relate Media Playlists that contain
/// alternative Renditions (Section 4.3.4.2.1) of the same content.  For
/// example, three EXT-X-MEDIA tags can be used to identify audio-only
/// Media Playlists that contain English, French and Spanish Renditions
/// of the same presentation.  Or two EXT-X-MEDIA tags can be used to
/// identify video-only Media Playlists that show two different camera angles.
#[derive(Debug, Default)]
pub struct AlternativeMedia {
    // <attribute-list>
    pub media_type: AlternativeMediaType,
    pub uri: Option<String>,
    pub group_id: String,
    pub language: Option<String>,
    pub assoc_language: Option<String>,
    pub name: String, // All EXT-X-MEDIA tags in the same Group MUST have different NAME attributes.
    pub default: bool, // Its absence indicates an implicit value of NO
    pub autoselect: bool, // Its absence indicates an implicit value of NO
    pub forced: bool, // Its absence indicates an implicit value of NO
    pub instream_id: Option<String>,
    pub characteristics: Option<String>,
}

impl AlternativeMedia {
    pub fn from_hashmap(mut attrs: HashMap<String, String>) -> AlternativeMedia {
        AlternativeMedia {
            media_type: attrs.get("TYPE")
                .and_then(|s| AlternativeMediaType::from_str(s).ok())
                .unwrap_or_else(Default::default),
            uri: attrs.remove("URI"),
            group_id: attrs.remove("GROUP-ID").unwrap_or_else(String::new),
            language: attrs.remove("LANGUAGE"),
            assoc_language: attrs.remove("ASSOC-LANGUAGE"),
            name: attrs.remove("NAME").unwrap_or(String::new()),
            default: bool_default_false(attrs.remove("DEFAULT")),
            autoselect: bool_default_false(attrs.remove("ASSOC-LANGUAGE")),
            forced: bool_default_false(attrs.remove("ASSOC-LANGUAGE")),
            instream_id: attrs.remove("INSTREAM-ID"),
            characteristics: attrs.remove("CHARACTERISTICS"),
        }
    }
}

#[derive(Debug)]
pub enum AlternativeMediaType {
    Audio,
    Video,
    Subtitles,
    ClosedCaptions,
}

impl FromStr for AlternativeMediaType {
    type Err = String;

    fn from_str(s: &str) -> Result<AlternativeMediaType, String> {
        match s {
            "AUDIO" => Ok(AlternativeMediaType::Audio),
            "VIDEO" => Ok(AlternativeMediaType::Video),
            "SUBTITLES" => Ok(AlternativeMediaType::Subtitles),
            "CLOSEDCAPTIONS" => Ok(AlternativeMediaType::ClosedCaptions),
            _ => Err(format!("Unable to create AlternativeMediaType from {:?}", s)),
        }
    }
}

impl Default for AlternativeMediaType {
    fn default() -> AlternativeMediaType {
        AlternativeMediaType::Video
    }
}

/// [`#EXT-X-SESSION-KEY:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.5)
#[derive(Debug, Default)]
pub struct SessionKey(pub Key);

/// [`#EXT-X-SESSION-DATA:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.4)
/// The EXT-X-SESSION-KEY tag allows encryption keys from Media Playlists
/// to be specified in a Master Playlist.  This allows the client to
/// preload these keys without having to read the Media Playlist(s) first.
#[derive(Debug, Default)]
pub struct SessionData {
    pub data_id: String,
    pub value: String,
    pub uri: String,
    pub language: Option<String>,
}

impl SessionData {
    pub fn from_hashmap(mut attrs: HashMap<String, String>) -> SessionData {
        SessionData {
            data_id: attrs.remove("DATA-ID").unwrap_or_else(String::new),
            value: attrs.remove("VALUE").unwrap_or_else(String::new),
            uri: attrs.remove("URI").unwrap_or_else(String::new),
            language: attrs.remove("SUBTITLES"),
        }
    }
}

// -----------------------------------------------------------------------------------------------
// Media Playlist
// -----------------------------------------------------------------------------------------------

/// A [Media Playlist]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.3)
/// contains a list of Media Segments, which when played
/// sequentially will play the multimedia presentation.
#[derive(Debug, Default)]
pub struct MediaPlaylist {
    pub version: usize,
    /// `#EXT-X-TARGETDURATION:<s>`
    pub target_duration: f32,
    /// `#EXT-X-MEDIA-SEQUENCE:<number>`
    pub media_sequence: i32,
    pub segments: Vec<MediaSegment>,
    /// `#EXT-X-DISCONTINUITY-SEQUENCE:<number>`
    pub discontinuity_sequence: i32,
    /// `#EXT-X-ENDLIST`
    pub end_list: bool,
    /// `#EXT-X-PLAYLIST-TYPE`
    pub playlist_type: MediaPlaylistType,
    /// `#EXT-X-I-FRAMES-ONLY`
    pub i_frames_only: bool,
    /// `#EXT-X-START`
    pub start: Option<Start>,
    /// `#EXT-X-INDEPENDENT-SEGMENTS`
    pub independent_segments: bool,
}

impl MediaPlaylist {
    pub fn from_tags(mut tags: Vec<MediaPlaylistTag>) -> MediaPlaylist {
        let mut media_playlist = MediaPlaylist::default();
        let mut next_segment = MediaSegment::new();
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
                    media_playlist.playlist_type = t;
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
                MediaPlaylistTag::Segment(segment_tag) => {
                    match segment_tag {
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
                        SegmentTag::Uri(u) => {
                            next_segment.key = encryption_key.clone();
                            next_segment.map = map.clone();
                            next_segment.uri = u;
                            media_playlist.segments.push(next_segment);
                            next_segment = MediaSegment::new();
                        }
                        _ => (),
                    }
                }
                _ => (),
            }
        }
        media_playlist
    }
}

/// [`#EXT-X-PLAYLIST-TYPE:<EVENT|VOD>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.3.5)
#[derive(Debug)]
pub enum MediaPlaylistType {
    Event,
    Vod,
}

impl FromStr for MediaPlaylistType {
    type Err = String;

    fn from_str(s: &str) -> Result<MediaPlaylistType, String> {
        match s {
            "EVENT" => Ok(MediaPlaylistType::Event),
            "VOD" => Ok(MediaPlaylistType::Vod),
            _ => Err(format!("Unable to create MediaPlaylistType from {:?}", s)),
        }
    }
}

impl Default for MediaPlaylistType {
    fn default() -> MediaPlaylistType {
        MediaPlaylistType::Event
    }
}

// -----------------------------------------------------------------------------------------------
// Media Segment
// -----------------------------------------------------------------------------------------------

/// A [Media Segment](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-3)
/// is specified by a URI and optionally a byte range.
#[derive(Debug, Default)]
pub struct MediaSegment {
    pub uri: String,
    /// `#EXTINF:<duration>,[<title>]`
    pub duration: f32,
    /// `#EXTINF:<duration>,[<title>]`
    pub title: Option<String>,
    /// `#EXT-X-BYTERANGE:<n>[@<o>]`
    pub byte_range: Option<ByteRange>,
    /// `#EXT-X-DISCONTINUITY`
    pub discontinuity: bool,
    /// `#EXT-X-KEY:<attribute-list>`
    pub key: Option<Key>,
    /// `#EXT-X-MAP:<attribute-list>`
    pub map: Option<Map>,
    /// `#EXT-X-PROGRAM-DATE-TIME:<YYYY-MM-DDThh:mm:ssZ>`
    pub program_date_time: Option<String>,
    /// `#EXT-X-DATERANGE:<attribute-list>`
    pub daterange: Option<String>,
}

impl MediaSegment {
    pub fn new() -> MediaSegment {
        Default::default()
    }
}

/// [`#EXT-X-KEY:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.4)
///
/// Media Segments MAY be encrypted.  The EXT-X-KEY tag specifies how to
/// decrypt them.  It applies to every Media Segment that appears between
/// it and the next EXT-X-KEY tag in the Playlist file with the same
/// KEYFORMAT attribute (or the end of the Playlist file).  Two or more
/// EXT-X-KEY tags with different KEYFORMAT attributes MAY apply to the
/// same Media Segment if they ultimately produce the same decryption key.
#[derive(Debug, Default, Clone)]
pub struct Key {
    pub method: String,
    pub uri: Option<String>,
    pub iv: Option<String>,
    pub keyformat: Option<String>,
    pub keyformatversions: Option<String>,
}

impl Key {
    pub fn from_hashmap(mut attrs: HashMap<String, String>) -> Key {
        Key {
            method: attrs.remove("METHOD").unwrap_or_else(String::new),
            uri: attrs.remove("URI"),
            iv: attrs.remove("IV"),
            keyformat: attrs.remove("KEYFORMAT"),
            keyformatversions: attrs.remove("KEYFORMATVERSIONS"),
        }
    }
}

/// [`#EXT-X-MAP:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.5)
///
/// The EXT-X-MAP tag specifies how to obtain the Media Initialization Section
/// [(Section 3)]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-3)
/// required to parse the applicable Media Segments.
/// It applies to every Media Segment that appears after it in the
/// Playlist until the next EXT-X-MAP tag or until the end of the
/// playlist.
#[derive(Debug, Default, Clone)]
pub struct Map {
    pub uri: String,
    pub byterange: Option<ByteRange>,
}

/// [`#EXT-X-BYTERANGE:<n>[@<o>]`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.2)
///
/// The EXT-X-BYTERANGE tag indicates that a Media Segment is a sub-range
/// of the resource identified by its URI.  It applies only to the next
/// URI line that follows it in the Playlist.
#[derive(Debug, Default, Clone)]
pub struct ByteRange {
    pub length: i32,
    pub offset: Option<i32>,
}

/// [`#EXT-X-DATERANGE:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.7)
///
/// The EXT-X-DATERANGE tag associates a Date Range (i.e. a range of time
/// defined by a starting and ending date) with a set of attribute /
/// value pairs.
#[derive(Debug, Default)]
pub struct DateRange {
    pub id: String,
    pub class: Option<String>,
    pub start_date: String,
    pub end_date: Option<String>,
    pub duration: Option<String>,
    pub planned_duration: Option<String>,
    pub x_prefixed: Option<String>, //  X-<client-attribute>
    pub end_on_next: bool,
}

// -----------------------------------------------------------------------------------------------
// Rest
// -----------------------------------------------------------------------------------------------

/// [`#EXT-X-START:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.5.2)
///
/// The EXT-X-START tag indicates a preferred point at which to start
/// playing a Playlist. By default, clients SHOULD start playback at
/// this point when beginning a playback session.
#[derive(Debug, Default)]
pub struct Start {
    pub time_offset: String,
    pub precise: Option<String>,
}

impl Start {
    pub fn from_hashmap(mut attrs: HashMap<String, String>) -> Start {
        Start {
            time_offset: attrs.remove("TIME-OFFSET").unwrap_or_else(String::new),
            precise: attrs.remove("PRECISE").or(Some("NO".to_string())),
        }
    }
}

/// A simple `#EXT-` tag
#[derive(Debug, Default)]
pub struct ExtTag {
    pub tag: String,
    pub rest: String,
}

fn bool_default_false(o: Option<String>) -> bool {
    if let Some(str) = o {
        if str == "YES" {
            return true;
        }
    }
    return false;
}

// -----------------------------------------------------------------------------------------------
// Display
// -----------------------------------------------------------------------------------------------

impl fmt::Display for Playlist {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Playlist::MasterPlaylist(ref p) => write!(f, "{}", p),
            &Playlist::MediaPlaylist(ref p) => write!(f, "{}", p),
        }
    }
}

impl fmt::Display for MasterPlaylist {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f,
                      "[Master Playlist, version: {} | {} Streams]\n",
                      self.version,
                      self.variants.len()));

        for (i, stream) in self.variants.iter().enumerate() {
            try!(write!(f, " {} -> {}", i + 1, stream))
        }

        Ok(())
    }
}

impl fmt::Display for MediaPlaylist {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "[Media Playlist | duration: {:?} ~ seq: {:?} ~ type: {:?} ~ segments: {}",
            self.target_duration,
            self.media_sequence,
            self.playlist_type,
            self.segments.len(),
        ));

        if self.i_frames_only {
            try!(write!(f, " [iframes only]"));
        }
        if self.independent_segments {
            try!(write!(f, " [independent segments]"));
        }

        try!(writeln!(f, "]"));

        for (i, segment) in self.segments.iter().enumerate() {
            try!(write!(f, " {} -> {}", i + 1, segment));
        }

        Ok(())
    }
}

impl fmt::Display for MediaSegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "[Segment |"));

        if let &Some(ref v) = &self.title {
            try!(write!(f, " title: {:?}", v));
        }

        try!(write!(f, " ~ duration: {:?}", self.duration));

        if let &Some(ref v) = &self.byte_range {
            try!(write!(f, " ~ byterange: {:?}", v));
        }

        if self.discontinuity {
            try!(write!(f, " [discontinuity]"));
        }

        if let &Some(ref v) = &self.program_date_time {
            try!(write!(f, " ~ datetime: {:?}", v));
        }

        if let &Some(ref v) = &self.daterange {
            try!(write!(f, " ~ daterange: {:?}", v));
        }

        writeln!(f, " ~ uri: {:?}]", self.uri)
    }
}

impl fmt::Display for VariantStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        match self.is_i_frame {
            true => try!(write!(f, "[VariantIFrame |")),
            false => try!(write!(f, "[Variant |")),
        };

        try!(write!(f, " uri: {:?}", self.uri));

        try!(write!(f, " ~ bandwidth: {}", self.bandwidth));
        if let &Some(ref v) = &self.resolution {
            try!(write!(f, " ~ res: {}", v));
        }

        try!(write!(f, " ~ alts: {}", self.alternatives.len()));

        if let &Some(ref v) = &self.frame_rate {
            try!(write!(f, " ~ fps: {}", v));
        }

        if let &Some(ref v) = &self.audio {
            try!(write!(f, " ~ audio: {}", v));
        }

        if let &Some(ref v) = &self.video {
            try!(write!(f, " ~ video: {}", v));
        }

        if let &Some(ref v) = &self.subtitles {
            try!(write!(f, " ~ subs: {}", v));
        }

        if let &Some(ref v) = &self.closed_captions {
            try!(write!(f, " ~ closed_captions: {}", v));
        }

        try!(write!(f, "]"));
        try!(write!(f, "\n"));

        for (_, alt) in self.alternatives.iter().enumerate() {
            try!(write!(f, "{}", alt));
        }

        Ok(())
    }
}

impl fmt::Display for AlternativeMedia {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        try!(write!(f,
                    "[AlternativeMedia | type: {:?} ~ group: {} ~ name: {:?}",
                    self.media_type,
                    self.group_id,
                    self.name));

        if let &Some(ref v) = &self.uri {
            try!(write!(f, " ~ uri: {:?}", v));
        }

        try!(write!(f, " ~ default: {}", self.default));

        if let &Some(ref v) = &self.language {
            try!(write!(f, " ~ lang: {}", v));
        }

        if let &Some(ref v) = &self.assoc_language {
            try!(write!(f, " ~ assoc_language: {}", v));
        }

        try!(write!(f, " ~ autoselect: {}", self.default));

        try!(write!(f, " ~ forced: {}", self.default));

        if let &Some(ref v) = &self.instream_id {
            try!(write!(f, " ~ instream_id: {}", v));
        }

        if let &Some(ref v) = &self.characteristics {
            try!(write!(f, " ~ characteristics: {}", v));
        }

        writeln!(f, "]")
    }
}
