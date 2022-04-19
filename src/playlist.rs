//! Contains all the structs used for parsing.
//!
//! The main type here is the `Playlist` enum.
//! Which is either a `MasterPlaylist` or a `MediaPlaylist`.

use crate::QuotedOrUnquoted;
use std::collections::HashMap;
use std::f32;
use std::fmt;
use std::fmt::Display;
use std::io::Write;
use std::str::FromStr;

macro_rules! write_some_attribute_quoted {
    ($w:expr, $tag:expr, $o:expr) => {
        if let &Some(ref v) = $o {
            write!($w, "{}=\"{}\"", $tag, v)
        } else {
            Ok(())
        }
    };
}

macro_rules! write_some_attribute {
    ($w:expr, $tag:expr, $o:expr) => {
        if let &Some(ref v) = $o {
            write!($w, "{}={}", $tag, v)
        } else {
            Ok(())
        }
    };
}

macro_rules! bool_default_false {
    ($optional:expr) => {
        match $optional {
            Some(ref s) if s == "YES" => true,
            Some(_) | None => false,
        }
    };
}

/// [Playlist](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.1),
/// can either be a `MasterPlaylist` or a `MediaPlaylist`.
///
/// A Playlist is a Media Playlist if all URI lines in the Playlist
/// identify Media Segments.  A Playlist is a Master Playlist if all URI
/// lines in the Playlist identify Media Playlists.  A Playlist MUST be
/// either a Media Playlist or a Master Playlist; all other Playlists are invalid.
#[derive(Debug, PartialEq, Clone)]
pub enum Playlist {
    MasterPlaylist(MasterPlaylist),
    MediaPlaylist(MediaPlaylist),
}

impl Playlist {
    pub fn write_to<T: Write>(&self, writer: &mut T) -> std::io::Result<()> {
        match *self {
            Playlist::MasterPlaylist(ref pl) => pl.write_to(writer),
            Playlist::MediaPlaylist(ref pl) => pl.write_to(writer),
        }
    }
}

// -----------------------------------------------------------------------------------------------
// Master Playlist
// -----------------------------------------------------------------------------------------------

/// A [Master Playlist](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4)
/// provides a set of Variant Streams, each of which
/// describes a different version of the same content.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct MasterPlaylist {
    pub version: Option<usize>,
    pub variants: Vec<VariantStream>,
    pub session_data: Vec<SessionData>,
    pub session_key: Vec<SessionKey>,
    pub start: Option<Start>,
    pub independent_segments: bool,
    pub alternatives: Vec<AlternativeMedia>, // EXT-X-MEDIA tags
    pub unknown_tags: Vec<ExtTag>,
}

impl MasterPlaylist {
    pub fn get_newest_variant(&mut self) -> Option<&mut VariantStream> {
        self.variants.iter_mut().rev().find(|v| !v.is_i_frame)
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        writeln!(w, "#EXTM3U")?;

        if let Some(ref v) = self.version {
            writeln!(w, "#EXT-X-VERSION:{}", v)?;
        }

        for alternative in &self.alternatives {
            alternative.write_to(w)?;
        }

        for variant in &self.variants {
            variant.write_to(w)?;
        }
        for session_data in &self.session_data {
            session_data.write_to(w)?;
        }
        for session_key in &self.session_key {
            session_key.write_to(w)?;
        }
        if let Some(ref start) = self.start {
            start.write_to(w)?;
        }
        if self.independent_segments {
            writeln!(w, "#EXT-X-INDEPENDENT-SEGMENTS")?;
        }
        for unknown_tag in &self.unknown_tags {
            writeln!(w, "{}", unknown_tag)?;
        }

        Ok(())
    }
}

/// [`#EXT-X-STREAM-INF:<attribute-list> <URI>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.2)
/// [`#EXT-X-I-FRAME-STREAM-INF:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.3)
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
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct VariantStream {
    pub is_i_frame: bool,
    pub uri: String,

    // <attribute-list>
    pub bandwidth: String,
    pub average_bandwidth: Option<String>,
    pub codecs: Option<String>,
    pub resolution: Option<String>,
    pub frame_rate: Option<String>,
    pub hdcp_level: Option<QuotedOrUnquoted>,
    pub audio: Option<String>,
    pub video: Option<String>,
    pub subtitles: Option<String>,
    pub closed_captions: Option<QuotedOrUnquoted>,
    // PROGRAM-ID tag was removed in protocol version 6
}

impl VariantStream {
    pub fn from_hashmap(
        mut attrs: HashMap<String, QuotedOrUnquoted>,
        is_i_frame: bool,
    ) -> VariantStream {
        VariantStream {
            is_i_frame,
            uri: attrs.remove("URI").unwrap_or_default().to_string(),
            bandwidth: attrs.remove("BANDWIDTH").unwrap_or_default().to_string(),
            average_bandwidth: attrs.remove("AVERAGE-BANDWIDTH").map(|a| a.to_string()),
            codecs: attrs.remove("CODECS").map(|c| c.to_string()),
            resolution: attrs.remove("RESOLUTION").map(|r| r.to_string()),
            frame_rate: attrs.remove("FRAME-RATE").map(|f| f.to_string()),
            hdcp_level: attrs.remove("HDCP-LEVEL"),
            audio: attrs.remove("AUDIO").map(|a| a.to_string()),
            video: attrs.remove("VIDEO").map(|v| v.to_string()),
            subtitles: attrs.remove("SUBTITLES").map(|s| s.to_string()),
            closed_captions: attrs.remove("CLOSED-CAPTIONS"),
        }
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        if self.is_i_frame {
            write!(w, "#EXT-X-I-FRAME-STREAM-INF:")?;
            self.write_stream_inf_common_attributes(w)?;
            writeln!(w, ",URI=\"{}\"", self.uri)
        } else {
            write!(w, "#EXT-X-STREAM-INF:")?;
            self.write_stream_inf_common_attributes(w)?;
            write_some_attribute_quoted!(w, ",AUDIO", &self.audio)?;
            write_some_attribute_quoted!(w, ",SUBTITLES", &self.subtitles)?;
            write_some_attribute!(w, ",CLOSED-CAPTIONS", &self.closed_captions)?;
            writeln!(w)?;
            writeln!(w, "{}", self.uri)
        }
    }

    fn write_stream_inf_common_attributes<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "BANDWIDTH={}", &self.bandwidth)?;
        write_some_attribute!(w, ",AVERAGE-BANDWIDTH", &self.average_bandwidth)?;
        write_some_attribute_quoted!(w, ",CODECS", &self.codecs)?;
        write_some_attribute!(w, ",RESOLUTION", &self.resolution)?;
        write_some_attribute!(w, ",FRAME-RATE", &self.frame_rate)?;
        write_some_attribute!(w, ",HDCP-LEVEL", &self.hdcp_level)?;
        write_some_attribute_quoted!(w, ",VIDEO", &self.video)
    }
}

/// [`#EXT-X-MEDIA:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.1)
///
/// The EXT-X-MEDIA tag is used to relate Media Playlists that contain
/// alternative Renditions (Section 4.3.4.2.1) of the same content.  For
/// example, three EXT-X-MEDIA tags can be used to identify audio-only
/// Media Playlists that contain English, French and Spanish Renditions
/// of the same presentation.  Or two EXT-X-MEDIA tags can be used to
/// identify video-only Media Playlists that show two different camera angles.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
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
    pub channels: Option<String>,
}

impl AlternativeMedia {
    pub fn from_hashmap(mut attrs: HashMap<String, QuotedOrUnquoted>) -> AlternativeMedia {
        AlternativeMedia {
            media_type: attrs
                .get("TYPE")
                .and_then(|s| AlternativeMediaType::from_str(s.to_string().as_str()).ok())
                .unwrap_or_default(),
            uri: attrs.remove("URI").map(|u| u.to_string()),
            group_id: attrs.remove("GROUP-ID").unwrap_or_default().to_string(),
            language: attrs.remove("LANGUAGE").map(|l| l.to_string()),
            assoc_language: attrs.remove("ASSOC-LANGUAGE").map(|a| a.to_string()),
            name: attrs.remove("NAME").unwrap_or_default().to_string(),
            default: bool_default_false!(attrs.remove("DEFAULT").map(|s| s.to_string())),
            autoselect: bool_default_false!(attrs.remove("AUTOSELECT").map(|s| s.to_string())),
            forced: bool_default_false!(attrs.remove("FORCED").map(|f| f.to_string())),
            instream_id: attrs.remove("INSTREAM-ID").map(|i| i.to_string()),
            characteristics: attrs.remove("CHARACTERISTICS").map(|c| c.to_string()),
            channels: attrs.remove("CHANNELS").map(|c| c.to_string()),
        }
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-MEDIA:")?;
        write!(w, "TYPE={}", self.media_type)?;
        write_some_attribute_quoted!(w, ",URI", &self.uri)?;
        write!(w, ",GROUP-ID=\"{}\"", self.group_id)?;
        write_some_attribute_quoted!(w, ",LANGUAGE", &self.language)?;
        write_some_attribute_quoted!(w, ",ASSOC-LANGUAGE", &self.assoc_language)?;
        write!(w, ",NAME=\"{}\"", self.name)?;
        if self.default {
            write!(w, ",DEFAULT=YES")?;
        }
        if self.autoselect {
            write!(w, ",AUTOSELECT=YES")?;
        }
        if self.forced {
            write!(w, ",FORCED=YES")?;
        }
        write_some_attribute_quoted!(w, ",INSTREAM-ID", &self.instream_id)?;
        write_some_attribute_quoted!(w, ",CHARACTERISTICS", &self.characteristics)?;
        write_some_attribute_quoted!(w, ",CHANNELS", &self.channels)?;
        writeln!(w)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
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
            "CLOSED-CAPTIONS" => Ok(AlternativeMediaType::ClosedCaptions),
            _ => Err(format!(
                "Unable to create AlternativeMediaType from {:?}",
                s
            )),
        }
    }
}

impl Default for AlternativeMediaType {
    fn default() -> AlternativeMediaType {
        AlternativeMediaType::Video
    }
}

impl fmt::Display for AlternativeMediaType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                AlternativeMediaType::Audio => "AUDIO",
                AlternativeMediaType::Video => "VIDEO",
                AlternativeMediaType::Subtitles => "SUBTITLES",
                AlternativeMediaType::ClosedCaptions => "CLOSED-CAPTIONS",
            }
        )
    }
}

/// [`#EXT-X-SESSION-KEY:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.5)
/// The EXT-X-SESSION-KEY tag allows encryption keys from Media Playlists
/// to be specified in a Master Playlist.  This allows the client to
/// preload these keys without having to read the Media Playlist(s) first.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct SessionKey(pub Key);

impl SessionKey {
    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-SESSION-KEY:")?;
        self.0.write_attributes_to(w)?;
        writeln!(w)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SessionDataField {
    Value(String),
    Uri(String),
}

/// [`#EXT-X-SESSION-DATA:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.4)
/// The EXT-X-SESSION-DATA tag allows arbitrary session data to be carried
/// in a Master Playlist.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SessionData {
    pub data_id: String,
    pub field: SessionDataField,
    pub language: Option<String>,
}

impl SessionData {
    pub fn from_hashmap(
        mut attrs: HashMap<String, QuotedOrUnquoted>,
    ) -> Result<SessionData, String> {
        let data_id = match attrs.remove("DATA-ID") {
            Some(data_id) => data_id,
            None => return Err("EXT-X-SESSION-DATA field without DATA-ID".to_string()),
        };

        let value = attrs.remove("VALUE").map(|v| v.to_string());
        let uri = attrs.remove("URI").map(|u| u.to_string());

        // SessionData must contain either a VALUE or a URI,
        // but not both https://tools.ietf.org/html/rfc8216#section-4.3.4.4
        let field = match (value, uri) {
            (Some(value), None) => SessionDataField::Value(value),
            (None, Some(uri)) => SessionDataField::Uri(uri),
            (Some(_), Some(_)) => {
                return Err(format![
                    "EXT-X-SESSION-DATA tag {} contains both a value and a uri",
                    data_id
                ])
            }
            (None, None) => {
                return Err(format![
                    "EXT-X-SESSION-DATA tag {} must contain either a value or a uri",
                    data_id
                ])
            }
        };

        Ok(SessionData {
            data_id: data_id.to_string(),
            field,
            language: attrs.remove("LANGUAGE").map(|s| s.to_string()),
        })
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-SESSION-DATA:")?;
        write!(w, "DATA-ID=\"{}\"", self.data_id)?;
        match &self.field {
            SessionDataField::Value(value) => write!(w, ",VALUE=\"{}\"", value)?,
            SessionDataField::Uri(uri) => write!(w, ",URI=\"{}\"", uri)?,
        };
        write_some_attribute_quoted!(w, ",LANGUAGE", &self.language)?;
        writeln!(w)
    }
}

// -----------------------------------------------------------------------------------------------
// Media Playlist
// -----------------------------------------------------------------------------------------------

/// A [Media Playlist](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.3)
/// contains a list of Media Segments, which when played
/// sequentially will play the multimedia presentation.
#[derive(Debug, Default, PartialEq, Clone)]
pub struct MediaPlaylist {
    pub version: Option<usize>,
    /// `#EXT-X-TARGETDURATION:<s>`
    pub target_duration: f32,
    /// `#EXT-X-MEDIA-SEQUENCE:<number>`
    pub media_sequence: u64,
    pub segments: Vec<MediaSegment>,
    /// `#EXT-X-DISCONTINUITY-SEQUENCE:<number>`
    pub discontinuity_sequence: u64,
    /// `#EXT-X-ENDLIST`
    pub end_list: bool,
    /// `#EXT-X-PLAYLIST-TYPE`
    pub playlist_type: Option<MediaPlaylistType>,
    /// `#EXT-X-I-FRAMES-ONLY`
    pub i_frames_only: bool,
    /// `#EXT-X-START`
    pub start: Option<Start>,
    /// `#EXT-X-INDEPENDENT-SEGMENTS`
    pub independent_segments: bool,
}

impl MediaPlaylist {
    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        writeln!(w, "#EXTM3U")?;

        if let Some(ref v) = self.version {
            writeln!(w, "#EXT-X-VERSION:{}", v)?;
        }
        writeln!(w, "#EXT-X-TARGETDURATION:{}", self.target_duration)?;

        if self.media_sequence != 0 {
            writeln!(w, "#EXT-X-MEDIA-SEQUENCE:{}", self.media_sequence)?;
        }
        if self.discontinuity_sequence != 0 {
            writeln!(
                w,
                "#EXT-X-DISCONTINUITY-SEQUENCE:{}",
                self.discontinuity_sequence
            )?;
        }
        if let Some(ref v) = self.playlist_type {
            writeln!(w, "#EXT-X-PLAYLIST-TYPE:{}", v)?;
        }
        if self.i_frames_only {
            writeln!(w, "#EXT-X-I-FRAMES-ONLY")?;
        }
        if let Some(ref start) = self.start {
            start.write_to(w)?;
        }
        if self.independent_segments {
            writeln!(w, "#EXT-X-INDEPENDENT-SEGMENTS")?;
        }
        for segment in &self.segments {
            segment.write_to(w)?;
        }
        if self.end_list {
            writeln!(w, "#EXT-X-ENDLIST")?;
        }

        Ok(())
    }
}

/// [`#EXT-X-PLAYLIST-TYPE:<EVENT|VOD>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.3.5)
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
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

impl fmt::Display for MediaPlaylistType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                MediaPlaylistType::Event => "EVENT",
                MediaPlaylistType::Vod => "VOD",
            }
        )
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
#[derive(Debug, Default, PartialEq, Clone)]
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
    /// `#EXT-`
    pub unknown_tags: Vec<ExtTag>,
}

impl MediaSegment {
    pub fn empty() -> MediaSegment {
        Default::default()
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        if let Some(ref byte_range) = self.byte_range {
            write!(w, "#EXT-X-BYTERANGE:")?;
            byte_range.write_value_to(w)?;
            writeln!(w)?;
        }
        if self.discontinuity {
            writeln!(w, "#EXT-X-DISCONTINUITY")?;
        }
        if let Some(ref key) = self.key {
            write!(w, "#EXT-X-KEY:")?;
            key.write_attributes_to(w)?;
            writeln!(w)?;
        }
        if let Some(ref map) = self.map {
            write!(w, "#EXT-X-MAP:")?;
            map.write_attributes_to(w)?;
            writeln!(w)?;
        }
        if let Some(ref v) = self.program_date_time {
            writeln!(w, "#EXT-X-PROGRAM-DATE-TIME:{}", v)?;
        }
        if let Some(ref v) = self.daterange {
            writeln!(w, "#EXT-X-DATERANGE:{}", v)?;
        }
        for unknown_tag in &self.unknown_tags {
            writeln!(w, "{}", unknown_tag)?;
        }

        write!(w, "#EXTINF:{},", self.duration)?;

        if let Some(ref v) = self.title {
            writeln!(w, "{}", v)?;
        } else {
            writeln!(w)?;
        }

        writeln!(w, "{}", self.uri)
    }
}

/// [`#EXT-X-KEY:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.4)
///
/// Media Segments MAY be encrypted.  The EXT-X-KEY tag specifies how to
/// decrypt them.  It applies to every Media Segment that appears between
/// it and the next EXT-X-KEY tag in the Playlist file with the same
/// KEYFORMAT attribute (or the end of the Playlist file).  Two or more
/// EXT-X-KEY tags with different KEYFORMAT attributes MAY apply to the
/// same Media Segment if they ultimately produce the same decryption key.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Key {
    pub method: String,
    pub uri: Option<String>,
    pub iv: Option<String>,
    pub keyformat: Option<String>,
    pub keyformatversions: Option<String>,
}

impl Key {
    pub fn from_hashmap(mut attrs: HashMap<String, QuotedOrUnquoted>) -> Key {
        Key {
            method: attrs.remove("METHOD").unwrap_or_default().to_string(),
            uri: attrs.remove("URI").map(|u| u.to_string()),
            iv: attrs.remove("IV").map(|i| i.to_string()),
            keyformat: attrs.remove("KEYFORMAT").map(|k| k.to_string()),
            keyformatversions: attrs.remove("KEYFORMATVERSIONS").map(|k| k.to_string()),
        }
    }

    pub fn write_attributes_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "METHOD={}", self.method)?;
        write_some_attribute_quoted!(w, ",URI", &self.uri)?;
        write_some_attribute!(w, ",IV", &self.iv)?;
        write_some_attribute!(w, ",KEYFORMAT", &self.keyformat)?;
        write_some_attribute!(w, ",KEYFORMATVERSIONS", &self.keyformatversions)
    }
}

/// [`#EXT-X-MAP:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.5)
///
/// The EXT-X-MAP tag specifies how to obtain the Media Initialization Section
/// [(Section 3)](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-3)
/// required to parse the applicable Media Segments.
/// It applies to every Media Segment that appears after it in the
/// Playlist until the next EXT-X-MAP tag or until the end of the
/// playlist.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Map {
    pub uri: String,
    pub byte_range: Option<ByteRange>,
}

impl Map {
    pub fn write_attributes_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "URI=\"{}\"", self.uri)?;
        if let Some(ref byte_range) = self.byte_range {
            write!(w, ",BYTERANGE=")?;
            byte_range.write_value_to(w)?;
        }
        Ok(())
    }
}

/// [`#EXT-X-BYTERANGE:<n>[@<o>]`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.2)
///
/// The EXT-X-BYTERANGE tag indicates that a Media Segment is a sub-range
/// of the resource identified by its URI.  It applies only to the next
/// URI line that follows it in the Playlist.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct ByteRange {
    pub length: u64,
    pub offset: Option<u64>,
}

impl ByteRange {
    pub fn write_value_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "{}", self.length)?;
        if let Some(offset) = self.offset {
            write!(w, "@{}", offset)?;
        }
        Ok(())
    }
}

/// [`#EXT-X-DATERANGE:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.7)
///
/// The EXT-X-DATERANGE tag associates a Date Range (i.e. a range of time
/// defined by a starting and ending date) with a set of attribute /
/// value pairs.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
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

/// [`#EXT-X-START:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.5.2)
///
/// The EXT-X-START tag indicates a preferred point at which to start
/// playing a Playlist. By default, clients SHOULD start playback at
/// this point when beginning a playback session.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Start {
    pub time_offset: String,
    pub precise: Option<String>,
}

impl Start {
    pub fn from_hashmap(mut attrs: HashMap<String, QuotedOrUnquoted>) -> Start {
        Start {
            time_offset: attrs.remove("TIME-OFFSET").unwrap_or_default().to_string(),
            precise: attrs
                .remove("PRECISE")
                .map(|a| a.to_string())
                .or_else(|| Some("NO".to_string())),
        }
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-START:TIME-OFFSET={}", self.time_offset)?;
        write_some_attribute!(w, ",PRECISE", &self.precise)?;
        writeln!(w)
    }
}

/// A simple `#EXT-` tag
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct ExtTag {
    pub tag: String,
    pub rest: Option<String>,
}

impl Display for ExtTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#EXT-{}", self.tag)?;
        if let Some(v) = &self.rest {
            write!(f, ":{}", v)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ext_tag_with_value_is_printable() {
        let cue_out_tag = ExtTag {
            tag: "X-CUE-OUT".into(),
            rest: Some("DURATION=30".into()),
        };

        let mut output = Vec::new();
        write!(output, "{}", cue_out_tag).unwrap();

        assert_eq!(
            std::str::from_utf8(output.as_slice()).unwrap(),
            "#EXT-X-CUE-OUT:DURATION=30"
        )
    }

    #[test]
    fn ext_tag_without_value_is_printable() {
        let cue_in_tag = ExtTag {
            tag: "X-CUE-IN".into(),
            rest: None,
        };

        let mut output = Vec::new();
        write!(output, "{}", cue_in_tag).unwrap();

        assert_eq!(
            std::str::from_utf8(output.as_slice()).unwrap(),
            "#EXT-X-CUE-IN"
        )
    }
}
