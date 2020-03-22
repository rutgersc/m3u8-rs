//! Contains all the structs used for parsing.
//!
//! The main type here is the `Playlist` enum.
//! Which is either a `MasterPlaylist` or a `MediaPlaylist`.

use std::io::Write;
use std::collections::HashMap;
use std::str::FromStr;
use std::fmt;
use super::*;
use std::f32;

macro_rules! write_some_attribute_quoted {
    ($w:expr, $tag:expr, $o:expr) => (
        if let &Some(ref v) = $o { write!($w, "{}=\"{}\"", $tag, v)  } else { Ok(()) }
    );
}

macro_rules! write_some_attribute {
    ($w:expr, $tag:expr, $o:expr) => (
        if let &Some(ref v) = $o { write!($w, "{}={}", $tag, v) } else { Ok(()) }
    );
}

macro_rules! bool_default_false {
    ($optional:expr) => (
        match $optional {
            Some(ref s) if s == "YES" => true,
            Some(_) | None => false,
        }
    );
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
        match self {
            &Playlist::MasterPlaylist(ref pl) => pl.write_to(writer),
            &Playlist::MediaPlaylist(ref pl) => pl.write_to(writer),
        }
    }
}

// -----------------------------------------------------------------------------------------------
// Master Playlist
// -----------------------------------------------------------------------------------------------

/// A [Master Playlist]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4)
/// provides a set of Variant Streams, each of which
/// describes a different version of the same content.
#[derive(Debug, Default, PartialEq, Clone)]
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

        while let Some(tag) = tags.pop() {
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

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        writeln!(w, "{}" ,"#EXTM3U")?;
        writeln!(w, "#EXT-X-VERSION:{}", self.version)?;

        for variant in &self.variants {
            variant.write_to(w)?;
        }
        if let Some(ref session_data) = self.session_data {
            session_data.write_to(w)?;
        }
        if let Some(ref session_key) = self.session_key {
            session_key.write_to(w)?;
        }
        if let Some(ref start) = self.start {
            start.write_to(w)?;
        }
        if self.independent_segments {
            writeln!(w, "#EXT-X-INDEPENDENT-SEGMENTS")?;
        }

        Ok(())
    }
}

/// [`#EXT-X-STREAM-INF:<attribute-list>
///   <URI>`]
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
#[derive(Debug, Default, PartialEq, Clone)]
pub struct VariantStream {
    pub is_i_frame: bool,
    pub uri: String,

    // <attribute-list>
    pub bandwidth: String,
    pub average_bandwidth: Option<String>,
    pub codecs: Option<String>,
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
            codecs: attrs.remove("CODECS"),
            resolution: attrs.remove("RESOLUTION"),
            frame_rate: attrs.remove("FRAME-RATE"),
            audio: attrs.remove("AUDIO"),
            video: attrs.remove("VIDEO"),
            subtitles: attrs.remove("SUBTITLES"),
            closed_captions: attrs.remove("CLOSED-CAPTIONS"),
            alternatives: vec![],
        }
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {

        for alternative in &self.alternatives {
            alternative.write_to(w)?;
        }

        if self.is_i_frame {
            write!(w, "#EXT-X-I-FRAME-STREAM-INF:")?;
            self.write_stream_inf_common_attributes(w)?;
            writeln!(w, ",URI=\"{}\"", self.uri)
        }
        else {
            write!(w, "#EXT-X-STREAM-INF:")?;
            self.write_stream_inf_common_attributes(w)?;
            write_some_attribute_quoted!(w, ",AUDIO", &self.audio)?;
            write_some_attribute_quoted!(w, ",SUBTITLES", &self.subtitles)?;
            write_some_attribute_quoted!(w, ",CLOSED-CAPTIONS", &self.closed_captions)?;
            write!(w, "\n")?;
            writeln!(w, "{}", self.uri)
        }
    }

    fn write_stream_inf_common_attributes<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "BANDWIDTH={}", &self.bandwidth)?;
        write_some_attribute!(w, ",AVERAGE-BANDWIDTH", &self.average_bandwidth)?;
        write_some_attribute_quoted!(w, ",CODECS", &self.codecs)?;
        write_some_attribute!(w, ",RESOLUTION", &self.resolution)?;
        write_some_attribute!(w, ",FRAME-RATE", &self.frame_rate)?;
        write_some_attribute_quoted!(w, ",VIDEO", &self.video)
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
#[derive(Debug, Default, PartialEq, Clone)]
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
            default: bool_default_false!(attrs.remove("DEFAULT")),
            autoselect: bool_default_false!(attrs.remove("AUTOSELECT")),
            forced: bool_default_false!(attrs.remove("FORCED")),
            instream_id: attrs.remove("INSTREAM-ID"),
            characteristics: attrs.remove("CHARACTERISTICS"),
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
        if self.default { write!(w, ",DEFAULT=YES")?; }
        if self.autoselect { write!(w, ",AUTOSELECT=YES")?; }
        if self.forced { write!(w, ",FORCED=YES")?; }
        write_some_attribute_quoted!(w, ",INSTREAM-ID", &self.instream_id)?;
        write_some_attribute_quoted!(w, ",CHARACTERISTICS", &self.characteristics)?;
        write!(w, "\n")
    }
}

#[derive(Debug, PartialEq, Clone)]
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

impl fmt::Display for AlternativeMediaType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &AlternativeMediaType::Audio => "AUDIO",
            &AlternativeMediaType::Video => "VIDEO",
            &AlternativeMediaType::Subtitles => "SUBTITLES",
            &AlternativeMediaType::ClosedCaptions => "CLOSEDCAPTIONS",
        })
    }
}


/// [`#EXT-X-SESSION-KEY:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.5)
#[derive(Debug, Default, PartialEq, Clone)]
pub struct SessionKey(pub Key);

impl SessionKey {
    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-SESSION-KEY:")?;
        self.0.write_attributes_to(w)?;
        write!(w, "\n")
    }
}

/// [`#EXT-X-SESSION-DATA:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.4)
/// The EXT-X-SESSION-KEY tag allows encryption keys from Media Playlists
/// to be specified in a Master Playlist.  This allows the client to
/// preload these keys without having to read the Media Playlist(s) first.
#[derive(Debug, Default, PartialEq, Clone)]
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
            language: attrs.remove("LANGUAGE"),
        }
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-SESSION-DATA:")?;
        write!(w, "DATA-ID=\"{}\"", self.data_id)?;
        write!(w, ",VALUE=\"{}\"", self.value)?;
        write!(w, ",URI=\"{}\"", self.uri)?;
        write_some_attribute_quoted!(w, ",LANGUAGE", &self.language)?;
        write!(w, "\n")
    }
}

// -----------------------------------------------------------------------------------------------
// Media Playlist
// -----------------------------------------------------------------------------------------------

/// A [Media Playlist]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.3)
/// contains a list of Media Segments, which when played
/// sequentially will play the multimedia presentation.
#[derive(Debug, Default, PartialEq, Clone)]
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
    pub playlist_type: Option<MediaPlaylistType>,
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
                            next_segment = MediaSegment::empty();
                            encryption_key = None;
                            map = None;
                        }
                        _ => (),
                    }
                }
                _ => (),
            }
        }
        media_playlist
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        writeln!(w, "{}" ,"#EXTM3U")?;
        writeln!(w, "#EXT-X-VERSION:{}", self.version)?;
        writeln!(w, "#EXT-X-TARGETDURATION:{}", self.target_duration)?;

        if self.media_sequence != 0 {
            writeln!(w, "#EXT-X-MEDIA-SEQUENCE:{}", self.media_sequence)?;
        }
        if self.discontinuity_sequence != 0 {
            writeln!(w, "#EXT-X-DISCONTINUITY-SEQUENCE:{}", self.discontinuity_sequence)?;
        }
        if self.end_list {
            writeln!(w, "#EXT-X-ENDLIST")?;
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

        Ok(())
    }
}

/// [`#EXT-X-PLAYLIST-TYPE:<EVENT|VOD>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.3.5)
#[derive(Debug, PartialEq, Clone)]
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
        write!(f, "{}", match self {
            &MediaPlaylistType::Event => "EVENT",
            &MediaPlaylistType::Vod => "VOD",
        })
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
}

impl MediaSegment {
    pub fn empty() -> MediaSegment {
        Default::default()
    }

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {

        if let Some(ref byte_range) = self.byte_range {
            write!(w, "#EXT-X-BYTERANGE:")?;
            byte_range.write_value_to(w)?;
            write!(w, "\n")?;
        }
        if self.discontinuity {
            writeln!(w, "{}", "#EXT-X-DISCONTINUITY")?;
        }
        if let Some(ref key) = self.key {
            write!(w, "#EXT-X-KEY:")?;
            key.write_attributes_to(w)?;
            write!(w, "\n")?;
        }
        if let Some(ref map) = self.map {
            write!(w, "#EXT-X-MAP:")?;
            map.write_attributes_to(w)?;
            write!(w, "\n")?;
        }
        if let Some(ref v) = self.program_date_time {
            writeln!(w, "#EXT-X-PROGRAM-DATE-TIME:{}", v)?;
        }
        if let Some(ref v) = self.daterange {
            writeln!(w, "#EXT-X-DATERANGE:{}", v)?;
        }

        write!(w, "#EXTINF:{},", self.duration)?;

        if let Some(ref v) = self.title {
            writeln!(w, "{}", v)?;
        } else {
            write!(w, "\n")?;
        }

        writeln!(w, "{}", self.uri)
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
#[derive(Debug, Default, PartialEq, Clone)]
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

    pub fn write_attributes_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "METHOD={}", self.method)?;
        write_some_attribute_quoted!(w, ",URI", &self.uri)?;
        write_some_attribute!(w, ",IV", &self.iv)?;
        write_some_attribute!(w, ",KEYFORMAT", &self.keyformat)?;
        write_some_attribute!(w, ",KEYFORMATVERSIONS", &self.keyformatversions)
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
#[derive(Debug, Default, PartialEq, Clone)]
pub struct Map {
    pub uri: String,
    pub byte_range: Option<ByteRange>,
}

impl Map {
    pub fn from_hashmap(mut attrs: HashMap<String, String>) -> Map {
        Map {
            uri: attrs.remove("URI").unwrap_or_default(),
            byte_range: attrs.remove("BYTERANGE").map(ByteRange::from),
        }
    }

    pub fn write_attributes_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "URI=\"{}\"", self.uri)?;
        if let Some(ref byte_range) = self.byte_range {
            write!(w, ",BYTERANGE=")?;
            byte_range.write_value_to(w)?;
        }
        Ok(())
    }
}


/// [`#EXT-X-BYTERANGE:<n>[@<o>]`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.2)
///
/// The EXT-X-BYTERANGE tag indicates that a Media Segment is a sub-range
/// of the resource identified by its URI.  It applies only to the next
/// URI line that follows it in the Playlist.
#[derive(Debug, Default, PartialEq, Clone)]
pub struct ByteRange {
    pub length: i32,
    pub offset: Option<i32>,
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

impl From<String> for ByteRange {
    fn from(s: String) -> Self {
        let w: &str = &s;
        ByteRange::from(w)
    }
}

impl<'a> From<&'a str> for ByteRange {
    fn from(s: &'a str) -> Self {
        match byte_range_val(s.as_bytes()) {
            IResult::Ok((_, br)) => br,
            _ => panic!("Should not happen"),
        }
    }
}


/// [`#EXT-X-DATERANGE:<attribute-list>`]
/// (https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.2.7)
///
/// The EXT-X-DATERANGE tag associates a Date Range (i.e. a range of time
/// defined by a starting and ending date) with a set of attribute /
/// value pairs.
#[derive(Debug, Default, PartialEq, Clone)]
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
#[derive(Debug, Default, PartialEq, Clone)]
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

    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-START:TIME-OFFSET={}", self.time_offset)?;
        write_some_attribute!(w, ",PRECISE", &self.precise)?;
        write!(w, "\n")
    }
}

/// A simple `#EXT-` tag
#[derive(Debug, Default, Clone)]
pub struct ExtTag {
    pub tag: String,
    pub rest: String,
}
