//! Contains all the structs used for parsing.
//!
//! The main type here is the `Playlist` enum.
//! Which is either a `MasterPlaylist` or a `MediaPlaylist`.

use crate::QuotedOrUnquoted;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fmt::Display;
use std::io::Write;
use std::str::FromStr;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::usize::MAX;
use std::{f32, usize};

/// The output precision for floats, such as #EXTINF (default is unset)
pub static WRITE_OPT_FLOAT_PRECISION: AtomicUsize = AtomicUsize::new(MAX);

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

macro_rules! write_some_other_attributes {
    ($w:expr, $attr:expr) => {
        if let &Some(ref attributes) = $attr {
            let mut status = std::io::Result::Ok(());
            for (name, val) in attributes {
                let res = write!($w, ",{}={}", name, val);
                if res.is_err() {
                    status = res;
                    break;
                }
            }
            status
        } else {
            Ok(())
        }
    };
}

macro_rules! is_yes {
    ($attrs:expr, $attr:expr) => {
        match $attrs.remove($attr) {
            Some(QuotedOrUnquoted::Unquoted(ref s)) if s == "YES" => true,
            Some(QuotedOrUnquoted::Unquoted(ref s)) if s == "NO" => false,
            Some(ref s) => {
                return Err(format!(
                    "Can't create bool from {} for {} attribute",
                    s, $attr
                ))
            }
            None => false,
        }
    };
}

macro_rules! quoted_string {
    ($attrs:expr, $attr:expr) => {
        match $attrs.remove($attr) {
            Some(QuotedOrUnquoted::Quoted(s)) => Some(s),
            Some(QuotedOrUnquoted::Unquoted(_)) => {
                return Err(format!(
                    "Can't create {} attribute from unquoted string",
                    $attr
                ))
            }
            None => None,
        }
    };
}

macro_rules! unquoted_string {
    ($attrs:expr, $attr:expr) => {
        match $attrs.remove($attr) {
            Some(QuotedOrUnquoted::Unquoted(s)) => Some(s),
            Some(QuotedOrUnquoted::Quoted(_)) => {
                return Err(format!(
                    "Can't create {} attribute from quoted string",
                    $attr
                ))
            }
            None => None,
        }
    };
}

macro_rules! unquoted_string_parse {
    ($attrs:expr, $attr:expr, $parse:expr) => {
        match $attrs.remove($attr) {
            Some(QuotedOrUnquoted::Unquoted(s)) => Some(
                ($parse(s.as_str())).map_err(|_| format!("Can't create attribute {}", $attr))?,
            ),
            Some(QuotedOrUnquoted::Quoted(_)) => {
                return Err(format!(
                    "Can't create {} attribute from quoted string",
                    $attr
                ))
            }
            None => None,
        }
    };
    ($attrs:expr, $attr:expr) => {
        unquoted_string_parse!($attrs, $attr, |s: &str| s.parse())
    };
}

macro_rules! quoted_string_parse {
    ($attrs:expr, $attr:expr, $parse:expr) => {
        match $attrs.remove($attr) {
            Some(QuotedOrUnquoted::Quoted(s)) => Some(
                ($parse(s.as_str())).map_err(|_| format!("Can't create attribute {}", $attr))?,
            ),
            Some(QuotedOrUnquoted::Unquoted(_)) => {
                return Err(format!(
                    "Can't create {} attribute from unquoted string",
                    $attr
                ))
            }
            None => None,
        }
    };
    ($attrs:expr, $attr:expr) => {
        quoted_string_parse!($attrs, $attr, |s: &str| s.parse())
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
#[derive(Debug, Default, PartialEq, Clone)]
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
        if self.independent_segments {
            writeln!(w, "#EXT-X-INDEPENDENT-SEGMENTS")?;
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
#[derive(Debug, Default, PartialEq, Clone)]
pub struct VariantStream {
    pub is_i_frame: bool,
    pub uri: String,

    // <attribute-list>
    pub bandwidth: u64,
    pub average_bandwidth: Option<u64>,
    pub codecs: Option<String>,
    pub resolution: Option<Resolution>,
    pub frame_rate: Option<f64>,
    pub hdcp_level: Option<HDCPLevel>,
    pub audio: Option<String>,
    pub video: Option<String>,
    pub subtitles: Option<String>,
    pub closed_captions: Option<ClosedCaptionGroupId>,
    // PROGRAM-ID tag was removed in protocol version 6
    pub other_attributes: Option<HashMap<String, QuotedOrUnquoted>>,
}

impl VariantStream {
    pub(crate) fn from_hashmap(
        mut attrs: HashMap<String, QuotedOrUnquoted>,
        is_i_frame: bool,
    ) -> Result<VariantStream, String> {
        let uri = quoted_string!(attrs, "URI").unwrap_or_default();
        // TODO: keep in attrs if parsing optional attributes fails
        let bandwidth = unquoted_string_parse!(attrs, "BANDWIDTH", |s: &str| s
            .parse::<u64>()
            .map_err(|err| format!("Failed to parse BANDWIDTH attribute: {}", err)))
        .ok_or_else(|| String::from("EXT-X-STREAM-INF without mandatory BANDWIDTH attribute"))?;
        let average_bandwidth = unquoted_string_parse!(attrs, "AVERAGE-BANDWIDTH", |s: &str| s
            .parse::<u64>()
            .map_err(|err| format!("Failed to parse AVERAGE-BANDWIDTH: {}", err)));
        let codecs = quoted_string!(attrs, "CODECS");
        let resolution = unquoted_string_parse!(attrs, "RESOLUTION");
        let frame_rate = unquoted_string_parse!(attrs, "FRAME-RATE", |s: &str| s
            .parse::<f64>()
            .map_err(|err| format!("Failed to parse FRAME-RATE attribute: {}", err)));
        let hdcp_level = unquoted_string_parse!(attrs, "HDCP-LEVEL");
        let audio = quoted_string!(attrs, "AUDIO");
        let video = quoted_string!(attrs, "VIDEO");
        let subtitles = quoted_string!(attrs, "SUBTITLES");
        let closed_captions = attrs
            .remove("CLOSED-CAPTIONS")
            .map(|c| c.try_into())
            .transpose()?;
        let other_attributes = if attrs.is_empty() { None } else { Some(attrs) };

        Ok(VariantStream {
            is_i_frame,
            uri,
            bandwidth,
            average_bandwidth,
            codecs,
            resolution,
            frame_rate,
            hdcp_level,
            audio,
            video,
            subtitles,
            closed_captions,
            other_attributes,
        })
    }

    pub(crate) fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        if self.is_i_frame {
            write!(w, "#EXT-X-I-FRAME-STREAM-INF:")?;
            self.write_stream_inf_common_attributes(w)?;
            writeln!(w, ",URI=\"{}\"", self.uri)
        } else {
            write!(w, "#EXT-X-STREAM-INF:")?;
            self.write_stream_inf_common_attributes(w)?;
            write_some_attribute_quoted!(w, ",AUDIO", &self.audio)?;
            write_some_attribute_quoted!(w, ",SUBTITLES", &self.subtitles)?;
            if let Some(ref closed_captions) = self.closed_captions {
                match closed_captions {
                    ClosedCaptionGroupId::None => write!(w, ",CLOSED-CAPTIONS=NONE")?,
                    ClosedCaptionGroupId::GroupId(s) => write!(w, ",CLOSED-CAPTIONS=\"{}\"", s)?,
                    ClosedCaptionGroupId::Other(s) => write!(w, ",CLOSED-CAPTIONS={}", s)?,
                }
            }
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
        write_some_attribute_quoted!(w, ",VIDEO", &self.video)?;
        write_some_other_attributes!(w, &self.other_attributes)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct Resolution {
    pub width: u64,
    pub height: u64,
}

impl Display for Resolution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}x{}", self.width, self.height)
    }
}

impl FromStr for Resolution {
    type Err = String;

    fn from_str(s: &str) -> Result<Resolution, String> {
        match s.split_once('x') {
            Some((width, height)) => {
                let width = width
                    .parse::<u64>()
                    .map_err(|err| format!("Can't parse RESOLUTION attribute width: {}", err))?;
                let height = height
                    .parse::<u64>()
                    .map_err(|err| format!("Can't parse RESOLUTION attribute height: {}", err))?;
                Ok(Resolution { width, height })
            }
            None => Err(String::from("Invalid RESOLUTION attribute")),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum HDCPLevel {
    Type0,
    Type1,
    None,
    Other(String),
}

impl FromStr for HDCPLevel {
    type Err = String;

    fn from_str(s: &str) -> Result<HDCPLevel, String> {
        match s {
            "TYPE-0" => Ok(HDCPLevel::Type0),
            "TYPE-1" => Ok(HDCPLevel::Type1),
            "NONE" => Ok(HDCPLevel::None),
            _ => Ok(HDCPLevel::Other(String::from(s))),
        }
    }
}

impl Display for HDCPLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                HDCPLevel::Type0 => "TYPE-0",
                HDCPLevel::Type1 => "TYPE-1",
                HDCPLevel::None => "NONE",
                HDCPLevel::Other(s) => s,
            }
        )
    }
}

/// TODO docs
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum ClosedCaptionGroupId {
    None,
    GroupId(String),
    Other(String),
}

impl TryFrom<QuotedOrUnquoted> for ClosedCaptionGroupId {
    type Error = String;

    fn try_from(s: QuotedOrUnquoted) -> Result<ClosedCaptionGroupId, String> {
        match s {
            QuotedOrUnquoted::Unquoted(s) if s == "NONE" => Ok(ClosedCaptionGroupId::None),
            QuotedOrUnquoted::Unquoted(s) => Ok(ClosedCaptionGroupId::Other(s)),
            QuotedOrUnquoted::Quoted(s) => Ok(ClosedCaptionGroupId::GroupId(s)),
        }
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
    pub instream_id: Option<InstreamId>,
    pub characteristics: Option<String>,
    pub channels: Option<String>,
    pub other_attributes: Option<HashMap<String, QuotedOrUnquoted>>,
}

impl AlternativeMedia {
    pub(crate) fn from_hashmap(
        mut attrs: HashMap<String, QuotedOrUnquoted>,
    ) -> Result<AlternativeMedia, String> {
        let media_type = unquoted_string_parse!(attrs, "TYPE")
            .ok_or_else(|| String::from("EXT-X-MEDIA without mandatory TYPE attribute"))?;
        let uri = quoted_string!(attrs, "URI");

        if media_type == AlternativeMediaType::ClosedCaptions && uri.is_some() {
            return Err(String::from(
                "URI attribute must not be included in CLOSED-CAPTIONS Alternative Medias",
            ));
        }

        let group_id = quoted_string!(attrs, "GROUP-ID")
            .ok_or_else(|| String::from("EXT-X-MEDIA without mandatory GROUP-ID attribute"))?;
        let language = quoted_string!(attrs, "LANGUAGE");
        let assoc_language = quoted_string!(attrs, "ASSOC-LANGUAGE");
        let name = quoted_string!(attrs, "NAME")
            .ok_or_else(|| String::from("EXT-X-MEDIA without mandatory NAME attribute"))?;
        let default = is_yes!(attrs, "DEFAULT");
        let autoselect = is_yes!(attrs, "AUTOSELECT");

        if media_type != AlternativeMediaType::Subtitles && attrs.contains_key("FORCED") {
            return Err(String::from(
                "FORCED attribute must not be included in non-SUBTITLE Alternative Medias",
            ));
        }
        let forced = is_yes!(attrs, "FORCED");

        if media_type != AlternativeMediaType::ClosedCaptions && attrs.contains_key("INSTREAM-ID") {
            return Err(String::from("INSTREAM-ID attribute must not be included in non-CLOSED-CAPTIONS Alternative Medias"));
        } else if media_type == AlternativeMediaType::ClosedCaptions
            && !attrs.contains_key("INSTREAM-ID")
        {
            return Err(String::from(
                "INSTREAM-ID attribute must be included in CLOSED-CAPTIONS Alternative Medias",
            ));
        }
        let instream_id = quoted_string_parse!(attrs, "INSTREAM-ID");
        let characteristics = quoted_string!(attrs, "CHARACTERISTICS");
        let channels = quoted_string!(attrs, "CHANNELS");
        let other_attributes = if attrs.is_empty() { None } else { Some(attrs) };

        Ok(AlternativeMedia {
            media_type,
            uri,
            group_id,
            language,
            assoc_language,
            name,
            default,
            autoselect,
            forced,
            instream_id,
            characteristics,
            channels,
            other_attributes,
        })
    }

    pub(crate) fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-MEDIA:")?;
        write!(w, "TYPE={}", self.media_type)?;
        if self.media_type != AlternativeMediaType::ClosedCaptions {
            write_some_attribute_quoted!(w, ",URI", &self.uri)?;
        }
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
        if self.forced && self.media_type == AlternativeMediaType::Subtitles {
            write!(w, ",FORCED=YES")?;
        }
        if self.media_type == AlternativeMediaType::ClosedCaptions {
            // FIXME: Mandatory for closed captions
            write_some_attribute_quoted!(w, ",INSTREAM-ID", &self.instream_id)?;
        }
        write_some_attribute_quoted!(w, ",CHARACTERISTICS", &self.characteristics)?;
        write_some_attribute_quoted!(w, ",CHANNELS", &self.channels)?;
        write_some_other_attributes!(w, &self.other_attributes)?;
        writeln!(w)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum AlternativeMediaType {
    Audio,
    Video,
    Subtitles,
    ClosedCaptions,
    Other(String),
}

impl FromStr for AlternativeMediaType {
    type Err = String;

    fn from_str(s: &str) -> Result<AlternativeMediaType, String> {
        match s {
            "AUDIO" => Ok(AlternativeMediaType::Audio),
            "VIDEO" => Ok(AlternativeMediaType::Video),
            "SUBTITLES" => Ok(AlternativeMediaType::Subtitles),
            "CLOSED-CAPTIONS" => Ok(AlternativeMediaType::ClosedCaptions),
            _ => Ok(AlternativeMediaType::Other(String::from(s))),
        }
    }
}

impl Default for AlternativeMediaType {
    fn default() -> AlternativeMediaType {
        AlternativeMediaType::Video
    }
}

impl Display for AlternativeMediaType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AlternativeMediaType::Audio => "AUDIO",
                AlternativeMediaType::Video => "VIDEO",
                AlternativeMediaType::Subtitles => "SUBTITLES",
                AlternativeMediaType::ClosedCaptions => "CLOSED-CAPTIONS",
                AlternativeMediaType::Other(s) => s.as_str(),
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum InstreamId {
    CC(u8),
    Service(u8),
    Other(String),
}

impl FromStr for InstreamId {
    type Err = String;

    fn from_str(s: &str) -> Result<InstreamId, String> {
        if let Some(cc) = s.strip_prefix("CC") {
            let cc = cc
                .parse::<u8>()
                .map_err(|err| format!("Unable to create InstreamId from {:?}: {}", s, err))?;
            Ok(InstreamId::CC(cc))
        } else if let Some(service) = s.strip_prefix("SERVICE") {
            let service = service
                .parse::<u8>()
                .map_err(|err| format!("Unable to create InstreamId from {:?}: {}", s, err))?;
            Ok(InstreamId::Service(service))
        } else {
            Ok(InstreamId::Other(String::from(s)))
        }
    }
}

impl Display for InstreamId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InstreamId::CC(cc) => write!(f, "CC{}", cc),
            InstreamId::Service(service) => write!(f, "SERVICE{}", service),
            InstreamId::Other(s) => write!(f, "{}", s),
        }
    }
}

/// [`#EXT-X-SESSION-KEY:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.4.5)
/// The EXT-X-SESSION-KEY tag allows encryption keys from Media Playlists
/// to be specified in a Master Playlist.  This allows the client to
/// preload these keys without having to read the Media Playlist(s) first.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct SessionKey(pub Key);

impl SessionKey {
    pub(crate) fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
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
    pub other_attributes: Option<HashMap<String, QuotedOrUnquoted>>,
}

impl SessionData {
    pub(crate) fn from_hashmap(
        mut attrs: HashMap<String, QuotedOrUnquoted>,
    ) -> Result<SessionData, String> {
        let data_id = quoted_string!(attrs, "DATA-ID")
            .ok_or_else(|| String::from("EXT-X-SESSION-DATA field without DATA-ID attribute"))?;

        let value = quoted_string!(attrs, "VALUE");
        let uri = quoted_string!(attrs, "URI");

        // SessionData must contain either a VALUE or a URI,
        // but not both https://tools.ietf.org/html/rfc8216#section-4.3.4.4
        let field = match (value, uri) {
            (Some(value), None) => SessionDataField::Value(value),
            (None, Some(uri)) => SessionDataField::Uri(uri),
            (Some(_), Some(_)) => {
                return Err(format![
                    "EXT-X-SESSION-DATA tag {} contains both a value and an URI",
                    data_id
                ])
            }
            (None, None) => {
                return Err(format![
                    "EXT-X-SESSION-DATA tag {} must contain either a value or an URI",
                    data_id
                ])
            }
        };

        let language = quoted_string!(attrs, "LANGUAGE");
        let other_attributes = if attrs.is_empty() { None } else { Some(attrs) };

        Ok(SessionData {
            data_id,
            field,
            language,
            other_attributes,
        })
    }

    pub(crate) fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-SESSION-DATA:")?;
        write!(w, "DATA-ID=\"{}\"", self.data_id)?;
        match &self.field {
            SessionDataField::Value(value) => write!(w, ",VALUE=\"{}\"", value)?,
            SessionDataField::Uri(uri) => write!(w, ",URI=\"{}\"", uri)?,
        };
        write_some_attribute_quoted!(w, ",LANGUAGE", &self.language)?;
        write_some_other_attributes!(w, &self.other_attributes)?;
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
    pub target_duration: u64,
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
    /// Unknown tags before the first media segment
    pub unknown_tags: Vec<ExtTag>,
}

impl MediaPlaylist {
    pub fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        writeln!(w, "#EXTM3U")?;

        if let Some(ref v) = self.version {
            writeln!(w, "#EXT-X-VERSION:{}", v)?;
        }
        if self.independent_segments {
            writeln!(w, "#EXT-X-INDEPENDENT-SEGMENTS")?;
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
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum MediaPlaylistType {
    Event,
    Vod,
    Other(String),
}

impl FromStr for MediaPlaylistType {
    type Err = String;

    fn from_str(s: &str) -> Result<MediaPlaylistType, String> {
        match s {
            "EVENT" => Ok(MediaPlaylistType::Event),
            "VOD" => Ok(MediaPlaylistType::Vod),
            _ => Ok(MediaPlaylistType::Other(String::from(s))),
        }
    }
}

impl Display for MediaPlaylistType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MediaPlaylistType::Event => "EVENT",
                MediaPlaylistType::Vod => "VOD",
                MediaPlaylistType::Other(s) => s,
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
    pub program_date_time: Option<chrono::DateTime<chrono::FixedOffset>>,
    /// `#EXT-X-DATERANGE:<attribute-list>`
    pub daterange: Option<DateRange>,
    /// `#EXT-`
    pub unknown_tags: Vec<ExtTag>,
}

impl MediaSegment {
    pub fn empty() -> MediaSegment {
        Default::default()
    }

    pub(crate) fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        if let Some(ref byte_range) = self.byte_range {
            write!(w, "#EXT-X-BYTERANGE:")?;
            byte_range.write_value_to(w)?;
            writeln!(w)?;
        }
        if self.discontinuity {
            writeln!(w, "#EXT-X-DISCONTINUITY")?;
        }

        fn write_key<T: Write>(w: &mut T, key: &Option<Key>) -> std::io::Result<()> {
            if let Some(ref key) = key {
                write!(w, "#EXT-X-KEY:")?;
                key.write_attributes_to(w)?;
                writeln!(w)?;
            }
            Ok(())
        }
        fn write_map<T: Write>(w: &mut T, map: &Option<Map>) -> std::io::Result<()> {
            if let Some(ref map) = map {
                write!(w, "#EXT-X-MAP:")?;
                map.write_attributes_to(w)?;
                writeln!(w)?;
            }
            Ok(())
        }

        let map_after_key = self.map.as_ref().map(|m| m.after_key).unwrap_or(false);
        if map_after_key {
            write_key(w, &self.key)?;
            write_map(w, &self.map)?;
        } else {
            write_map(w, &self.map)?;
            write_key(w, &self.key)?;
        }

        if let Some(ref v) = self.program_date_time {
            writeln!(
                w,
                "#EXT-X-PROGRAM-DATE-TIME:{}",
                v.to_rfc3339_opts(chrono::SecondsFormat::Millis, true)
            )?;
        }
        if let Some(ref v) = self.daterange {
            write!(w, "#EXT-X-DATERANGE:")?;
            v.write_attributes_to(w)?;
            writeln!(w)?;
        }
        for unknown_tag in &self.unknown_tags {
            writeln!(w, "{}", unknown_tag)?;
        }

        match WRITE_OPT_FLOAT_PRECISION.load(Ordering::Relaxed) {
            MAX => {
                write!(w, "#EXTINF:{},", self.duration)?;
            }
            n => {
                write!(w, "#EXTINF:{:.*},", n, self.duration)?;
            }
        };

        if let Some(ref v) = self.title {
            writeln!(w, "{}", v)?;
        } else {
            writeln!(w)?;
        }

        writeln!(w, "{}", self.uri)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum KeyMethod {
    None,
    AES128,
    SampleAES,
    Other(String),
}

impl Default for KeyMethod {
    fn default() -> Self {
        KeyMethod::None
    }
}

impl FromStr for KeyMethod {
    type Err = String;

    fn from_str(s: &str) -> Result<KeyMethod, String> {
        match s {
            "NONE" => Ok(KeyMethod::None),
            "AES-128" => Ok(KeyMethod::AES128),
            "SAMPLE-AES" => Ok(KeyMethod::SampleAES),
            _ => Ok(KeyMethod::Other(String::from(s))),
        }
    }
}

impl Display for KeyMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                KeyMethod::None => "NONE",
                KeyMethod::AES128 => "AES-128",
                KeyMethod::SampleAES => "SAMPLE-AES",
                KeyMethod::Other(s) => s,
            }
        )
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
    pub method: KeyMethod,
    pub uri: Option<String>,
    pub iv: Option<String>,
    pub keyformat: Option<String>,
    pub keyformatversions: Option<String>,
}

impl Key {
    pub(crate) fn from_hashmap(
        mut attrs: HashMap<String, QuotedOrUnquoted>,
    ) -> Result<Key, String> {
        let method: KeyMethod = unquoted_string_parse!(attrs, "METHOD")
            .ok_or_else(|| String::from("EXT-X-KEY without mandatory METHOD attribute"))?;

        let uri = quoted_string!(attrs, "URI");
        let iv = unquoted_string!(attrs, "IV");
        if method == KeyMethod::None && iv.is_none() {
            return Err("IV is required unless METHOD is NONE".parse().unwrap());
        }
        let keyformat = quoted_string!(attrs, "KEYFORMAT");
        let keyformatversions = quoted_string!(attrs, "KEYFORMATVERSIONS");

        Ok(Key {
            method,
            uri,
            iv,
            keyformat,
            keyformatversions,
        })
    }

    pub fn write_attributes_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "METHOD={}", self.method)?;
        write_some_attribute_quoted!(w, ",URI", &self.uri)?;
        write_some_attribute!(w, ",IV", &self.iv)?;
        write_some_attribute_quoted!(w, ",KEYFORMAT", &self.keyformat)?;
        write_some_attribute_quoted!(w, ",KEYFORMATVERSIONS", &self.keyformatversions)
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
    pub other_attributes: HashMap<String, QuotedOrUnquoted>,
    /// Whether the EXT-X-MAP tag locates after the EXT-X-KEY tag
    pub after_key: bool,
}

impl Map {
    pub fn write_attributes_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "URI=\"{}\"", self.uri)?;
        if let Some(ref byte_range) = self.byte_range {
            write!(w, ",BYTERANGE=\"")?;
            byte_range.write_value_to(w)?;
            write!(w, "\"")?;
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
#[derive(Debug, PartialEq, Clone)]
pub struct DateRange {
    pub id: String,
    pub class: Option<String>,
    pub start_date: chrono::DateTime<chrono::FixedOffset>,
    pub end_date: Option<chrono::DateTime<chrono::FixedOffset>>,
    pub duration: Option<f64>,
    pub planned_duration: Option<f64>,
    pub x_prefixed: Option<HashMap<String, QuotedOrUnquoted>>, //  X-<client-attribute>
    pub end_on_next: bool,
    pub other_attributes: Option<HashMap<String, QuotedOrUnquoted>>,
}

impl DateRange {
    pub fn from_hashmap(mut attrs: HashMap<String, QuotedOrUnquoted>) -> Result<DateRange, String> {
        let id = quoted_string!(attrs, "ID")
            .ok_or_else(|| String::from("EXT-X-DATERANGE without mandatory ID attribute"))?;
        let class = quoted_string!(attrs, "CLASS");
        let start_date =
            quoted_string_parse!(attrs, "START-DATE", chrono::DateTime::parse_from_rfc3339)
                .ok_or_else(|| {
                    String::from("EXT-X-DATERANGE without mandatory START-DATE attribute")
                })?;
        let end_date =
            quoted_string_parse!(attrs, "END-DATE", chrono::DateTime::parse_from_rfc3339);
        let duration = unquoted_string_parse!(attrs, "DURATION", |s: &str| s
            .parse::<f64>()
            .map_err(|err| format!("Failed to parse DURATION attribute: {}", err)));
        let planned_duration = unquoted_string_parse!(attrs, "PLANNED-DURATION", |s: &str| s
            .parse::<f64>()
            .map_err(|err| format!("Failed to parse PLANNED-DURATION attribute: {}", err)));
        let end_on_next = is_yes!(attrs, "END-ON-NEXT");
        let mut x_prefixed = HashMap::new();
        let mut other_attributes = HashMap::new();
        for (k, v) in attrs.into_iter() {
            if k.starts_with("X-") {
                x_prefixed.insert(k, v);
            } else {
                other_attributes.insert(k, v);
            }
        }

        Ok(DateRange {
            id,
            class,
            start_date,
            end_date,
            duration,
            planned_duration,
            x_prefixed: if x_prefixed.is_empty() {
                None
            } else {
                Some(x_prefixed)
            },
            end_on_next,
            other_attributes: if other_attributes.is_empty() {
                None
            } else {
                Some(other_attributes)
            },
        })
    }

    pub fn write_attributes_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write_some_attribute_quoted!(w, "ID", &Some(&self.id))?;
        write_some_attribute_quoted!(w, ",CLASS", &self.class)?;
        write_some_attribute_quoted!(w, ",START-DATE", &Some(&self.start_date.to_rfc3339()))?;
        write_some_attribute_quoted!(
            w,
            ",END-DATE",
            &self.end_date.as_ref().map(|dt| dt.to_rfc3339())
        )?;
        write_some_attribute!(w, ",DURATION", &self.duration)?;
        write_some_attribute!(w, ",PLANNED-DURATION", &self.planned_duration)?;
        if let Some(x_prefixed) = &self.x_prefixed {
            for (name, attr) in x_prefixed {
                write!(w, ",{}={}", name, attr)?;
            }
        }
        if self.end_on_next {
            write!(w, ",END-ON-NEXT=YES")?;
        }
        if let Some(other_attributes) = &self.other_attributes {
            for (name, attr) in other_attributes {
                write!(w, ",{}={}", name, attr)?;
            }
        }
        Ok(())
    }
}

// -----------------------------------------------------------------------------------------------
// Rest
// -----------------------------------------------------------------------------------------------

/// [`#EXT-X-START:<attribute-list>`](https://tools.ietf.org/html/draft-pantos-http-live-streaming-19#section-4.3.5.2)
///
/// The EXT-X-START tag indicates a preferred point at which to start
/// playing a Playlist. By default, clients SHOULD start playback at
/// this point when beginning a playback session.
#[derive(Debug, Default, PartialEq, Clone)]
pub struct Start {
    pub time_offset: f64,
    pub precise: Option<bool>,
    pub other_attributes: HashMap<String, QuotedOrUnquoted>,
}

impl Start {
    pub(crate) fn from_hashmap(
        mut attrs: HashMap<String, QuotedOrUnquoted>,
    ) -> Result<Start, String> {
        let time_offset = unquoted_string_parse!(attrs, "TIME-OFFSET", |s: &str| s
            .parse::<f64>()
            .map_err(|err| format!("Failed to parse TIME-OFFSET attribute: {}", err)))
        .ok_or_else(|| String::from("EXT-X-START without mandatory TIME-OFFSET attribute"))?;
        Ok(Start {
            time_offset,
            precise: is_yes!(attrs, "PRECISE").into(),
            other_attributes: attrs,
        })
    }

    pub(crate) fn write_to<T: Write>(&self, w: &mut T) -> std::io::Result<()> {
        write!(w, "#EXT-X-START:TIME-OFFSET={}", self.time_offset)?;
        if let Some(precise) = self.precise {
            if precise {
                write!(w, ",PRECISE=YES")?;
            }
        }
        writeln!(w)?;

        Ok(())
    }
}

/// A simple `#EXT-` tag
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct ExtTag {
    pub tag: String,
    pub rest: Option<String>,
}

impl Display for ExtTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
