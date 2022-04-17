use crate::attributes::QuotedOrUnquoted::{Quoted, Unquoted};
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum QuotedOrUnquoted {
    Unquoted(String),
    Quoted(String),
}

impl Default for QuotedOrUnquoted {
    fn default() -> Self {
        Quoted(String::new())
    }
}

impl From<&str> for QuotedOrUnquoted {
    fn from(s: &str) -> Self {
        if s.starts_with('"') && s.ends_with('"') {
            return Quoted(s.trim_matches('"').to_string());
        }
        Unquoted(s.to_string())
    }
}

impl fmt::Display for QuotedOrUnquoted {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Unquoted(s) => write!(f, "{}", s),
            Quoted(u) => write!(f, "{}", u),
        }
    }
}

// EXT-X-KEY
//
// METHOD
// The value is an enumerated-string that specifies the encryption
// method. The methods defined are: NONE, AES-128, and SAMPLE-AES.
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum KeyMethod {
    None,
    AES_128,
    SAMPLE_AES,
    Enum(String),
}

impl Default for KeyMethod {
    fn default() -> Self {
        KeyMethod::None
    }
}

impl From<QuotedOrUnquoted> for KeyMethod {
    fn from(s: QuotedOrUnquoted) -> Self {
        match s {
            QuotedOrUnquoted::Unquoted(s) if s == "NONE" => KeyMethod::None,
            QuotedOrUnquoted::Unquoted(s) if s == "AES-128" => KeyMethod::AES_128,
            QuotedOrUnquoted::Unquoted(s) if s == "SAMPLE-AES" => KeyMethod::SAMPLE_AES,
            _ => KeyMethod::Enum(s.to_string()),
        }
    }
}
impl From<&str> for KeyMethod {
    fn from(s: &str) -> Self {
        QuotedOrUnquoted::from(s).into()
    }
}

impl fmt::Display for KeyMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            KeyMethod::None => write!(f, "NONE"),
            KeyMethod::AES_128 => write!(f, "AES-128"),
            KeyMethod::SAMPLE_AES => write!(f, "SAMPLE-AES"),
            KeyMethod::Enum(s) => write!(f, "{}", s),
        }
    }
}

// EXT-X-STREAM-INF:
//
// HDCP-LEVEL
// The value is an enumerated-string; valid strings are TYPE-0, TYPE-
// 1, and NONE
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum HdcpLevel {
    Type0,
    Type1,
    None,
    Enum(String),
}

impl From<QuotedOrUnquoted> for HdcpLevel {
    fn from(s: QuotedOrUnquoted) -> Self {
        match s {
            QuotedOrUnquoted::Unquoted(s) if s == "NONE" => HdcpLevel::None,
            QuotedOrUnquoted::Unquoted(s) if s == "TYPE-0" => HdcpLevel::Type0,
            QuotedOrUnquoted::Unquoted(s) if s == "TYPE-1" => HdcpLevel::Type1,
            _ => HdcpLevel::Enum(s.to_string()),
        }
    }
}

impl From<&str> for HdcpLevel {
    fn from(s: &str) -> Self {
        QuotedOrUnquoted::from(s).into()
    }
}

impl fmt::Display for HdcpLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HdcpLevel::None => write!(f, "NONE"),
            HdcpLevel::Type0 => write!(f, "TYPE-0"),
            HdcpLevel::Type1 => write!(f, "Type-1"),
            HdcpLevel::Enum(s) => write!(f, "{}", s),
        }
    }
}

// EXT-X-STREAM-INF
//
// CLOSED-CAPTIONS
// The value can be either a quoted-string or an enumerated-string
// with the value NONE.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ClosedCaptions {
    None,
    GroupId(String),
    Enum(String),
}

impl From<QuotedOrUnquoted> for ClosedCaptions {
    fn from(s: QuotedOrUnquoted) -> Self {
        match s {
            QuotedOrUnquoted::Unquoted(s) if s == "NONE" => ClosedCaptions::None,
            QuotedOrUnquoted::Quoted(gid) => ClosedCaptions::GroupId(gid),
            QuotedOrUnquoted::Unquoted(e) => ClosedCaptions::Enum(e),
        }
    }
}

impl From<&str> for ClosedCaptions {
    fn from(s: &str) -> Self {
        QuotedOrUnquoted::from(s).into()
    }
}

impl fmt::Display for ClosedCaptions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClosedCaptions::None => write!(f, "NONE"),
            ClosedCaptions::GroupId(gid) => write!(f, "{}", gid),
            ClosedCaptions::Enum(e) => write!(f, "{}", e),
        }
    }
}
