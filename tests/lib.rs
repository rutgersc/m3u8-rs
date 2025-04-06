#![allow(unused_variables, unused_imports, dead_code)]

use chrono::prelude::*;
use m3u8_rs::QuotedOrUnquoted::Quoted;
use m3u8_rs::*;
use nom::AsBytes;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path;
use std::sync::atomic::Ordering;
use std::{fs, io};

fn all_sample_m3u_playlists() -> Vec<path::PathBuf> {
    let path: path::PathBuf = ["sample-playlists"].iter().collect();
    fs::read_dir(path.to_str().unwrap())
        .unwrap()
        .filter_map(Result::ok)
        .map(|dir| dir.path())
        .filter(|path| path.extension().map_or(false, |ext| ext == "m3u8"))
        .collect()
}

fn getm3u(path: &str) -> String {
    let mut buf = String::new();
    let mut file = File::open(path).unwrap_or_else(|_| panic!("Can't find m3u8: {}", path));
    let u = file.read_to_string(&mut buf).expect("Can't read file");
    buf
}

fn get_sample_playlist(name: &str) -> String {
    let path: path::PathBuf = ["sample-playlists", name].iter().collect();
    getm3u(path.to_str().unwrap())
}

// -----------------------------------------------------------------------------------------------
// Playlist

fn print_parse_playlist_test(playlist_name: &str) -> bool {
    let input: String = get_sample_playlist(playlist_name);
    println!("Parsing playlist file: {:?}", playlist_name);
    let parsed = parse_playlist(input.as_bytes());

    if let Ok((i, o)) = &parsed {
        println!("{:?}", o);
        true
    } else {
        println!("Parsing failed:\n {:?}", parsed);
        false
    }
}

#[test]
fn playlist_master_with_alternatives() {
    assert!(print_parse_playlist_test("master-with-alternatives.m3u8"));
}

#[test]
fn playlist_master_with_alternatives_2_3() {
    assert!(print_parse_playlist_test("master-with-alternatives-2.m3u8"));
}

#[test]
fn playlist_master_with_i_frame_stream_inf() {
    assert!(print_parse_playlist_test(
        "master-with-i-frame-stream-inf.m3u8"
    ));
}

#[test]
fn playlist_master_with_multiple_codecs() {
    assert!(print_parse_playlist_test(
        "master-with-multiple-codecs.m3u8"
    ));
}

// -- Media playlists

#[test]
fn playlist_media_standard() {
    assert!(print_parse_playlist_test("mediaplaylist.m3u8"));
}

#[test]
fn playlist_media_without_segments() {
    assert!(print_parse_playlist_test(
        "media-playlist-without-segments.m3u8"
    ));
}

#[test]
fn playlist_media_with_cues() {
    assert!(print_parse_playlist_test("media-playlist-with-cues.m3u8"));
}

#[test]
fn playlist_media_with_cues1() {
    assert!(print_parse_playlist_test("media-playlist-with-cues-1.m3u8"));
}

#[test]
fn playlist_media_with_scte35() {
    assert!(print_parse_playlist_test("media-playlist-with-scte35.m3u8"));
}

#[test]
fn playlist_media_with_scte35_1() {
    assert!(print_parse_playlist_test(
        "media-playlist-with-scte35-1.m3u8"
    ));
}

// -----------------------------------------------------------------------------------------------
// Playlist with no newline end

#[test]
fn playlist_not_ending_in_newline_master() {
    assert!(print_parse_playlist_test(
        "master-not-ending-in-newline.m3u8"
    ));
}

#[test]
fn playlist_not_ending_in_newline_master1() {
    assert!(print_parse_playlist_test(
        "master-not-ending-in-newline-1.m3u8"
    ));
}

#[test]
fn playlist_not_ending_in_newline_media() {
    assert!(print_parse_playlist_test(
        "media-not-ending-in-newline.m3u8"
    ));
}

// -----------------------------------------------------------------------------------------------
// Playlist type detection tests

#[test]
fn playlist_type_is_master() {
    let input = get_sample_playlist("master.m3u8");
    let result = is_master_playlist(input.as_bytes());
    assert!(result);
}

// #[test]
// fn playlist_type_with_unknown_tag() {
//     let input = get_sample_playlist("!!");
//     let result = is_master_playlist(input.as_bytes());
//     println!("Playlist_type_with_unknown_tag is master playlist: {:?}", result);
//     assert_eq!(true, result);
// }

#[test]
fn playlist_types() {
    for path_buf in all_sample_m3u_playlists() {
        let path = path_buf.to_str().unwrap();
        let input = getm3u(path);
        let is_master = is_master_playlist(input.as_bytes());

        println!("{:?} = {:?}", path, is_master);

        assert_eq!(path.to_lowercase().contains("master"), is_master);
    }
}

// -----------------------------------------------------------------------------------------------
// Creating playlists

fn print_create_and_parse_playlist(playlist_original: &mut Playlist) -> Playlist {
    let mut utf8: Vec<u8> = Vec::new();
    playlist_original.write_to(&mut utf8).unwrap();

    let m3u8_str: &str = std::str::from_utf8(&utf8).unwrap();

    let playlist_parsed = match *playlist_original {
        Playlist::MasterPlaylist(_) => {
            Playlist::MasterPlaylist(parse_master_playlist_res(m3u8_str.as_bytes()).unwrap())
        }
        Playlist::MediaPlaylist(_) => {
            Playlist::MediaPlaylist(parse_media_playlist_res(m3u8_str.as_bytes()).unwrap())
        }
    };

    print!("\n\n---- utf8 result\n\n{}", m3u8_str);
    print!("\n---- Original\n\n{:?}", playlist_original);
    print!("\n\n---- Parsed\n\n{:?}\n\n", playlist_parsed);

    playlist_parsed
}

#[test]
fn create_and_parse_master_playlist_empty() {
    let mut playlist_original = Playlist::MasterPlaylist(MasterPlaylist {
        ..Default::default()
    });
    let playlist_parsed = print_create_and_parse_playlist(&mut playlist_original);
    assert_eq!(playlist_original, playlist_parsed);
}

#[test]
fn create_segment_float_inf() {
    let playlist = Playlist::MediaPlaylist(MediaPlaylist {
        version: Some(6),
        target_duration: 3,
        media_sequence: 338559,
        discontinuity_sequence: 1234,
        end_list: true,
        playlist_type: Some(MediaPlaylistType::Vod),
        segments: vec![MediaSegment {
            uri: "20140311T113819-01-338559live.ts".into(),
            duration: 2.000f32,
            title: Some("title".into()),
            ..Default::default()
        }],
        ..Default::default()
    });

    let mut v: Vec<u8> = Vec::new();
    playlist.write_to(&mut v).unwrap();
    let m3u8_str: &str = std::str::from_utf8(&v).unwrap();
    assert!(m3u8_str.contains("#EXTINF:2,title"));

    WRITE_OPT_FLOAT_PRECISION.store(5, Ordering::Relaxed);

    playlist.write_to(&mut v).unwrap();
    let m3u8_str: &str = std::str::from_utf8(&v).unwrap();
    assert!(m3u8_str.contains("#EXTINF:2.00000,title"));
}

#[test]
fn create_and_parse_master_playlist_full() {
    let mut playlist_original = Playlist::MasterPlaylist(MasterPlaylist {
        version: Some(6),
        alternatives: vec![
            AlternativeMedia {
                media_type: AlternativeMediaType::Audio,
                uri: Some("alt-media-uri".into()),
                group_id: "group-id".into(),
                language: Some("language".into()),
                assoc_language: Some("assoc-language".into()),
                name: "Xmedia".into(),
                default: true,    // Its absence indicates an implicit value of NO
                autoselect: true, // Its absence indicates an implicit value of NO
                forced: false,    // Its absence indicates an implicit value of NO
                instream_id: None,
                characteristics: Some("characteristics".into()),
                channels: Some("channels".into()),
                other_attributes: Default::default(),
            },
            AlternativeMedia {
                media_type: AlternativeMediaType::Subtitles,
                uri: Some("alt-media-uri".into()),
                group_id: "group-id".into(),
                language: Some("language".into()),
                assoc_language: Some("assoc-language".into()),
                name: "Xmedia".into(),
                default: true,    // Its absence indicates an implicit value of NO
                autoselect: true, // Its absence indicates an implicit value of NO
                forced: true,     // Its absence indicates an implicit value of NO
                instream_id: None,
                characteristics: Some("characteristics".into()),
                channels: Some("channels".into()),
                other_attributes: Default::default(),
            },
            AlternativeMedia {
                media_type: AlternativeMediaType::ClosedCaptions,
                uri: None,
                group_id: "group-id".into(),
                language: Some("language".into()),
                assoc_language: Some("assoc-language".into()),
                name: "Xmedia".into(),
                default: true,    // Its absence indicates an implicit value of NO
                autoselect: true, // Its absence indicates an implicit value of NO
                forced: false,    // Its absence indicates an implicit value of NO
                instream_id: Some(InstreamId::CC(1)),
                characteristics: Some("characteristics".into()),
                channels: Some("channels".into()),
                other_attributes: Default::default(),
            },
        ],
        variants: vec![VariantStream {
            is_i_frame: false,
            uri: "masterplaylist-uri".into(),
            bandwidth: 10010010,
            average_bandwidth: Some(10010010),
            codecs: Some("TheCODEC".into()),
            resolution: Some(Resolution {
                width: 1000,
                height: 3000,
            }),
            frame_rate: Some(60.0),
            hdcp_level: Some(HDCPLevel::None),
            audio: Some("audio".into()),
            video: Some("video".into()),
            subtitles: Some("subtitles".into()),
            closed_captions: Some(ClosedCaptionGroupId::GroupId("closed_captions".into())),
            other_attributes: Default::default(),
        }],
        session_data: vec![SessionData {
            data_id: "****".into(),
            field: SessionDataField::Value("%%%%".to_string()),
            language: Some("SessionDataLanguage".into()),
            other_attributes: Default::default(),
        }],
        session_key: vec![SessionKey(Key {
            method: KeyMethod::AES128,
            uri: Some("https://secure.domain.com".into()),
            iv: Some("0xb059217aa2649ce170b734".into()),
            keyformat: Some("xXkeyformatXx".into()),
            keyformatversions: Some("xXFormatVers".into()),
        })],
        start: Some(Start {
            time_offset: "123123123".parse().unwrap(),
            precise: Some(true),
            other_attributes: Default::default(),
        }),
        independent_segments: true,
        unknown_tags: vec![],
    });
    let playlist_parsed = print_create_and_parse_playlist(&mut playlist_original);
    assert_eq!(playlist_original, playlist_parsed);
}

#[test]
fn create_and_parse_media_playlist_empty() {
    let mut playlist_original = Playlist::MediaPlaylist(MediaPlaylist {
        ..Default::default()
    });
    let playlist_parsed = print_create_and_parse_playlist(&mut playlist_original);
    assert_eq!(playlist_original, playlist_parsed);
}

#[test]
fn create_and_parse_media_playlist_single_segment() {
    let mut playlist_original = Playlist::MediaPlaylist(MediaPlaylist {
        target_duration: 2,
        segments: vec![MediaSegment {
            uri: "20140311T113819-01-338559live.ts".into(),
            duration: 2.002,
            title: Some("hey".into()),
            ..Default::default()
        }],
        ..Default::default()
    });
    let playlist_parsed = print_create_and_parse_playlist(&mut playlist_original);
    assert_eq!(playlist_original, playlist_parsed);
}

#[test]
fn create_and_parse_media_playlist_full() {
    let mut playlist_original = Playlist::MediaPlaylist(MediaPlaylist {
        version: Some(4),
        target_duration: 3,
        media_sequence: 338559,
        discontinuity_sequence: 1234,
        end_list: true,
        playlist_type: Some(MediaPlaylistType::Vod),
        i_frames_only: true,
        start: Some(Start {
            time_offset: "9999".parse().unwrap(),
            precise: Some(true),
            other_attributes: Default::default(),
        }),
        independent_segments: true,
        segments: vec![MediaSegment {
            uri: "20140311T113819-01-338559live.ts".into(),
            duration: 2.002,
            title: Some("338559".into()),
            byte_range: Some(ByteRange {
                length: 137116,
                offset: Some(4559),
            }),
            discontinuity: true,
            key: Some(Key {
                method: KeyMethod::None,
                uri: Some("https://secure.domain.com".into()),
                iv: Some("0xb059217aa2649ce170b734".into()),
                keyformat: Some("xXkeyformatXx".into()),
                keyformatversions: Some("xXFormatVers".into()),
            }),
            map: Some(Map {
                uri: "www.map-uri.com".into(),
                byte_range: Some(ByteRange {
                    length: 137116,
                    offset: Some(4559),
                }),
                other_attributes: Default::default(),
                after_key: true,
            }),
            program_date_time: Some(
                chrono::FixedOffset::east(8 * 3600)
                    .ymd(2010, 2, 19)
                    .and_hms_milli(14, 54, 23, 31),
            ),
            daterange: Some(DateRange {
                id: "9999".into(),
                class: Some("class".into()),
                start_date: chrono::FixedOffset::east(8 * 3600)
                    .ymd(2010, 2, 19)
                    .and_hms_milli(14, 54, 23, 31),
                end_date: None,
                duration: None,
                planned_duration: Some("40.000".parse().unwrap()),
                x_prefixed: Some(HashMap::from([(
                    "X-client-attribute".into(),
                    "whatever".into(),
                )])),
                end_on_next: false,
                other_attributes: Default::default(),
            }),
            unknown_tags: vec![ExtTag {
                tag: "X-CUE-OUT".into(),
                rest: Some("DURATION=2.002".into()),
            }],
            ..Default::default()
        }],
        unknown_tags: vec![],
    });
    let playlist_parsed = print_create_and_parse_playlist(&mut playlist_original);
    assert_eq!(playlist_original, playlist_parsed);
}

//
// Roundtrip

#[test]
fn parsing_write_to_should_produce_the_same_structure() {
    for playlist in all_sample_m3u_playlists() {
        let input = getm3u(playlist.to_str().unwrap());

        let expected = parse_playlist_res(input.as_bytes()).unwrap();
        let mut written: Vec<u8> = Vec::new();
        expected.write_to(&mut written).unwrap();

        let actual = parse_playlist_res(&written).unwrap();

        assert_eq!(
            expected,
            actual,
            "\n\nFailed parser input:\n\n{}\n\nOriginal input:\n\n{}",
            std::str::from_utf8(&written).unwrap(),
            input
        );
    }
}

// Failure on arbitrary text files that don't start with #EXTM3U8

#[test]
fn parsing_text_file_should_fail() {
    let s = "
Lorem ipsum dolor sit amet, consectetur adipiscing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia
deserunt mollit anim id est laborum.
    ";
    let res = parse_master_playlist_res(s.as_bytes());

    assert!(res.is_err());
}

#[test]
fn parsing_binary_data_should_fail_cleanly() {
    let data = (0..1024).map(|i| (i % 255) as u8).collect::<Vec<u8>>();
    let res = parse_master_playlist_res(&data);

    assert!(res.is_err());
}
