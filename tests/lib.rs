#![allow(unused_variables, unused_imports, dead_code)]

extern crate m3u8_rs;
extern crate nom;

use m3u8_rs::playlist::*;
use m3u8_rs::*;
use nom::*;
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::path;

fn all_sample_m3u_playlists() -> Vec<path::PathBuf> {
    let path: std::path::PathBuf = ["sample-playlists"].iter().collect();
    fs::read_dir(path.to_str().unwrap())
        .unwrap()
        .filter_map(Result::ok)
        .map(|dir| dir.path())
        .filter(|path| path.extension().map_or(false, |ext| ext == "m3u8"))
        .collect()
}

fn getm3u(path: &str) -> String {
    let mut buf = String::new();
    let mut file = fs::File::open(path).expect(&format!("Can't find m3u8: {}", path));
    let u = file.read_to_string(&mut buf).expect("Can't read file");
    buf
}

fn get_sample_playlist(name: &str) -> String {
    let path: std::path::PathBuf = ["sample-playlists", name].iter().collect();
    getm3u(path.to_str().unwrap())
}

// -----------------------------------------------------------------------------------------------
// Playlist

fn print_parse_playlist_test(playlist_name: &str) -> bool {
    let input: String = get_sample_playlist(playlist_name);
    println!("Parsing playlist file: {:?}", playlist_name);
    let parsed = parse_playlist(input.as_bytes());

    if let Result::Ok((i, o)) = parsed {
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
    assert_eq!(true, result);
}

// #[test]
// fn playlist_type_with_unkown_tag() {
//     let input = get_sample_playlist("!!");
//     let result = is_master_playlist(input.as_bytes());
//     println!("Playlist_type_with_unkown_tag is master playlist: {:?}", result);
//     assert_eq!(true, result);
// }

#[test]
fn playlist_types() {
    for path_buf in all_sample_m3u_playlists() {
        let path = path_buf.to_str().unwrap();
        let input = getm3u(path);
        let is_master = is_master_playlist(input.as_bytes());

        println!("{:?} = {:?}", path, is_master);

        assert!(path.to_lowercase().contains("master") == is_master);
    }
}

// -----------------------------------------------------------------------------------------------
// Variant

#[test]
fn variant_stream() {
    let input = b"#EXT-X-STREAM-INF:BANDWIDTH=300000,CODECS=\"xxx\"\n";
    let result = variant_stream_tag(input);
    println!("{:?}", result);
}

// -----------------------------------------------------------------------------------------------
// Other

#[test]
fn test_key_value_pairs_trailing_equals() {
    let res = key_value_pairs(b"BANDWIDTH=395000,CODECS=\"avc1.4d001f,mp4a.40.2\"\r\nrest=");
    println!("{:?}\n\n", res);
}

#[test]
fn test_key_value_pairs_multiple_quoted_values() {
    assert_eq!(
        key_value_pairs(b"BANDWIDTH=86000,URI=\"low/iframe.m3u8\",PROGRAM-ID=1,RESOLUTION=\"1x1\",VIDEO=1\nrest"),
        Result::Ok((
            "\nrest".as_bytes(),
            vec![
                ("BANDWIDTH".to_string(), "86000".to_string()),
                ("URI".to_string(), "low/iframe.m3u8".to_string()),
                ("PROGRAM-ID".to_string(), "1".to_string()),
                ("RESOLUTION".to_string(), "1x1".to_string()),
                ("VIDEO".to_string(), "1".to_string())
            ].into_iter().collect::<HashMap<String,String>>()
        ))
    );
}

#[test]
fn test_key_value_pairs_quotes() {
    let res = key_value_pairs(b"BANDWIDTH=300000,CODECS=\"avc1.42c015,mp4a.40.2\"\r\nrest");
    println!("{:?}\n\n", res);
}

#[test]
fn test_key_value_pairs() {
    let res = key_value_pairs(b"BANDWIDTH=300000,RESOLUTION=22x22,VIDEO=1\r\nrest=");
    println!("{:?}\n\n", res);
}

#[test]
fn test_key_value_pair() {
    assert_eq!(
        key_value_pair(b"PROGRAM-ID=1,rest"),
        Result::Ok((
            "rest".as_bytes(),
            ("PROGRAM-ID".to_string(), "1".to_string())
        ))
    );
}

#[test]
fn comment() {
    assert_eq!(
        comment_tag(b"#Hello\nxxx"),
        Result::Ok(("xxx".as_bytes(), "Hello".to_string()))
    );
}

#[test]
fn quotes() {
    assert_eq!(
        quoted(b"\"value\"rest"),
        Result::Ok(("rest".as_bytes(), "value".to_string()))
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
fn create_and_parse_master_playlist_full() {
    let mut playlist_original = Playlist::MasterPlaylist(MasterPlaylist {
        version: 6,
        alternatives: vec![AlternativeMedia {
            media_type: AlternativeMediaType::Audio,
            uri: Some("alt-media-uri".into()),
            group_id: "group-id".into(),
            language: Some("language".into()),
            assoc_language: Some("assoc-language".into()),
            name: "Xmedia".into(),
            default: true,    // Its absence indicates an implicit value of NO
            autoselect: true, // Its absence indicates an implicit value of NO
            forced: true,     // Its absence indicates an implicit value of NO
            instream_id: Some("instream_id".into()),
            characteristics: Some("characteristics".into()),
            channels: Some("channels".into()),
        }],
        variants: vec![VariantStream {
            is_i_frame: false,
            uri: "masterplaylist-uri".into(),
            bandwidth: "10010010".into(),
            average_bandwidth: Some("10010010".into()),
            codecs: Some("TheCODEC".into()),
            resolution: Some("1000x3000".into()),
            frame_rate: Some("60".into()),
            hdcp_level: Some("NONE".into()),
            audio: Some("audio".into()),
            video: Some("video".into()),
            subtitles: Some("subtitles".into()),
            closed_captions: Some("closed_captions".into()),
        }],
        session_data: vec![SessionData {
            data_id: "****".into(),
            field: SessionDataField::Value("%%%%".to_string()),
            language: Some("SessionDataLanguage".into()),
        }],
        session_key: vec![SessionKey(Key {
            method: "AES-128".into(),
            uri: Some("https://secure.domain.com".into()),
            iv: Some("0xb059217aa2649ce170b734".into()),
            keyformat: Some("xXkeyformatXx".into()),
            keyformatversions: Some("xXFormatVers".into()),
        })],
        start: Some(Start {
            time_offset: "123123123".into(),
            precise: Some("YES".into()),
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
        version: 4,
        target_duration: 3.0,
        media_sequence: 338559,
        discontinuity_sequence: 1234,
        end_list: true,
        playlist_type: Some(MediaPlaylistType::Vod),
        i_frames_only: true,
        start: Some(Start {
            time_offset: "9999".into(),
            precise: Some("YES".into()),
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
                method: "AES-128".into(),
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
            }),
            program_date_time: Some("broodlordinfestorgg".into()),
            daterange: None,
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
