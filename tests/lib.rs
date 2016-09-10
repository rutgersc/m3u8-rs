#![allow(unused_variables, unused_imports, dead_code)]

#[macro_use]
extern crate nom;
extern crate m3u8_rs;

use std::fs;
use std::path;
use m3u8_rs::*;
use std::io::Read;
use nom::*;
use std::collections::HashMap;

fn all_sample_m3u_playlists() -> Vec<path::PathBuf> {
    fs::read_dir("sample-playlists\\").unwrap()
        .filter_map(Result::ok)
        .map(|dir| dir.path())
        .filter(|path| path.extension().map_or(false, |ext| ext == "m3u8"))
        .collect()
}

fn getm3u(path: &str) -> String {
    let mut buf = String::new();
    let mut file = fs::File::open(path).expect("Can't find m3u8.");
    let u = file.read_to_string(&mut buf).expect("Can't read file");
    buf
}

fn get_sample_playlist(name: &str) -> String {
    getm3u(&(String::from("sample-playlists\\") + name))
}

// -----------------------------------------------------------------------------------------------
// Playlist

fn print_parse_playlist_test(playlist_name: &str) -> bool {
    let input = get_sample_playlist(playlist_name);
    println!("Parsing playlist file: {:?}", playlist_name);
    let parsed = parse_playlist(input.as_bytes());

    if let IResult::Done(i,o) = parsed {
        println!("{}", o);
        true
    }
    else {
        println!("Parsing failed:\n {:?}", parsed);
        false
    }
}

#[test]
fn playlist_master_with_alternatives() {
    assert!(print_parse_playlist_test("master-with-alternatives.m3u8"));
}

#[test]
fn playlist_master_with_i_frame_stream_inf() {
    assert!(print_parse_playlist_test("master-with-i-frame-stream-inf.m3u8"));
}

#[test]
fn playlist_master_with_multiple_codecs() {
    assert!(print_parse_playlist_test("master-with-multiple-codecs.m3u8"));
}

// -- Media playlists

#[test]
fn playlist_media_standard() {
    assert!(print_parse_playlist_test("mediaplaylist.m3u8"));
}

#[test]
fn playlist_media_without_segments() {
    assert!(print_parse_playlist_test("media-playlist-without-segments.m3u8"));
}

// -----------------------------------------------------------------------------------------------
// Playlist with no newline end

#[test]
fn playlist_not_ending_in_newline_master() {
    assert!(print_parse_playlist_test("master-not-ending-in-newline.m3u8"));
}

#[test]
fn playlist_not_ending_in_newline_media() {
    assert!(print_parse_playlist_test("media-not-ending-in-newline.m3u8"));
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

        assert!(path.to_lowercase().contains("master") == is_master);

        println!("{:?} = {:?}", path, is_master);
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
        IResult::Done(
            "\nrest".as_bytes(),
            vec![
                ("BANDWIDTH".to_string(), "86000".to_string()),
                ("URI".to_string(), "low/iframe.m3u8".to_string()),
                ("PROGRAM-ID".to_string(), "1".to_string()),
                ("RESOLUTION".to_string(), "1x1".to_string()),
                ("VIDEO".to_string(), "1".to_string())
            ].into_iter().collect::<HashMap<String,String>>()
        )
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
        IResult::Done(
            "rest".as_bytes(),
            ("PROGRAM-ID".to_string(), "1".to_string())
        )
    );
}

#[test]
fn comment() {
    assert_eq!(
        comment_tag(b"#Hello\nxxx"),
        IResult::Done("xxx".as_bytes(), "Hello".to_string())
    );
}

#[test]
fn quotes() {
    assert_eq!(
        quoted(b"\"value\"rest"),
        IResult::Done("rest".as_bytes(), "value".to_string())
    );
}

#[test]
fn consume_empty_line() {
    let line = consume_line(b"\r\nrest");
    println!("{:?}", line);
}

#[test]
fn float_() {
    assert_eq!(
        float(b"33.22rest"),
        IResult::Done("rest".as_bytes(), 33.22f32)
    );
}

#[test]
fn float_no_decimal() {
    assert_eq!(
        float(b"33rest"),
        IResult::Done("rest".as_bytes(), 33f32)
    );
}

#[test]
fn float_should_ignore_trailing_dot() {
    assert_eq!(
        float(b"33.rest"),
        IResult::Done(".rest".as_bytes(), 33f32)
    );
}
