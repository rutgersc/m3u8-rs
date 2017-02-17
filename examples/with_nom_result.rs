extern crate nom;
extern crate m3u8_rs;

use m3u8_rs::playlist::{Playlist};
use std::io::Read;
use nom::IResult;

fn main() {
    let mut file = std::fs::File::open("playlist.m3u8").unwrap();
    let mut bytes: Vec<u8> = Vec::new();
    file.read_to_end(&mut bytes).unwrap();

    let parsed = m3u8_rs::parse_playlist(&bytes);

    let playlist = match parsed {
        IResult::Done(i, playlist) => playlist,
        IResult::Error(e) =>  panic!("Parsing error: \n{}", e),
        IResult::Incomplete(e) => panic!("Parsing error: \n{:?}", e),
    };

    match playlist {
        Playlist::MasterPlaylist(pl) => println!("Master playlist:\n{:?}", pl),
        Playlist::MediaPlaylist(pl) => println!("Media playlist:\n{:?}", pl),
    }
}

fn main_alt() {
    let mut file = std::fs::File::open("playlist.m3u8").unwrap();
    let mut bytes: Vec<u8> = Vec::new();
    file.read_to_end(&mut bytes).unwrap();

    let parsed = m3u8_rs::parse_playlist(&bytes);

    match parsed {
        IResult::Done(i, Playlist::MasterPlaylist(pl)) => println!("Master playlist:\n{:?}", pl),
        IResult::Done(i, Playlist::MediaPlaylist(pl)) => println!("Media playlist:\n{:?}", pl),
        IResult::Error(e) =>  panic!("Parsing error: \n{}", e),
        IResult::Incomplete(e) => panic!("Parsing error: \n{:?}", e),
    }
}
