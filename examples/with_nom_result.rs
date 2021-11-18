use m3u8_rs::Playlist;
use std::io::Read;

fn main() {
    let mut file = std::fs::File::open("playlist.m3u8").unwrap();
    let mut bytes: Vec<u8> = Vec::new();
    file.read_to_end(&mut bytes).unwrap();

    let parsed = m3u8_rs::parse_playlist(&bytes);

    let playlist = match parsed {
        Result::Ok((_i, playlist)) => playlist,
        Result::Err(e) => panic!("Parsing error: \n{}", e),
    };

    match playlist {
        Playlist::MasterPlaylist(pl) => println!("Master playlist:\n{:?}", pl),
        Playlist::MediaPlaylist(pl) => println!("Media playlist:\n{:?}", pl),
    }
}

#[allow(unused)]
fn main_alt() {
    let mut file = std::fs::File::open("playlist.m3u8").unwrap();
    let mut bytes: Vec<u8> = Vec::new();
    file.read_to_end(&mut bytes).unwrap();

    let parsed = m3u8_rs::parse_playlist(&bytes);

    match parsed {
        Result::Ok((_i, Playlist::MasterPlaylist(pl))) => println!("Master playlist:\n{:?}", pl),
        Result::Ok((_i, Playlist::MediaPlaylist(pl))) => println!("Media playlist:\n{:?}", pl),
        Result::Err(e) => panic!("Parsing error: \n{}", e),
    }
}
