use m3u8_rs::playlist::Playlist;
use std::io::Read;

fn main() {
    let mut file = std::fs::File::open("playlist.m3u8").unwrap();
    let mut bytes: Vec<u8> = Vec::new();
    file.read_to_end(&mut bytes).unwrap();

    let parsed = m3u8_rs::parse_playlist_res(&bytes);

    match parsed {
        Ok(Playlist::MasterPlaylist(pl)) => println!("Master playlist:\n{:?}", pl),
        Ok(Playlist::MediaPlaylist(pl)) => println!("Media playlist:\n{:?}", pl),
        Err(e) => println!("Error: {:?}", e),
    }
}
