use std::path::Path;
extern crate rustsourcebundler;
use rustsourcebundler::Bundler;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut bundler: Bundler = Bundler::new(Path::new("src/main.rs"),
                                            Path::new("singlefile.rs"));
    bundler.crate_name("coding_game_2022");
    bundler.run();
    Ok(())
}