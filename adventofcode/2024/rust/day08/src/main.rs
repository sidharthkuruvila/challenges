use std::fs::File;
use std::io::prelude::*;
use std::path::Path;


fn main() {
    let contents = read_file("../../input/day06/input.txt").as_bytes().to_vec();
    println!("Part 1: {:#?}", contents);
}


fn read_file(filename: &str) -> String {
    let path = Path::new(filename);
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}
