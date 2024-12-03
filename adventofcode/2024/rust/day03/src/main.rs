use regex::Regex;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let path = Path::new("../../input/day03/input.txt");
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    let re = Regex::new(r"mul[(](\d+),(\d+)[)]").unwrap();
    let sum: i32 = re.captures_iter(&contents).map(|cap| {
        let (_, [a, b]) = cap.extract();
        let na = a.parse::<i32>().unwrap();
        let nb = b.parse::<i32>().unwrap();
        na * nb
    }).sum();
    println!("Part 1: {}", sum);


    let command_re = Regex::new(r"mul[(]\d+,\d+[)]|do[(][)]|don't[(][)]").unwrap();
    let nums_re = Regex::new(r"(\d+),(\d+)").unwrap();
    let mut enabled = true;
    let mut sum = 0;
    for (m, []) in command_re.captures_iter(&contents).map(|cap| {cap.extract()}) {
        if m.starts_with("do(") {
            enabled = true;
        } else if m.starts_with("don't(") {
            enabled = false;
        } else {
            if enabled {
                let (_, [a, b]) = nums_re.captures(&m).unwrap().extract();
                let na = a.parse::<i32>().unwrap();
                let nb = b.parse::<i32>().unwrap();
                sum += na * nb
            }

        }
    }
    println!("Part 2: {}", sum);
}
