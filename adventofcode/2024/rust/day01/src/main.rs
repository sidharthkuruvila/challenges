use regex::Regex;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let path = Path::new("../../input/day01/input.txt");
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    let mut alist: Vec<u32> = vec![];
    let mut blist: Vec<u32> = vec![];
    let re = Regex::new(r"(\w+)\s+(\w+)").unwrap();
    for (_, [a, b]) in re.captures_iter(&contents).map(|c| c.extract()) {
        alist.push(a.parse::<u32>().unwrap());
        blist.push(b.parse::<u32>().unwrap());
    }
    alist.sort();
    blist.sort();
    let mut sum: u32 = 0;
    for i in 0..alist.len() {
        sum += alist[i].abs_diff(blist[i]);
    }
    println!("Part 1: {}", sum);

    let mut total: u32 = 0;
    for a in alist.iter() {
        for b in blist.iter() {
            if a == b {
                total += a;
            }
        }
    }
    println!("Part 2: {}", total);
}
