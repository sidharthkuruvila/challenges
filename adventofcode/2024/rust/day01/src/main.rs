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
    println!("Part 2, n^2: {}", total);

    let groupeda = group(&alist);
    let groupedb = group(&blist);
    let mut ai = 0;
    let mut bi = 0;
    let mut total: u32 = 0;
    while ai < groupeda.len() && bi < groupedb.len() {
        let (numa, ca) = groupeda[ai];
        let (numb, cb) = groupedb[bi];
        if numa == numb {
            total += numa * ca * cb;
            ai += 1;
            bi += 1;
        } else if numa < numb {
            ai += 1;
        } else if numa > numb {
            bi += 1;
        }
    }
    println!("Part 2, n lg n: {}", total);
}

fn group(v: &Vec<u32>) -> Vec<(u32, u32)> {
    let mut grouped: Vec<(u32, u32)> = vec![];
    let mut n = 0;
    let mut a = v[0];

    for i in 0..v.len() {
        if v[i] != a {
            grouped.push((a, n));
            a = v[i];
            n = 0;
        }
        n += 1;
    }
    grouped.push((a, n));
    grouped
}
