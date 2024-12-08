use std::cmp::max;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let contents = read_file("../../input/day08/input.txt");
    let lines: Vec<&str> = contents.split("\n").collect();
    let width = lines[0].chars().count();
    let height = lines.len();
    let it = itertools::iproduct!(0..height, 0..width)
        .filter_map(|(x, y)| {
            let ch = lines.get(y).unwrap().chars().nth(x).unwrap();
            if ch != '.' {
                return Some((ch, (x, y)));
            } else {
                return None;
            };
        })
        .into_group_map();

    let nodes:Vec<(i32, i32)> = it
        .iter()
        .flat_map(|(_, v)| {
            let combs = combinations(
                v.clone()
                    .iter()
                    .map(|&(x, y)| (x as i32, y as i32))
                    .collect(),
            );
            let v: HashSet<(i32, i32)> = combs.iter().flat_map(|&((x1, y1), (x2, y2))| {
                let dx = x1 - x2;
                let dy = y1 - y2;
                vec![(x1 + dx, y1 + dy), (x2 - dx, y2 - dy)]
            }).filter(|&(x, y)| x >= 0 && x < height as i32 && y >=0 && y < width as i32 ).collect();
            v
        }).collect();
    let hs: HashSet<(i32, i32)> = nodes.clone().into_iter().collect();
    println!("Part 1: {:?}", hs.len());
    let nodes:Vec<(i32, i32)> = it
        .iter()
        .flat_map(|(_, v)| {
            let combs = combinations(
                v.clone()
                    .iter()
                    .map(|&(x, y)| (x as i32, y as i32))
                    .collect(),
            );
            let v: HashSet<(i32, i32)> = combs.iter().flat_map(|&((x1, y1), (x2, y2))| {
                let dx = x1 - x2;
                let dy = y1 - y2;
                let mut v = vec![];
                let mut nx1 = x1;
                let mut nx2 = x2;
                let mut ny1 = y1;
                let mut ny2 = y2;
                v.push((x1, y1));
                v.push((x2, y2));
                for _ in 0 .. max(height, width) {
                    nx1 = nx1 + dx;
                    ny1 = ny1 + dy;
                    nx2 = nx2 - dx;
                    ny2  = ny2 - dy;
                    v.push((nx1, ny1));
                    v.push((nx2, ny2));
                }
                v
            }).filter(|&(x, y)| x >= 0 && x < height as i32 && y >=0 && y < width as i32 ).collect();
            v
        }).collect();
    let hs: HashSet<(i32, i32)> = nodes.clone().into_iter().collect();
    println!("Part 2: {:?}", hs.len());
}

fn combinations<T: Clone>(v: Vec<T>) -> Vec<(T, T)> {
    let mut res = vec![];
    for i in 0..v.len() {
        for j in i + 1..v.len() {
            res.push((v[i].clone(), v[j].clone()));
        }
    }
    return res;
}

fn read_file(filename: &str) -> String {
    let path = Path::new(filename);
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}
