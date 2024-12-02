use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let path = Path::new("../../input/day02/input.txt");
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let rows: Vec<Vec<i32>> = contents
        .split('\n')
        .map(|s| s.split(' ').map(|s| s.parse::<i32>().unwrap()).collect())
        .collect();
    let mut count = 0;
    for row in &rows {
        if test_row(row) {
            count += 1;
        }
    }
    println!("Part 1: {}", count);

    let mut count = 0;
    'rows: for row in &rows {
        if test_row(row) {
            count += 1;
        } else {
            // remove one item and try again
            for i in 0..row.len() {
                let mut copy: Vec<i32> = Vec::new();
                for n in row {
                    copy.push(*n);
                }
                copy.remove(i);
                if test_row(&copy) {
                    count += 1;
                    continue 'rows;
                }
            }
        }
    }
    println!("Part 2: {}", count);
}

fn test_row(row: &Vec<i32>) -> bool {
    let sign = (row[1] - row[0]).signum();
    for i in 0..row.len() - 1 {
        let d = row[i + 1] - row[i];
        if d.abs() < 1 || d.abs() > 3 || d.signum() != sign {
            return false;
        }
    }
    true
}
