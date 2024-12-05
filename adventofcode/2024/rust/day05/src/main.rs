use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let path = Path::new("../../input/day05/input.txt");
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let lst: Vec<&str> = contents.split("\n\n").collect::<Vec<&str>>();
    let pairs: Vec<(usize, usize)> = lst[0]
        .split('\n')
        .map(|line| {
            let pl: Vec<usize> = line
                .split('|')
                .map(|ns| ns.parse::<usize>().unwrap())
                .collect();
            return (pl[0], pl[1]);
        })
        .collect();
    let lines: Vec<Vec<usize>> = lst[1]
        .split('\n')
        .map(|line| {
            line.split(',')
                .map(|ns| ns.parse::<usize>().unwrap())
                .collect()
        })
        .collect();
    // println!("Pairs: {:?}", pairs);
    // println!("Lines: {:?}", lines);


    let mut numbers_before: [u128; 100] = [0; 100];
    for (a, b) in pairs.clone() {
        numbers_before[b] = numbers_before[b] | (1 << a)
    }
    let mut numbers_after: [u128; 100] = [0; 100];
    for (a, b) in pairs.clone() {
        numbers_after[a] = numbers_after[a] | (1 << b)
    }
    let mut incorrect: Vec<Vec<usize>> = vec![];

    let mut count = 0;
    'out: for line in lines {
        let mut bits: u128 = 0;
        // println!("Line: {:?}", line);
        for num in line.clone() {
            let mask = numbers_before[num];
            if mask & bits != bits {
                incorrect.push(line.clone());
                continue 'out;
            }
            bits = bits | (1 << num)
        }
        count += line[line.len() / 2];
    }
    println!("Part 1: {}", count);
    let mut count = 0;

    for line in incorrect {
        let mut candidate_pairs: Vec<(usize, usize)> = vec![];
        for (a, b) in pairs.clone() {
            if line.contains(&a) && line.contains(&b) {
                candidate_pairs.push((a, b));
            }
        }
        let mut fin = vec![];
        loop {
            // println!("Candidate pairs: {:?}", candidate_pairs);
            if candidate_pairs.len() == 0 {
                break;
            }

            if candidate_pairs.len() == 1 {
                let (a, b) = candidate_pairs.pop().unwrap();
                fin.push(a);
                fin.push(b);
                break;
            }
            let rights = candidate_pairs.iter().map(|(_, b)| *b).collect::<HashSet<usize>>();
            let lefts = candidate_pairs.iter().map(|(a, _)| *a).collect::<HashSet<usize>>();
            let to_remove = lefts.difference(&rights).map(|n| *n).max().unwrap();
            fin.push(to_remove);
            candidate_pairs = candidate_pairs.clone().iter().filter(|(a, _)| *a != to_remove).map(|a| a.clone()).collect();
        }
        // println!("NL: {:?}", fin);
        count += fin[fin.len() / 2];
    }


    println!("Part 2: {}", count);
}
