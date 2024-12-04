use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let path = Path::new("../../input/day04/input.txt");
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    let lines: Vec<Vec<char>> = contents
        .lines()
        .map(|s| {
            s.as_bytes()
                .iter()
                .map(|e| *e as char)
                .collect::<Vec<char>>()
        })
        .collect();

    let mut items: Vec<(usize, usize, char)> = vec![];
    for i in 0..(&lines).len() {
        let line = &lines[i];
        for j in 0..line.len() {
            let c: char = line[j];
            items.push((i, j, c))
        }
    }

    let mut starts = vec![];
    for (i, j, c) in &items {
        if *c == 'X' {
            starts.push((i, j, c));
        }
    }

    let directions: [(i32, i32); 8] = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];

    let moves = starts
        .iter()
        .flat_map(|&(i, j, _)| directions.map(|(di, dj)| (*i, *j, di, dj)))
        .collect::<Vec<(usize, usize, i32, i32)>>();

    fn next_step(
        c: char,
        lines: &Vec<Vec<char>>,
        items: Vec<(usize, usize, i32, i32)>,
    ) -> Vec<(usize, usize, i32, i32)> {
        let mut out = vec![];
        for (i, j, di, dj) in items {
            if let Ok(ni) = usize::try_from(i as i32 + di) {
                if let Ok(nj) = usize::try_from(j as i32 + dj) {
                    if let Some(line) = lines.get(ni) {
                        if let Some(nc) = line.get(nj) {
                            if *nc == c {
                                out.push((ni, nj, di, dj))
                            }
                        }
                    }
                }
            }
        }
        return out;
    }

    let l1 = next_step('M', &lines, moves.clone());
    let l2 = next_step('A', &lines, l1);
    let l3 = next_step('S', &lines, l2);
    let res = l3.len();
    println!("Part 1: {}", res);

    let cross_directions: [[(i32, i32); 4]; 4] = [
        [(-1, -1), (1, 1), (1, -1), (-1, 1)],
        [(1, 1), (-1, -1), (-1, 1), (1, -1)],
        [(-1, -1), (1, 1), (-1, 1), (1, -1)],
        [(1, 1), (-1, -1), (1, -1), (-1, 1)],
    ];

    fn get_char(lines: &Vec<Vec<char>>, (i, j): (i32, i32), (di, dj): (i32, i32)) -> char {
        lines[(i + di) as usize][(j + dj) as usize]
    }

    let mut count = 0;
    for i in 1..(lines.len() - 1) as i32 {
        for j in 1..(lines[0].len() - 1) as i32 {
            let c = lines[i as usize][j as usize];
            if c == 'A' {
                for [a1, a2, b1, b2] in &cross_directions {
                    let m1 = get_char(&lines, (i, j), a1.clone());
                    let s1 = get_char(&lines, (i, j), a2.clone());
                    let m2 = get_char(&lines, (i, j), b1.clone());
                    let s2 = get_char(&lines, (i, j), b2.clone());
                    if m1 == 'M' && m2 == 'M' && s1 == 'S' && s2 == 'S' {
                        count += 1;
                    }
                }
            }
        }
    }
    println!("Part 2: {}", count);
}
