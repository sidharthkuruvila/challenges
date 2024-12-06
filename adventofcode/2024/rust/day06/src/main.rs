use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let contents = read_file("../../input/day06/input.txt")
        .as_bytes()
        .to_vec();
    let bx = get_box(&contents);
    let width = bx.1;
    let height = bx.2;
    let top_index = contents.iter().position(|&c| c == '^' as u8).unwrap();

    let top_position = (top_index % (width + 1), top_index / width - 1);

    let count = walk_path(top_position, &bx).unwrap();
    println!("Part 1: {:?}", count);

    let mut count = 0;
    let mut modifiable_contents = contents.clone();

    for i in 0..width {
        for j in 0..height {
            let idx = get_index(&bx, (i, j));
            if modifiable_contents[idx] != '#' as u8 {
                modifiable_contents[idx] = '#' as u8;
                let mbx = get_box(&modifiable_contents);
                if walk_path(top_position, &mbx) == None {
                    count += 1;
                }
                modifiable_contents[idx] = '.' as u8;
            }
        }
    }

    println!("Part 2, Slow: {:?}", count);

    // Build a teleport map.
    /*
    1. Get the positions of each block

     */
}

fn walk_path(top_position: (usize, usize), bx: &(&[u8], usize, usize)) -> Option<usize> {
    let mut dir: (isize, isize) = (0, -1);
    let mut cp = top_position;
    let mut visited: HashSet<((usize, usize), (isize, isize))> = HashSet::new();

    loop {
        if let Some(np) = add(cp, dir) {
            if let Some(ch) = get(bx, np) {
                if ch == '#' {
                    dir = rotate(dir);
                } else {
                    if visited.contains(&(np, dir)) {
                        return None;
                    }
                    visited.insert((np, dir));
                    cp = np;
                }
            } else {
                break;
            }
        } else {
            break;
        }
    }
    let hs: HashSet<(usize, usize)> = visited.iter().map(|&(p, _)| p).collect();
    Some(hs.len())
}

fn read_file(filename: &str) -> String {
    let path = Path::new(filename);
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}

fn rotate((i, j): (isize, isize)) -> (isize, isize) {
    (j * -1, i)
}

fn add((x, y): (usize, usize), (i, j): (isize, isize)) -> Option<(usize, usize)> {
    if let Some(nx) = x.checked_add_signed(i) {
        if let Some(ny) = y.checked_add_signed(j) {
            return Some((nx, ny));
        }
    }
    None
}

fn get_index((_, _, width): &(&[u8], usize, usize), (x, y): (usize, usize)) -> usize {
    y * (width + 1) + x
}

fn get((contents, height, width): &(&[u8], usize, usize), (x, y): (usize, usize)) -> Option<char> {
    if *width <= x || *height <= y {
        None
    } else {
        let idx = y * (width + 1) + x;
        Some(contents[idx] as char)
    }
}

fn get_box(grid: &Vec<u8>) -> (&[u8], usize, usize) {
    let width = grid.iter().position(|&c| c == '\n' as u8).unwrap();
    let height = grid.len() / (width);
    (grid, width, height)
}
#[cfg(test)]
mod tests {
    use crate::{get, get_box, read_file, rotate};

    #[test]
    fn test_rotate() {
        assert_eq!(rotate((0, -1)), (1, 0));
        assert_eq!(rotate((1, 0)), (0, 1));
        assert_eq!(rotate((0, 1)), (-1, 0));
        assert_eq!(rotate((-1, 0)), (0, -1));
    }

    #[test]
    fn test_get_box() {
        let grid = read_file("../../input/day06/small-input.txt")
            .as_bytes()
            .to_vec();
        let (_, width, height) = get_box(&grid);
        assert_eq!(height, 10);
        assert_eq!(width, 10);
    }

    #[test]
    fn test_get() {
        let grid = "abcd
efgh
ijkl
mnop"
            .as_bytes()
            .to_vec();
        let bx = get_box(&grid);
        let mut ch = 'a';
        for i in 0..4 {
            for j in 0..4 {
                assert_eq!(get(&bx, (j, i)).unwrap(), ch);
                ch = (ch as u8 + 1) as char;
            }
        }
        for i in 4..10 {
            for j in 4..10 {
                assert_eq!(get(&bx, (j, i)), None);
            }
        }
    }
}
