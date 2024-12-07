use std::fs::File;
use std::io::Read;
use std::path::Path;

fn read_file(filename: &str) -> String {
    let path = Path::new(filename);
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}

fn parse(content: &String) -> Vec<(i64, Vec<i64>)> {
    let lines = content.split("\n");
    let mut result: Vec<(i64, Vec<i64>)> = Vec::new();
    for line in lines {
        let mut parts = line.split(" ");
        let first_part = parts.next().unwrap();
        let total: i64 = first_part.replace(":", "").parse().unwrap();
        let nums = parts.map(|x| x.parse().unwrap()).collect::<Vec<i64>>();
        result.push((total, nums))
    }
    result
}

fn calibration_matches(total: i64, parts: &Vec<i64>, part_index: usize, acc: i64) -> bool {
    if part_index == parts.len() {
        return total == acc;
    }
    if total < acc {
        return false;
    }

    let nacc = acc * parts[part_index];
    if calibration_matches(total, parts, part_index + 1, nacc) {
        return true;
    }
    let nacc = acc + parts[part_index];
    if calibration_matches(total, parts, part_index + 1, nacc) {
        return true;
    }
    let nacc = append_base10(acc, parts[part_index]);
    if calibration_matches(total, parts, part_index + 1, nacc) {
        return true;
    }
    false
}

fn append_base10(n1: i64, n2: i64) -> i64 {
    n1 * 10_i64.pow(n2.ilog10() + 1) + n2
}

fn main() {
    let contents = read_file("../../input/day07/input.txt");
    let lines = parse(&contents);
    let mut sum = 0;
    'outer: for (total, parts) in lines.clone() {
        for mut i in 0..2 << (parts.len() - 1) {
            let mut ctotal = parts[0];
            for &part in parts[1..].iter() {
                let p = i & 1;
                i = i >> 1;
                if p == 0 {
                    ctotal = ctotal + part
                } else {
                    ctotal = ctotal * part
                }
            }
            if ctotal == total {
                sum += total;
                continue 'outer;
            }
        }
    }
    println!("Part 1: {}", sum);

    let mut sum: i64 = 0;
    for (total, parts) in lines {
        if calibration_matches(total, &parts, 1, parts[0]) {
            sum += total
        }
    }
    println!("Part 2: {}", sum);
}

#[cfg(test)]
mod tests {
    use crate::{append_base10, calibration_matches};

    #[test]
    fn test_append_base10() {
        assert_eq!(append_base10(11, 12), 1112);
        assert_eq!(append_base10(111321, 1232312), 1113211232312);
    }

    #[test]
    fn test_calibration_matches() {
        let v: Vec<i64> = vec![15i64, 6i64];
        assert_eq!(calibration_matches(156, &v, 1, 15), true);
    }
}
