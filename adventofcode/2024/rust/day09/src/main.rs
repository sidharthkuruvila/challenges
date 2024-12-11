use std::cmp::min;
use std::fs::File;
use std::io::Read;
use std::path::Path;

fn main() {
    let contents = read_file("../../input/day09/input.txt");

    assert_eq!(contents.len() % 2, 1);
    let mut bytes = contents
        .into_bytes()
        .iter()
        .map(|b| (*b as char).to_string().parse::<usize>().unwrap())
        .collect::<Vec<_>>();
}

fn get_checksum(index: usize, block_id: usize, range: usize) -> usize {
    let mut sum = 0;
    for i in index .. index + range {
        sum = sum + block_id * i
    }
    println!("index: {}, block_id: {}, range: {}, sum: {}", index, block_id, range, sum);
    // block_id * (index * ((range - 1) + range * (range + 1) / 2))
    sum
}


fn part2(nums: Vec<usize>) {
    let mut new_nums: Vec<(usize, usize, usize)> = Vec::new();
    let mut gaps: Vec<(usize, usize)> = Vec::new();
    let mut index = 0;
    for i in 0..new_nums.len()/2 {
        let width = nums[i*2];
        new_nums.push((index, width, i));
        index+=width;
        if nums.len() < i*2 + 1 {
            let gap = nums[i*2 + 1];
            gaps.push((index, gap));
            index+=gap;
        }
    }
    gaps.reverse();
    let mut front_index = 0;
    let mut back_index = new_nums.len() - 1;
    let mut gap_index = 0;

    let fin: Vec<(usize, usize, usize)> = Vec::new();

    let front = new_nums[front_index];
    let back = new_nums[back_index];
    while front_index < back_index {


    }

    let indexes = indexes(&nums);
    let checksum = 0;
    for i in 0..&nums.len()/2 {
        let front_index = i * 2;
        checksum += get_checksum(indexes.get(i*2))
    }
    let mut front_index = 0;
    let mut back_index = nums.len() - 1;
    let mut new_nums:Vec<(usize, usize, usize)> = vec![];
    let mut index: usize = 0;
    loop {
        if front_index >= back_index {
            break;
        }
        let front_block_id = front_index / 2;
        let front = nums[front_index];
        new_nums.push((index, front_block_id, front));
        loop {
            let back = nums[back_index];
            let back_block_id = back_index / 2;
            let gap = nums[front_index+1];
            let diff = min(gap, back);

        }



        let checksum = get_checksum(front_index, front_block_id, nums[front_index]);
        let front_gap = nums[front_index + 1];
        let back_block_id = back_index / 2;

        loop {
            let front = nums[front_index];

        }
    }

}

fn indexes(nums: &Vec<usize>) -> Vec<usize> {
    let mut new_nums = vec![];
    let mut sum = 0;
    for &i in nums {
        new_nums.push(sum);
        sum += i;
    }
    new_nums
}
fn read_file(filename: &str) -> String {
    let path = Path::new(filename);
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}


// 6354982853268 is too high
// 6354982853268
// 6354717832270
// 6354717832270