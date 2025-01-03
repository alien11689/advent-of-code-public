use crate::helper::read_file_lines;
use crate::Day;
use std::collections::{HashMap, HashSet};
use std::vec::Vec;

const YEAR: u16 = 2023;
const DAY: u8 = 4;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn count_matching(line: &str) -> u32 {
    let rest = line.split(": ").last().unwrap();
    let own_and_winning: Vec<&str> = rest.split(" | ").collect();
    let own: HashSet<&str> = own_and_winning[0].split_whitespace().collect();
    let winning: HashSet<&str> = own_and_winning[1].split_whitespace().collect();
    let count = own.intersection(&winning).count() as u32;
    count
}

fn solve_part1(lines: &[String]) -> u32 {
    let mut score: u32 = 0;
    for line in lines {
        let count = count_matching(line);
        if count > 0 {
            score += 2_u32.pow(count - 1);
        }
    }
    score
}

fn solve_part2(lines: &[String]) -> u32 {
    let mut counts: HashMap<usize, u32> = HashMap::new();
    let max = lines.len();
    for i in 0..max {
        counts.insert(i, 1);
    }
    for (i, line) in lines.iter().enumerate() {
        let count = count_matching(line) as usize;
        let cur_amount = *counts.get(&i).unwrap();
        for ii in 0..count {
            let to_update = i + ii + 1;
            if to_update >= max {
                break;
            }
            counts.entry(to_update).and_modify(|cur| *cur += cur_amount);
        }
    }
    counts.values().sum()
}

#[cfg(not(tarpaulin_include))]
fn main() {
    let lines = read_file_lines(YEAR, DAY, "input.txt");
    println!("{}", solve_part1(&lines));
    println!("{}", solve_part2(&lines));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1(&lines), 13);
    }

    #[test]
    fn should_part2_pass_test() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part2(&lines), 30);
    }
}
