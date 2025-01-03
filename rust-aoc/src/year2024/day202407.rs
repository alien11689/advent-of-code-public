use crate::helper::read_file_lines;
use crate::Day;
use rayon::iter::ParallelBridge;
use rayon::prelude::ParallelIterator;
use std::collections::VecDeque;

const YEAR: u16 = 2024;
const DAY: u8 = 7;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1(lines: &[String]) -> i64 {
    lines
        .iter()
        .par_bridge()
        .filter_map(|line| {
            let parts: Vec<&str> = line.split(": ").collect();
            let target = parts[0].parse::<i64>().unwrap();
            let options: Vec<i64> = parts[1]
                .split(" ")
                .map(|p| p.parse::<i64>().unwrap())
                .collect();
            let start = (options[0], 1);
            let mut deque = VecDeque::new();
            deque.push_back(start);
            while let Some((cur, idx)) = deque.pop_front() {
                if idx == options.len() {
                    if target == cur {
                        return Some(target);
                    }
                } else {
                    deque.push_back((cur + options[idx], idx + 1));
                    deque.push_back((cur * options[idx], idx + 1));
                }
            }
            None
        })
        .sum()
}

fn solve_part2(lines: &[String]) -> i64 {
    lines
        .iter()
        .par_bridge()
        .filter_map(|line| {
            let parts: Vec<&str> = line.split(": ").collect();
            let target = parts[0].parse::<i64>().unwrap();
            let options: Vec<i64> = parts[1]
                .split(" ")
                .map(|p| p.parse::<i64>().unwrap())
                .collect();
            let start = (options[0], 1);
            let mut deque = VecDeque::new();
            deque.push_back(start);
            while let Some((cur, idx)) = deque.pop_front() {
                if idx == options.len() {
                    if target == cur {
                        return Some(target);
                    }
                } else {
                    deque.push_back((cur + options[idx], idx + 1));
                    deque.push_back((cur * options[idx], idx + 1));
                    deque.push_back((format!("{}{}", cur, options[idx]).parse().unwrap(), idx + 1));
                }
            }
            None
        })
        .sum()
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
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1(&lines), 3749);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part2(&lines), 11387);
    }
}
