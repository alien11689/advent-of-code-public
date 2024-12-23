use crate::helper::read_file_lines;
use rayon::iter::ParallelBridge;
use rayon::prelude::ParallelIterator;
use std::collections::VecDeque;

const DAY: u8 = 7;

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
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/{:0>2}/input.txt", DAY);
    let lines = read_file_lines(&full_path);
    println!("Day{:0>2}", DAY);
    println!("{}", solve_part1(&lines));
    println!("{}", solve_part2(&lines));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part1(&lines), 3749);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part2(&lines), 11387);
    }
}
