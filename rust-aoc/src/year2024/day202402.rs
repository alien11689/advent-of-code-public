use crate::helper::read_file_lines;
use regex::Regex;
use std::collections::HashSet;
use std::vec::Vec;

fn solve_part1(lines: &Vec<String>) -> i32 {
    let regex = Regex::new(r" +").unwrap();
    let numbers: Vec<Vec<i32>> = lines
        .iter()
        .map(|line| {
            regex
                .split(line)
                .map(|n| n.parse::<i32>().expect("valid number"))
                .collect::<Vec<i32>>()
        })
        .collect();
    let mut count = 0;
    for set in numbers {
        // println!("{:?}", set);
        if is_safe(set) {
            count += 1;
        }
    }
    count
}

fn is_safe(vec: Vec<i32>) -> bool {
    let mut diffs: Vec<i32> = Vec::new();
    let mut prev: Option<i32> = None;
    for cur in vec {
        match prev {
            None => {
                prev = Some(cur);
            }
            Some(p) => {
                diffs.push(p - cur);
                prev = Some(cur);
            }
        }
    }
    // println!("{:?}", diffs);
    let mut valid: HashSet<i32> = HashSet::new();
    valid.insert(1);
    valid.insert(2);
    valid.insert(3);
    let correct = (diffs.iter().all(|i| *i > 0) || diffs.iter().all(|i| *i < 0))
        && diffs.iter().map(|i| i.abs()).all(|i| valid.contains(&i));
    // println!("{:?}", correct);
    correct
}

fn solve_part2(lines: &Vec<String>) -> i32 {
    0 // TODO
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/02/input.txt");
    let lines = read_file_lines(&full_path);
    println!("Day02");
    println!("{}", solve_part1(&lines));
    println!("{}", solve_part2(&lines));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines("./resources/2024/02/test1.txt");
        assert_eq!(solve_part1(&lines), 2);
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines("./resources/2024/02/test1.txt");
        assert_eq!(solve_part2(&lines), 4);
    }
}
