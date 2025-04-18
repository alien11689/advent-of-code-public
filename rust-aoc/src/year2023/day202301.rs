use crate::helper::{read_file_lines, reverse_string};
use crate::Day;
use std::vec::Vec;

const YEAR: u16 = 2023;
const DAY: u8 = 1;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1(lines: &Vec<String>) -> u32 {
    let mut sum = 0;
    for line in lines {
        let digits = line
            .chars()
            .filter(|c| c.is_ascii_digit())
            .flat_map(|c| c.to_digit(10))
            .collect::<Vec<_>>();
        let first_digit = digits.first().expect("there must be first digit");
        let last_digit = digits.last().expect("there must be first digit");
        sum += first_digit * 10 + last_digit;
    }
    sum
}

fn solve_part2(lines: &Vec<String>) -> u32 {
    let words = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];
    let reversed_words = words
        .iter()
        .map(|w| reverse_string(&(String::from(*w))))
        .collect::<Vec<_>>();
    let numbers = ["1", "2", "3", "4", "5", "6", "7", "8", "9"];

    let mut sum = 0;
    for line in lines {
        let mut first_idx: usize = line.len();
        let mut first_num: u32 = 0;
        let mut last_idx: usize = line.len();
        let mut last_num: u32 = 0;
        for (idx, number) in numbers.iter().enumerate() {
            if let Some(x) = line.find(number) {
                if x < first_idx {
                    first_idx = x;
                    first_num = u32::try_from(idx + 1).expect("should success");
                }
            }
            if let Some(x) = reverse_string(line).find(number) {
                if x < last_idx {
                    last_idx = x;
                    last_num = u32::try_from(idx + 1).expect("should success");
                }
            }
        }
        for (idx, number) in words.iter().enumerate() {
            if let Some(x) = line.find(number) {
                if x < first_idx {
                    first_idx = x;
                    first_num = u32::try_from(idx + 1).expect("should success");
                }
            }
        }
        let reversed_line = reverse_string(line);
        for (idx, number) in reversed_words.iter().enumerate() {
            if let Some(x) = reversed_line.find(number) {
                if x < last_idx {
                    last_idx = x;
                    last_num = u32::try_from(idx + 1).expect("should success");
                }
            }
        }
        sum += first_num * 10 + last_num;
    }
    sum
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
        assert_eq!(solve_part1(&lines), 142);
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines(YEAR, DAY, "test2.txt");
        assert_eq!(solve_part2(&lines), 281);
    }
}
