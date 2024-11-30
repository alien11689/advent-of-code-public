use std::vec::Vec;

use crate::helper::{read_file_lines, reverse_string};

fn solve_part1(lines: &Vec<String>) -> u32 {
    todo!()
}

fn solve_part2(lines: &Vec<String>) -> u32 {
    todo!()
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/01/input.txt");
    let lines = read_file_lines(&full_path);
    println!("Day 01");
    println!("Part 1");
    println!("{}", solve_part1(&lines));
    println!("Part 2");
    println!("{}", solve_part2(&lines));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines("./resources/2024/01/test1.txt");
        assert_eq!(solve_part1(&lines), 142);
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines("./resources/2024/01/test2.txt");
        assert_eq!(solve_part2(&lines), 281);
    }
}
