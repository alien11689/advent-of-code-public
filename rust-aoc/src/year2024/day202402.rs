use std::vec::Vec;

use crate::helper::read_file_lines;

fn solve_part1(lines: &Vec<String>) -> i32 {
    0 // TODO
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
        assert_eq!(solve_part1(&lines), 0);
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines("./resources/2024/02/test1.txt");
        assert_eq!(solve_part2(&lines), 0);
    }
}
