use crate::helper::read_file_lines;
use regex::Regex;

fn solve_part1(lines: &[String]) -> i32 {
    0
}

fn solve_part2(lines: &[String]) -> i32 {
    0
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/04/input.txt");
    let lines = read_file_lines(&full_path);
    println!("Day04");
    println!("{}", solve_part1(&lines));
    println!("{}", solve_part2(&lines));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines("./resources/2024/04/test1.txt");
        assert_eq!(solve_part1(&lines), 0);
    }

    #[ignore]
    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines("./resources/2024/04/test1.txt");
        assert_eq!(solve_part2(&lines), 0);
    }
}
