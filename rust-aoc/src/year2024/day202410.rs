use crate::helper::{read_file_lines, Point2D};
use std::collections::{HashMap, HashSet, VecDeque};

const DAY: u8 = 10;

#[derive(Eq, PartialEq, Hash, Clone)]
struct Position {
    loc: Point2D,
    num: u32,
    start: Point2D,
}

impl Position {
    pub fn new(loc: Point2D, num: u32, start: Point2D) -> Self {
        Self { loc, num, start }
    }
}

fn solve_part1_and_2(lines: &[String]) -> (usize, usize) {
    let left_up = Point2D::new(0, 0);
    let right_down = Point2D::new(lines[0].len() as i32 - 1, lines.len() as i32 - 1);
    let mut map = HashMap::new();
    let mut q = VecDeque::new();
    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let digit = c.to_digit(10).unwrap();
            let p = Point2D::new(x as i32, y as i32);
            map.insert(p, digit);
            if digit == 0 {
                q.push_back(Position::new(p, digit, p));
            }
        }
    }
    let mut part1 = HashSet::new();
    let mut part2 = Vec::new();
    while let Some(position) = q.pop_front() {
        if position.num == 9 {
            part1.insert(position.clone());
            part2.push(position.clone());
            continue;
        }
        let next_num = position.num + 1;
        position
            .loc
            .neighbours_cross()
            .iter()
            .filter(|n| n.in_range(left_up, right_down) && map.get(n) == Some(&(next_num)))
            .for_each(|n| q.push_back(Position::new(*n, next_num, position.start)));
    }
    (part1.len(), part2.len())
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/{:0>2}/input.txt", DAY);
    let lines = read_file_lines(&full_path);
    println!("Day{:0>2}", DAY);
    let (part1, part2) = solve_part1_and_2(&lines);
    println!("{part1}");
    println!("{part2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part1_and_2(&lines).0, 36);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part1_and_2(&lines).1, 81);
    }
}
