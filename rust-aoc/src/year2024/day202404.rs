use crate::helper::{read_file_lines, read_map, Point2D};
use crate::Day;
use std::collections::HashMap;

const YEAR: u16 = 2024;
const DAY: u8 = 4;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1(lines: &[String]) -> i32 {
    let map = read_map(lines);
    let xs: Vec<&Point2D> = map
        .iter()
        .filter(|(_, c)| **c == 'X')
        .map(|(p, _)| p)
        .collect();
    xs.iter().map(|p| count_xmas_starting_from(p, &map)).sum()
}

fn count_xmas_starting_from(p: &Point2D, map: &HashMap<Point2D, char>) -> i32 {
    let mut count = 0;
    for possible_m in p.neighbours() {
        if let Some('M') = map.get(&possible_m) {
            let dx = possible_m.x - p.x;
            let dy = possible_m.y - p.y;
            if let Some('A') = map.get(&possible_m.mv(dx, dy)) {
                if let Some('S') = map.get(&possible_m.mv(dx * 2, dy * 2)) {
                    count += 1;
                }
            }
        }
    }
    count
}

fn solve_part2(lines: &[String]) -> i32 {
    let map = read_map(lines);
    let mut count = 0;
    for (y, line) in lines.iter().take(lines.len() - 2).enumerate() {
        for (x, c) in line.chars().take(line.len() - 2).enumerate() {
            let x = x as i32;
            let y = y as i32;
            if map[&Point2D::new(x + 1, y + 1)] == 'A' {
                let left_up = c;
                let left_down = map[&Point2D::new(x, y + 2)];
                let right_up = map[&Point2D::new(x + 2, y)];
                let right_down = map[&Point2D::new(x + 2, y + 2)];
                if left_up == 'M' && right_up == 'M' && left_down == 'S' && right_down == 'S' {
                    count += 1
                }
                if left_up == 'M' && left_down == 'M' && right_up == 'S' && right_down == 'S' {
                    count += 1
                }
                if left_up == 'S' && right_up == 'S' && left_down == 'M' && right_down == 'M' {
                    count += 1
                }
                if left_up == 'S' && left_down == 'S' && right_up == 'M' && right_down == 'M' {
                    count += 1
                }
            }
        }
    }
    count
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
        assert_eq!(solve_part1(&lines), 18);
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part2(&lines), 9);
    }
}
