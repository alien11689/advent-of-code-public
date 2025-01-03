use crate::helper::{read_file_lines, Point2D};
use crate::Day;
use std::collections::{HashMap, HashSet};

const YEAR: u16 = 2024;
const DAY: u8 = 8;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1_and_2(lines: &[String]) -> (usize, usize) {
    let mut map = HashMap::new();
    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c != '.' {
                let p = Point2D::new(x as i32, y as i32);
                let v = map.entry(c).or_insert(Vec::with_capacity(2));
                v.push(p);
            }
        }
    }
    let left_up = Point2D::new(0, 0);
    let right_down = Point2D::new(lines[0].len() as i32 - 1, lines.len() as i32 - 1);

    let mut part1 = HashSet::new();
    let mut part2 = HashSet::new();
    map.iter().for_each(|(_, antenas)| {
        for (i, a1) in antenas.iter().enumerate() {
            for a2 in antenas.iter().skip(i) {
                if a1 == a2 {
                    continue;
                }
                let dx = a2.x - a1.x;
                let dy = a2.y - a1.y;
                // part 1
                let p1 = Point2D::new(a1.x - dx, a1.y - dy);
                let p2 = Point2D::new(a2.x + dx, a2.y + dy);
                if p1.in_range(left_up, right_down) {
                    part1.insert(p1);
                }
                if p2.in_range(left_up, right_down) {
                    part1.insert(p2);
                }
                // part2
                let mut a1 = *a1;
                let mut a2 = *a2;
                loop {
                    if a1.in_range(left_up, right_down) {
                        part2.insert(a1);
                        a1 = Point2D::new(a1.x - dx, a1.y - dy);
                    } else {
                        break;
                    }
                }
                loop {
                    if a2.in_range(left_up, right_down) {
                        part2.insert(a2);
                        a2 = Point2D::new(a2.x + dx, a2.y + dy);
                    } else {
                        break;
                    }
                }
            }
        }
    });

    (part1.len(), part2.len())
}

#[cfg(not(tarpaulin_include))]
fn main() {
    let lines = read_file_lines(YEAR, DAY, "input.txt");
    let (part1, part2) = solve_part1_and_2(&lines);
    println!("{part1}");
    println!("{part2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1_and_2(&lines).0, 14);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1_and_2(&lines).1, 34);
    }
}
