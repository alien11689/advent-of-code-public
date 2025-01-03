use crate::helper::{read_file_lines, Point2D};
use crate::Day;
use std::collections::HashSet;

const YEAR: u16 = 2024;
const DAY: u8 = 25;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1(lines: &[String]) -> usize {
    let mut y = 0;
    let mut lock = false;
    let mut cur_block = HashSet::new();
    let mut keys = HashSet::new();
    let mut locks = HashSet::new();
    for line in lines {
        if line.is_empty() {
            let representation = heights(&cur_block);
            if lock {
                locks.insert(representation);
            } else {
                keys.insert(representation);
            }
            y = 0;
            lock = false;
            cur_block.clear();
        } else {
            for (x, c) in line.chars().enumerate() {
                if y == 0 && x == 0 && c == '#' {
                    lock = true;
                }
                if c == '#' {
                    cur_block.insert(Point2D::new(x as i32, y));
                }
            }
            y += 1;
        }
    }
    if y > 0 {
        let representation = heights(&cur_block);
        if lock {
            locks.insert(representation);
        } else {
            keys.insert(representation);
        }
    }
    let mut count = 0;
    for k in keys {
        for l in &locks {
            let mut fixes = true;
            for c in 0..5 {
                if k[c] + l[c] > 5 {
                    fixes = false;
                    break;
                }
            }
            if fixes {
                count += 1;
            }
        }
    }
    count
}

fn heights(block: &HashSet<Point2D>) -> Vec<usize> {
    (0..5)
        .map(|c| block.iter().filter(|p| p.x == c).count() - 1)
        .collect()
}

#[cfg(not(tarpaulin_include))]
fn main() {
    let lines = read_file_lines(YEAR, DAY, "input.txt");
    println!("{}", solve_part1(&lines));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1(&lines), 3);
    }
}
