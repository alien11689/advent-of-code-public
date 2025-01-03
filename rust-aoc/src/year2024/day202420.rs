use crate::helper::{read_file_lines, Point2D};
use crate::Day;
use std::collections::{HashMap, HashSet};

const YEAR: u16 = 2024;
const DAY: u8 = 20;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1_and_2(lines: &[String], threshold: i32, limit: i32) -> i32 {
    let mut blocks = HashSet::new();
    let left_up = Point2D::new(0, 0);
    let mut start = left_up;
    let right_down = Point2D::new(lines[0].len() as i32 - 1, lines.len() as i32 - 1);
    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let p = Point2D::new(x as i32, y as i32);
            match c {
                '#' => {
                    blocks.insert(p);
                }
                'S' => {
                    start = p;
                }
                _ => {}
            }
        }
    }
    let mut paths = HashMap::new();
    let mut visited = HashSet::new();
    let mut cur = Some(start);
    let mut steps = 0;
    while let Some(c) = cur {
        // println!("Path {c:?}");
        paths.insert(steps, c);
        visited.insert(c);
        steps += 1;
        cur = None;
        for n in c.neighbours_cross() {
            if n.in_range(left_up, right_down) && !visited.contains(&n) && !blocks.contains(&n) {
                cur = Some(n);
                break;
            }
        }
    }
    let mut count = 0;
    for i in 0..paths.len() {
        let pos = paths.get(&i).unwrap();
        for j in (i + 1)..paths.len() {
            let other = paths.get(&j).unwrap();
            let manhattan = pos.manhattan(other);
            // println!("{pos:?} and {other:?} manhattan is {manhattan}");
            if manhattan > limit {
                continue;
            }
            let speed_up = j as i32 - i as i32 - manhattan;
            // println!("{pos:?} and {other:?} speed up is {speed_up}");
            if speed_up >= threshold {
                count += 1;
            }
        }
    }
    count
}

#[cfg(not(tarpaulin_include))]
fn main() {
    let lines = read_file_lines(YEAR, DAY, "input.txt");
    println!("{}", solve_part1_and_2(&lines, 100, 2));
    println!("{}", solve_part1_and_2(&lines, 100, 20));
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(64, 2, 1)]
    #[case(40, 2, 2)]
    #[case(39, 2, 2)]
    #[case(38, 2, 3)]
    #[case(20, 2, 5)]
    #[case(76, 20, 3)]
    #[case(74, 20, 7)]
    #[case(72, 20, 29)]
    fn should_part1_and_2_pass_test_input1(
        #[case] threshold: i32,
        #[case] limit: i32,
        #[case] res: i32,
    ) {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1_and_2(&lines, threshold, limit), res);
    }
}
