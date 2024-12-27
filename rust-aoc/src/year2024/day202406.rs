use crate::helper::{read_file_lines, Dir, Point2D};
use rayon::iter::ParallelBridge;
use rayon::prelude::ParallelIterator;
use std::collections::HashSet;

const DAY: u8 = 6;

fn solve_part1(lines: &[String]) -> usize {
    let (blocks, start, max) = read_input(lines);
    let visited = iterate(&blocks, &start, &max);
    visited.len()
}

fn iterate(blocks: &HashSet<Point2D>, start: &Point2D, max: &Point2D) -> HashSet<Point2D> {
    let mut cur = *start;
    let mut dir = Dir::N;
    let mut visited_v2: HashSet<(Point2D, Dir)> = HashSet::new();
    while cur.x >= 0 && cur.y >= 0 && cur.x < max.x && cur.y < max.y {
        let key = (cur, dir.clone());
        if visited_v2.contains(&key) {
            return HashSet::new();
        }
        visited_v2.insert(key);
        let next = cur + &dir;
        if blocks.contains(&next) {
            dir = dir.right()
        } else {
            cur = next;
        }
    }
    visited_v2.iter().map(|(p, _)| *p).collect()
}

fn read_input(lines: &[String]) -> (HashSet<Point2D>, Point2D, Point2D) {
    let mut blocks = HashSet::new();
    let mut start = Point2D::new(0, 0);
    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '^' {
                start = Point2D::new(x as i32, y as i32);
            }
            if c == '#' {
                blocks.insert(Point2D::new(x as i32, y as i32));
            }
        }
    }
    let max = Point2D::new(lines[0].len() as i32, lines.len() as i32);
    (blocks, start, max)
}

fn solve_part2(lines: &[String]) -> usize {
    let (blocks, start, max) = read_input(lines);
    let visited = iterate(&blocks, &start, &max);
    visited
        .iter()
        .par_bridge()
        .filter(|obstacle| {
            let mut new_blocks = blocks.clone();
            new_blocks.insert(**obstacle);
            iterate(&new_blocks, &start, &max).is_empty()
        })
        .count()
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/{:0>2}/input.txt", DAY);
    let lines = read_file_lines(&full_path);
    println!("Day{:0>2}", DAY);
    println!("{}", solve_part1(&lines));
    println!("{}", solve_part2(&lines));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part1(&lines), 41);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part2(&lines), 6);
    }
}
