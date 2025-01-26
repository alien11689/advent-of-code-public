use crate::helper::{read_file_lines, Point2D};
use crate::year2024::day202418::Position::Nil;
use crate::Day;
use std::collections::{HashSet, VecDeque};

const YEAR: u16 = 2024;
const DAY: u8 = 18;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1_and_2(lines: &[String], max: i32, take: usize) -> (i32, String) {
    let start = Point2D::new(0, 0);
    let target = Point2D::new(max, max);
    let mut blocks: HashSet<Point2D> = lines.iter().take(take).map(|s| cords_to_point(s)).collect();
    let part1 = iterate(start, target, &blocks);
    let mut best_path = create_path(&part1);
    let new_blocks: Vec<Point2D> = lines.iter().skip(take).map(|s| cords_to_point(s)).collect();
    for nb in new_blocks {
        blocks.insert(nb);
        if !best_path.contains(&nb) {
            continue;
        }
        // println!("Checking {:?}", nb);
        let result = iterate(start, target, &blocks);
        match result {
            Position::Exist(_, _, _) => {
                best_path = create_path(&result);
            }
            Nil => return (part1.steps(), format!("{},{}", nb.x, nb.y)),
        }
    }
    unreachable!()
}

fn cords_to_point(line: &str) -> Point2D {
    let vec: Vec<i32> = line.split(",").map(|v| v.parse().unwrap()).collect();
    Point2D::new(vec[0], vec[1])
}

fn create_path(position: &Position) -> HashSet<Point2D> {
    let mut best_path = HashSet::new();
    let mut cur = Some(position);
    while let Some(c) = cur {
        match c {
            Position::Exist(p, _, prev) => {
                best_path.insert(*p);
                cur = Some(prev.as_ref());
            }
            Nil => {
                cur = None;
            }
        }
    }
    best_path
}

#[derive(Debug, Clone)]
enum Position {
    Exist(Point2D, i32, Box<Position>),
    Nil,
}

impl Position {
    fn steps(&self) -> i32 {
        match self {
            Position::Exist(_, steps, _) => *steps,
            Nil => -1,
        }
    }
}

fn iterate(start: Point2D, target: Point2D, blocks: &HashSet<Point2D>) -> Position {
    let mut visited = HashSet::new();
    let mut q = VecDeque::new();
    q.push_back(Position::Exist(start, 0, Box::from(Nil)));
    while let Some(cur) = q.pop_front() {
        let (p, steps) = match cur {
            Position::Exist(p, steps, _) => (p, steps),
            Nil => unreachable!(),
        };
        // println!("Checking {:?}", cur);
        if visited.contains(&p) {
            continue;
        }
        visited.insert(p);
        if p == target {
            return cur;
        }
        p.neighbours_cross()
            .iter()
            .filter(|n| !blocks.contains(n) && n.in_range(start, target))
            .for_each(|n| {
                q.push_back(Position::Exist(*n, steps + 1, Box::new(cur.clone())));
            });
    }
    Nil
}

#[cfg(not(tarpaulin_include))]
fn main() {
    let lines = read_file_lines(YEAR, DAY, "input.txt");
    let max = 70;
    let take = 1024;
    let (part1, part2) = solve_part1_and_2(&lines, max, take);
    println!("{part1}");
    println!("{part2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1_and_2(&lines, 6, 12).0, 22);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1_and_2(&lines, 6, 12).1, "6,1");
    }
}
