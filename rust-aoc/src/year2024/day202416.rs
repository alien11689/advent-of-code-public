use crate::helper::{read_file_lines, Dir, Point2D};
use crate::Day;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::ops::Add;

const YEAR: u16 = 2024;
const DAY: u8 = 16;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

#[derive(Eq, PartialEq)]
struct Position {
    p: Point2D,
    d: Dir,
    points: i32,
    path: HashSet<Point2D>,
    fitness: i32,
}

impl PartialOrd<Self> for Position {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> Ordering {
        let fitness_cmp = other.fitness.cmp(&self.fitness);
        if fitness_cmp == Ordering::Equal {
            other.points.cmp(&self.points)
        } else {
            fitness_cmp
        }
    }
}

impl Position {
    pub fn new(p: Point2D, d: Dir, points: i32, path: HashSet<Point2D>, fitness: i32) -> Self {
        Self {
            p,
            d,
            points,
            path,
            fitness,
        }
    }
}

fn solve_part1_and_2(lines: &[String]) -> (i32, usize) {
    let mut start = Point2D::new(0, 0);
    let mut end = Point2D::new(0, 0);
    let mut blocks = HashSet::new();
    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                blocks.insert(Point2D::new(x as i32, y as i32));
            } else if c == 'S' {
                start = Point2D::new(x as i32, y as i32);
            } else if c == 'E' {
                end = Point2D::new(x as i32, y as i32);
            }
        }
    }
    let mut mem: HashMap<(Point2D, Dir), i32> = HashMap::new();
    let mut pq: BinaryHeap<Position> = BinaryHeap::new();
    [(Dir::E, 0), (Dir::N, 1000), (Dir::S, 1000), (Dir::W, 2000)]
        .iter()
        .for_each(|(d, points)| {
            let mut path = HashSet::new();
            path.insert(start);
            pq.push(Position::new(
                start,
                *d,
                *points,
                path,
                start.manhattan(&end),
            ));
        });

    let mut best_points = i32::MAX;
    let mut best_paths: HashSet<Point2D> = HashSet::new();
    while let Some(cur) = pq.pop() {
        // println!("Iterating over q of size {} points={} fitness={}, best_points={}", pq.len(), cur.points, cur.fitness, best_points);
        let key = (cur.p, cur.d);
        if mem.get(&key).unwrap_or(&i32::MAX) < &cur.points {
            continue;
        }
        if cur.points > best_points {
            continue;
        }
        mem.insert(key, cur.points);
        let mut p = cur.p;
        let mut new_points = cur.points;
        let mut local_visited: HashSet<Point2D> = HashSet::new();
        loop {
            let np = p.add(&cur.d);
            if blocks.contains(&np) {
                break;
            }
            new_points += 1;
            local_visited.insert(np);
            if end.eq(&np) {
                match new_points.cmp(&best_points) {
                    Ordering::Less => {
                        best_points = new_points;
                        best_paths = cur.path.clone();
                        best_paths.extend(local_visited.iter());
                    }
                    Ordering::Equal => {
                        best_paths.extend(cur.path.iter());
                        best_paths.extend(local_visited.iter());
                    }
                    Ordering::Greater => {}
                }
                break;
            }
            let left = cur.d.left();
            let right = cur.d.right();
            pq.push(Position::new(
                np,
                left,
                new_points + 1000,
                merge(&cur.path, &local_visited),
                np.manhattan(&end),
            ));
            pq.push(Position::new(
                np,
                right,
                new_points + 1000,
                merge(&cur.path, &local_visited),
                np.manhattan(&end),
            ));
            p = np;
        }
    }

    (best_points, best_paths.len())
}

fn merge(s1: &HashSet<Point2D>, s2: &HashSet<Point2D>) -> HashSet<Point2D> {
    let mut res = s1.clone();
    res.extend(s2.iter());
    res
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
        assert_eq!(solve_part1_and_2(&lines).0, 7036);
    }

    #[test]
    fn should_part1_pass_test_input2() {
        let lines = read_file_lines(YEAR, DAY, "test2.txt");
        assert_eq!(solve_part1_and_2(&lines).0, 11048);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1_and_2(&lines).1, 45);
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines(YEAR, DAY, "test2.txt");
        assert_eq!(solve_part1_and_2(&lines).1, 64);
    }
}
