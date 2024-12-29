use crate::helper::{clusters_full, read_file_lines, Point2D};
use regex::Regex;
use std::collections::HashSet;

const DAY: u8 = 14;

struct Robot {
    x: i32,
    y: i32,
    dx: i32,
    dy: i32,
}

impl Robot {
    fn to_point2d(&self) -> Point2D {
        Point2D::new(self.x, self.y)
    }

    fn iterate(&self, iter: i32, x_limit: i32, y_limit: i32) -> Point2D {
        Point2D::new(
            ((self.x + self.dx * iter) % x_limit + x_limit) % x_limit,
            ((self.y + self.dy * iter) % y_limit + y_limit) % y_limit,
        )
    }

    fn iterate_once(&self, x_limit: i32, y_limit: i32) -> Robot {
        Robot::new(
            (self.x + self.dx + x_limit) % x_limit,
            (self.y + self.dy + y_limit) % y_limit,
            self.dx,
            self.dy,
        )
    }
}

impl Robot {
    pub fn new(x: i32, y: i32, dx: i32, dy: i32) -> Self {
        Self { x, y, dx, dy }
    }
}

fn solve_part1(lines: &[String], x_size: i32, y_size: i32) -> i32 {
    let robots = read_robots(lines);
    let points: Vec<Point2D> = robots
        .iter()
        .map(|r| r.iterate(100, x_size, y_size))
        .collect();
    let half_x = x_size / 2;
    let half_y = y_size / 2;
    let q1 = points
        .iter()
        .filter(|p| p.in_range(Point2D::new(0, 0), Point2D::new(half_x - 1, half_y - 1)))
        .count() as i32;
    let q2 = points
        .iter()
        .filter(|p| {
            p.in_range(
                Point2D::new(half_x + 1, 0),
                Point2D::new(x_size - 1, half_y - 1),
            )
        })
        .count() as i32;
    let q3 = points
        .iter()
        .filter(|p| {
            p.in_range(
                Point2D::new(0, half_y + 1),
                Point2D::new(half_x - 1, y_size - 1),
            )
        })
        .count() as i32;
    let q4 = points
        .iter()
        .filter(|p| {
            p.in_range(
                Point2D::new(half_x + 1, half_y + 1),
                Point2D::new(x_size - 1, y_size - 1),
            )
        })
        .count() as i32;
    q1 * q2 * q3 * q4
}

fn read_robots(lines: &[String]) -> Vec<Robot> {
    let regex = Regex::new(r"[=, ]").unwrap();
    let robots: Vec<Robot> = lines
        .iter()
        .map(|line| {
            let parts: Vec<&str> = regex.split(line).collect();
            Robot::new(
                parts[1].parse().unwrap(),
                parts[2].parse().unwrap(),
                parts[4].parse().unwrap(),
                parts[5].parse().unwrap(),
            )
        })
        .collect();
    robots
}

fn solve_part2(lines: &[String], x_size: i32, y_size: i32) -> usize {
    let mut robots = read_robots(lines);
    let mut count = 0;
    loop {
        count += 1;
        robots = robots
            .iter()
            .map(|r| r.iterate_once(x_size, y_size))
            .collect();
        let points: HashSet<Point2D> = robots.iter().map(|r| r.to_point2d()).collect();
        let max = clusters_full(&points)
            .iter()
            .map(|c| c.len())
            .max()
            .unwrap();
        if max >= robots.len() / 4 {
            break;
        }
    }
    count
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/{:0>2}/input.txt", DAY);
    let lines = read_file_lines(&full_path);
    println!("Day{:0>2}", DAY);
    println!("{}", solve_part1(&lines, 101, 103));
    println!("{}", solve_part2(&lines, 101, 103));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part1(&lines, 11, 7), 12);
    }
}
