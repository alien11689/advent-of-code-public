use std::collections::HashSet;
use std::fs;
use std::ops::Add;
use std::time::Instant;

fn read_file(file_name: &str) -> String {
    fs::read_to_string(file_name).expect("File should exist")
}

pub(crate) fn read_file_lines(file_name: &str) -> Vec<String> {
    read_file(file_name)
        .trim()
        .lines()
        .map(String::from)
        .collect()
}

pub(crate) fn reverse_string(input: &str) -> String {
    input.chars().rev().collect::<String>()
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub(crate) struct Point2D {
    pub x: i32,
    pub y: i32,
}

impl Point2D {
    pub(crate) fn new(x: i32, y: i32) -> Point2D {
        Point2D { x, y }
    }

    pub(crate) fn mv(&self, dx: i32, dy: i32) -> Point2D {
        Point2D {
            x: self.x + dx,
            y: self.y + dy,
        }
    }

    pub(crate) fn neighbours(&self) -> HashSet<Point2D> {
        let mut neighbours = self.neighbours_cross();
        neighbours.insert(Point2D {
            x: self.x + 1,
            y: self.y - 1,
        });
        neighbours.insert(Point2D {
            x: self.x + 1,
            y: self.y + 1,
        });
        neighbours.insert(Point2D {
            x: self.x - 1,
            y: self.y - 1,
        });
        neighbours.insert(Point2D {
            x: self.x - 1,
            y: self.y + 1,
        });
        neighbours
    }

    pub(crate) fn neighbours_cross(&self) -> HashSet<Point2D> {
        let mut neighbours = HashSet::new();
        neighbours.insert(Point2D {
            x: self.x + 1,
            y: self.y,
        });
        neighbours.insert(Point2D {
            x: self.x,
            y: self.y + 1,
        });
        neighbours.insert(Point2D {
            x: self.x,
            y: self.y - 1,
        });
        neighbours.insert(Point2D {
            x: self.x - 1,
            y: self.y,
        });
        neighbours
    }

    pub(crate) fn in_range(&self, left_up: Point2D, right_down: Point2D) -> bool {
        self.x >= left_up.x
            && self.x <= right_down.x
            && self.y >= left_up.y
            && self.y <= right_down.y
    }

    pub(crate) fn manhattan(&self, other: &Point2D) -> i32 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

impl Add<&Dir> for Point2D {
    type Output = Point2D;

    fn add(self, rhs: &Dir) -> Self::Output {
        match rhs {
            Dir::N => Point2D::new(self.x, self.y - 1),
            Dir::W => Point2D::new(self.x - 1, self.y),
            Dir::S => Point2D::new(self.x, self.y + 1),
            Dir::E => Point2D::new(self.x + 1, self.y),
        }
    }
}

pub(crate) fn measure_time<F>(f: F)
where
    F: Fn(),
{
    let start = Instant::now();
    f();
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub(crate) enum Dir {
    N,
    W,
    S,
    E,
}

impl Dir {
    pub(crate) fn left(&self) -> Dir {
        match self {
            Dir::N => Dir::W,
            Dir::W => Dir::S,
            Dir::S => Dir::E,
            Dir::E => Dir::N,
        }
    }

    pub(crate) fn right(&self) -> Dir {
        match self {
            Dir::N => Dir::E,
            Dir::W => Dir::N,
            Dir::S => Dir::W,
            Dir::E => Dir::S,
        }
    }
}
