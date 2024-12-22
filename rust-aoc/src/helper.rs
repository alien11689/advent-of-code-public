use std::collections::HashSet;
use std::fs;
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

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
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
        let mut neighbours = HashSet::new();
        neighbours.insert(Point2D {
            x: self.x + 1,
            y: self.y,
        });
        neighbours.insert(Point2D {
            x: self.x + 1,
            y: self.y - 1,
        });
        neighbours.insert(Point2D {
            x: self.x + 1,
            y: self.y + 1,
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
}

pub(crate) fn measure_time<F>(f: F)
where
    F: Fn() -> (),
{
    let start = Instant::now();
    f();
    let duration = start.elapsed();
    println!("Time: {:?}", duration);
}
