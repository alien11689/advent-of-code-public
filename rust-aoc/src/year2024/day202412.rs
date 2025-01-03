use crate::helper::{clusters_cross, read_file_lines, read_map, Dir, Point2D};
use crate::Day;
use std::collections::{HashMap, HashSet, VecDeque};

const YEAR: u16 = 2024;
const DAY: u8 = 12;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1_and_2(lines: &[String]) -> (usize, usize) {
    let map = read_map(lines);
    let mut map_duplicated = map.clone();
    let mut part1 = 0;
    let mut part2 = 0;
    while let Some(entry) = map_duplicated.iter().next() {
        let points = find_single_region(&map, entry.0, entry.1);
        points.iter().for_each(|p| {
            map_duplicated.remove(p);
        });
        let area = points.len();
        // part 1
        let perimeter = points
            .iter()
            .flat_map(|p| p.neighbours_cross())
            .filter(|p| !points.contains(p))
            .count();
        part1 += area * perimeter;
        // part2
        let sides: usize = [Dir::E, Dir::N, Dir::W, Dir::S]
            .iter()
            .map(|d| {
                let border: HashSet<Point2D> = points
                    .iter()
                    .map(|p| *p + d)
                    .filter(|p| !points.contains(p))
                    .collect();
                clusters_cross(&border).len()
            })
            .sum();
        part2 += area * sides;
    }
    (part1, part2)
}

fn find_single_region(map: &HashMap<Point2D, char>, point: &Point2D, c: &char) -> HashSet<Point2D> {
    let mut region = HashSet::new();
    let mut q = VecDeque::new();
    q.push_back(*point);
    while let Some(cur) = q.pop_front() {
        if region.contains(&cur) {
            continue;
        }
        cur.neighbours_cross()
            .iter()
            .filter(|p| map.get(*p) == Some(c) && !region.contains(*p))
            .for_each(|p| q.push_back(*p));
        region.insert(cur);
    }
    region
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
        assert_eq!(solve_part1_and_2(&lines).0, 140);
    }

    #[test]
    fn should_part1_pass_test_input2() {
        let lines = read_file_lines(YEAR, DAY, "test2.txt");
        assert_eq!(solve_part1_and_2(&lines).0, 772);
    }

    #[test]
    fn should_part1_pass_test_input3() {
        let lines = read_file_lines(YEAR, DAY, "test3.txt");
        assert_eq!(solve_part1_and_2(&lines).0, 1930);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1_and_2(&lines).1, 80);
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines(YEAR, DAY, "test2.txt");
        assert_eq!(solve_part1_and_2(&lines).1, 436);
    }

    #[test]
    fn should_part2_pass_test_input3() {
        let lines = read_file_lines(YEAR, DAY, "test3.txt");
        assert_eq!(solve_part1_and_2(&lines).1, 1206);
    }

    #[test]
    fn should_part2_pass_test_input4() {
        let lines = read_file_lines(YEAR, DAY, "test4.txt");
        assert_eq!(solve_part1_and_2(&lines).1, 236);
    }

    #[test]
    fn should_part2_pass_test_input5() {
        let lines = read_file_lines(YEAR, DAY, "test5.txt");
        assert_eq!(solve_part1_and_2(&lines).1, 368);
    }
}
