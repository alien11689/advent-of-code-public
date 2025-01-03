use crate::helper::{read_file_lines, Dir, Point2D};
use crate::Day;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};

const YEAR: u16 = 2024;
const DAY: u8 = 21;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct SubPath {
    start: Point2D,
    end: Point2D,
    clicks: Vec<char>,
}

impl SubPath {
    pub fn new(start: Point2D, end: Point2D, clicks: Vec<char>) -> Self {
        Self { start, end, clicks }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct Key {
    sub_paths: Vec<SubPath>,
    level: usize,
}

impl Key {
    pub fn new(sub_paths: Vec<SubPath>, level: usize) -> Self {
        Self { sub_paths, level }
    }
}

fn solve_part1_and_2(lines: &[String], limit: usize) -> i64 {
    let mut numeric = HashMap::new();
    numeric.insert(Point2D::new(0, 0), '7');
    numeric.insert(Point2D::new(1, 0), '8');
    numeric.insert(Point2D::new(2, 0), '9');
    numeric.insert(Point2D::new(0, 1), '4');
    numeric.insert(Point2D::new(1, 1), '5');
    numeric.insert(Point2D::new(2, 1), '6');
    numeric.insert(Point2D::new(0, 2), '1');
    numeric.insert(Point2D::new(1, 2), '2');
    numeric.insert(Point2D::new(2, 2), '3');
    numeric.insert(Point2D::new(1, 3), '0');
    numeric.insert(Point2D::new(2, 3), 'A');

    let mut directional = HashMap::new();
    directional.insert(Point2D::new(1, 0), '^');
    directional.insert(Point2D::new(2, 0), 'A');
    directional.insert(Point2D::new(0, 1), '<');
    directional.insert(Point2D::new(1, 1), 'v');
    directional.insert(Point2D::new(2, 1), '>');

    let mut memo: HashMap<Key, i64> = HashMap::new();
    let mut numeric_cache: HashMap<(Point2D, Point2D), Vec<Vec<Dir>>> = HashMap::new();
    let mut directional_cache: HashMap<(Point2D, Point2D), Vec<Vec<Dir>>> = HashMap::new();
    lines
        .iter()
        .map(|line| {
            let level_0_initial = Point2D::new(2, 3);
            let chars: Vec<char> = line.chars().collect();
            let paths0: HashSet<Vec<SubPath>> =
                find_possible_paths(&level_0_initial, &chars, &numeric, &mut numeric_cache);
            let mut best = i64::MAX;
            for path0 in paths0 {
                let local_best = find_best_length(
                    &path0,
                    1,
                    limit,
                    &directional,
                    &mut memo,
                    &mut directional_cache,
                );
                if local_best < best {
                    best = local_best;
                }
            }
            best * line[0..3].parse::<i64>().unwrap()
        })
        .sum()
}

fn find_best_length(
    sub_paths: &Vec<SubPath>,
    level: usize,
    limit: usize,
    board: &HashMap<Point2D, char>,
    mem: &mut HashMap<Key, i64>,
    board_cache: &mut HashMap<(Point2D, Point2D), Vec<Vec<Dir>>>,
) -> i64 {
    let mut best = 0;
    let key = Key::new(sub_paths.to_vec(), level);
    if mem.contains_key(&key) {
        return mem[&key];
    }
    for sub_path in sub_paths {
        let possible_paths =
            find_possible_paths(&Point2D::new(2, 0), &sub_path.clicks, board, board_cache);
        if level == limit {
            best += possible_paths
                .iter()
                .map(|paths| paths.iter().map(|sb| sb.clicks.len() as i64).sum::<i64>())
                .min()
                .unwrap();
        } else {
            best += possible_paths
                .iter()
                .map(|paths| find_best_length(paths, level + 1, limit, board, mem, board_cache))
                .min()
                .unwrap();
        }
    }
    mem.insert(key, best);
    best
}

struct PositionNumeric {
    cur: Point2D,
    idx: usize,
    sub_paths: Vec<SubPath>,
}

impl PositionNumeric {
    pub fn new(cur: Point2D, idx: usize, sub_paths: Vec<SubPath>) -> Self {
        Self {
            cur,
            idx,
            sub_paths,
        }
    }
}

fn find_possible_paths(
    init: &Point2D,
    chars: &[char],
    board: &HashMap<Point2D, char>,
    board_cache: &mut HashMap<(Point2D, Point2D), Vec<Vec<Dir>>>,
) -> HashSet<Vec<SubPath>> {
    let mut paths = HashSet::new();
    let mut q = VecDeque::new();
    q.push_front(PositionNumeric::new(*init, 0, Vec::new()));
    while let Some(pos) = q.pop_back() {
        if pos.idx == chars.len() {
            paths.insert(pos.sub_paths);
            continue;
        }
        let n = chars[pos.idx];
        let target_pos = board.iter().find(|(_, v)| **v == n).unwrap().0;
        find_paths(&pos.cur, target_pos, board, board_cache)
            .iter()
            .for_each(|path| {
                let mut sub_paths = Vec::new();
                pos.sub_paths.iter().for_each(|sb| {
                    sub_paths.push(sb.clone());
                });
                let mut clicks: Vec<char> = path
                    .iter()
                    .map(|d| match d {
                        Dir::N => '^',
                        Dir::W => '<',
                        Dir::S => 'v',
                        Dir::E => '>',
                    })
                    .collect();
                clicks.push('A');
                sub_paths.push(SubPath::new(pos.cur, *target_pos, clicks));
                q.push_back(PositionNumeric::new(*target_pos, pos.idx + 1, sub_paths));
            });
    }
    paths
}

#[derive(Eq, PartialEq)]
struct Path {
    cur: Point2D,
    moves: Vec<Dir>,
    fitness: i32,
}

impl PartialOrd<Self> for Path {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> Ordering {
        other.fitness.cmp(&self.fitness)
    }
}

impl Path {
    pub fn new(cur: Point2D, moves: Vec<Dir>, fitness: i32) -> Self {
        Self {
            cur,
            moves,
            fitness,
        }
    }
}

fn find_paths(
    start: &Point2D,
    target: &Point2D,
    board: &HashMap<Point2D, char>,
    board_cache: &mut HashMap<(Point2D, Point2D), Vec<Vec<Dir>>>,
) -> Vec<Vec<Dir>> {
    let key = (*start, *target);
    if board.len() == 5 && board_cache.contains_key(&key) {
        return board_cache[&key].to_vec();
    }
    let mut result = Vec::new();
    let mut pq = BinaryHeap::new();
    pq.push(Path::new(*start, Vec::new(), start.manhattan(target)));
    let mut best = 1000;
    while let Some(path) = pq.pop() {
        if best < path.moves.len() {
            continue;
        }
        if path.cur == *target {
            if best > path.moves.len() {
                result = Vec::new();
                best = path.moves.len();
            }
            result.push(path.moves);
        } else {
            [Dir::E, Dir::N, Dir::W, Dir::S].iter().for_each(|d| {
                let n = path.cur + d;
                if board.contains_key(&n) {
                    let mut new_path = path.moves.to_vec();
                    new_path.push(*d);
                    pq.push(Path::new(n, new_path, n.manhattan(target)));
                }
            });
        }
    }
    board_cache.insert(key, result.to_vec());
    result
}

#[cfg(not(tarpaulin_include))]
fn main() {
    let lines = read_file_lines(YEAR, DAY, "input.txt");
    println!("{}", solve_part1_and_2(&lines, 2));
    println!("{}", solve_part1_and_2(&lines, 25));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1_and_2(&lines, 2), 126384);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        // calculated in java
        assert_eq!(solve_part1_and_2(&lines, 25), 154115708116294);
    }
}
