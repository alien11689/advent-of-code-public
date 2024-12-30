use crate::helper::{read_file_lines, Dir, Point2D};
use std::collections::{HashSet, VecDeque};

const DAY: u8 = 15;

fn solve_part1(lines: &[String]) -> i32 {
    let (blocks, mut boxes, mut q, mut robot) = read_input(lines);
    while let Some(dir) = q.pop_front() {
        let next_robot = robot + &dir;
        if blocks.contains(&next_robot) {
            continue;
        }
        if boxes.contains(&next_robot) {
            let mut place_for_robot = next_robot + &dir;
            loop {
                if blocks.contains(&place_for_robot) {
                    break;
                }
                if boxes.contains(&place_for_robot) {
                    place_for_robot = place_for_robot + &dir;
                    continue;
                }
                robot = next_robot;
                boxes.remove(&next_robot);
                boxes.insert(place_for_robot);
                break;
            }
        } else {
            robot = next_robot;
        }
    }
    boxes.iter().map(|p| p.y * 100 + p.x).sum()
}

fn read_input(lines: &[String]) -> (HashSet<Point2D>, HashSet<Point2D>, VecDeque<Dir>, Point2D) {
    let mut blocks = HashSet::new();
    let mut boxes = HashSet::new();
    let mut robot = HashSet::new();
    let mut q = VecDeque::new();
    for (y, line) in lines.iter().enumerate() {
        if line.is_empty() {
            continue;
        } else if line.contains("#") {
            for (x, c) in line.chars().enumerate() {
                let x = x as i32;
                let y = y as i32;
                if c == '#' {
                    blocks.insert(Point2D::new(x, y));
                } else if c == 'O' {
                    boxes.insert(Point2D::new(x, y));
                } else if c == '@' {
                    robot.insert(Point2D::new(x, y));
                }
            }
        } else {
            line.chars()
                .map(|c| match c {
                    '<' => Dir::W,
                    '^' => Dir::N,
                    '>' => Dir::E,
                    'v' => Dir::S,
                    _ => panic!(),
                })
                .for_each(|d| q.push_back(d));
        }
    }
    let robot = *robot.iter().next().unwrap();
    (blocks, boxes, q, robot)
}

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
struct Box {
    left: Point2D,
    right: Point2D,
}

impl Box {
    fn mv(&self, dir: &Dir) -> Box {
        Box::new(self.left + dir, self.right + dir)
    }

    fn contains(&self, p0: &Point2D) -> bool {
        *p0 == self.left || *p0 == self.right
    }

    fn new(left: Point2D, right: Point2D) -> Self {
        Self { left, right }
    }

    fn flatten(&self) -> HashSet<Point2D> {
        let mut res = HashSet::new();
        res.insert(self.left);
        res.insert(self.right);
        res
    }
}

fn read_input2(lines: &[String]) -> (HashSet<Point2D>, HashSet<Box>, VecDeque<Dir>, Point2D) {
    let mut blocks = HashSet::new();
    let mut boxes = HashSet::new();
    let mut robot = HashSet::new();
    let mut q = VecDeque::new();
    for (y, line) in lines.iter().enumerate() {
        if line.is_empty() {
            continue;
        } else if line.contains("#") {
            for (x, c) in line.chars().enumerate() {
                let x = x as i32;
                let y = y as i32;
                if c == '#' {
                    blocks.insert(Point2D::new(x * 2, y));
                    blocks.insert(Point2D::new(x * 2 + 1, y));
                } else if c == 'O' {
                    boxes.insert(Box::new(Point2D::new(x * 2, y), Point2D::new(x * 2 + 1, y)));
                } else if c == '@' {
                    robot.insert(Point2D::new(x * 2, y));
                }
            }
        } else {
            line.chars()
                .map(|c| match c {
                    '<' => Dir::W,
                    '^' => Dir::N,
                    '>' => Dir::E,
                    'v' => Dir::S,
                    _ => panic!(),
                })
                .for_each(|d| q.push_back(d));
        }
    }
    let robot = *robot.iter().next().unwrap();
    (blocks, boxes, q, robot)
}

fn solve_part2(lines: &[String]) -> i32 {
    let (blocks, mut boxes, mut q, mut robot) = read_input2(lines);
    while let Some(dir) = q.pop_front() {
        let next_robot = robot + &dir;
        if blocks.contains(&next_robot) {
            continue;
        }
        let box_points: HashSet<Point2D> = boxes.iter().flat_map(|b| b.flatten()).collect();
        if box_points.contains(&next_robot) {
            let mut boxes_copy = boxes.clone();
            let mut boxes_to_move: HashSet<Box> = boxes
                .iter()
                .filter(|b| b.contains(&next_robot))
                .copied()
                .collect();
            boxes_to_move.iter().for_each(|b| {
                boxes_copy.remove(b);
            });
            loop {
                let new_boxes: HashSet<Box> = boxes_to_move.iter().map(|b| b.mv(&dir)).collect();
                if new_boxes
                    .iter()
                    .any(|b| blocks.contains(&b.left) || blocks.contains(&b.right))
                {
                    break;
                }
                let new_boxes_flatten: HashSet<Point2D> =
                    new_boxes.iter().flat_map(|b| b.flatten()).collect();
                let next_boxes_to_move: HashSet<Box> = boxes_copy
                    .iter()
                    .filter(|b| {
                        new_boxes_flatten.contains(&b.left)
                            || new_boxes_flatten.contains(&b.right)
                    })
                    .copied()
                    .collect();
                if next_boxes_to_move.is_empty() {
                    robot = next_robot;
                    boxes_to_move.iter().for_each(|bb| {
                        boxes.remove(bb);
                    });
                    new_boxes.iter().for_each(|bb| {
                        boxes.insert(*bb);
                    });
                    break;
                } else {
                    next_boxes_to_move.iter().for_each(|nb| {
                        boxes_copy.remove(nb);
                        boxes_to_move.insert(*nb);
                    });
                }
            }
            //                 while (true) {
            //                     Set<Box> newBoxes = boxesToMove.stream().map(b -> b.move(dir)).collect(Collectors.toSet());
            //                     if (newBoxes.stream().anyMatch(b -> blocks.contains(b.left) || blocks.contains(b.right))) {
            //                         //cannot move because of blocks
            //                         break;
            //                     }
            //                     Set<Point2D> newBoxesFlattenInternal = newBoxes.stream().flatMap(b -> Stream.of(b.left, b.right)).collect(Collectors.toSet());
            //                     Set<Box> nextBoxesToMove = boxesCopy.stream().filter(b -> newBoxesFlattenInternal.contains(b.left) || newBoxesFlattenInternal.contains(b.right)).collect(Collectors.toSet());
            //                     if (nextBoxesToMove.isEmpty()) {
            //                         // can move robot and all the boxes
            //                         robot = nextRobot;
            // //                        System.out.println("Removing " + boxesToMove);
            // //                        System.out.println("Adding " + newBoxes);
            //                         boxes.removeAll(boxesToMove);
            //                         boxes.addAll(newBoxes);
            //                         break;
            //                     } else {
            //                         // add next boxes and try to move all again
            //                         boxesCopy.removeAll(nextBoxesToMove);
            //                         boxesToMove.addAll(nextBoxesToMove);
            //                     }
            //                 }
        } else {
            robot = next_robot;
        }
    }
    boxes.iter().map(|p| p.left.y * 100 + p.left.x).sum()
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
        assert_eq!(solve_part1(&lines), 10092);
    }

    #[test]
    fn should_part1_pass_test_input2() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test2.txt", DAY));
        assert_eq!(solve_part1(&lines), 2028);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part2(&lines), 9021);
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test2.txt", DAY));
        assert_eq!(solve_part2(&lines), 1751);
    }

    #[test]
    fn should_part2_pass_test_input3() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test3.txt", DAY));
        assert_eq!(solve_part2(&lines), 618);
    }
}
