use std::cmp::Reverse;
use crate::helper::read_file_lines;
use std::collections::{BinaryHeap, HashMap, HashSet};

const DAY: u8 = 19;

fn solve_part1_and_2(lines: &[String]) -> (i32, i64) {
    let towels: Vec<String> = lines[0].split(", ").map(String::from).collect();
    let mut part1 = 0;
    let mut part2 = 0;
    for i in 1..lines.len() {
        let design = &lines[i];
        if !design.is_empty() {
            let possible = find_possible(design, &towels);
            // println!("For {design} is {possible}");
            if possible > 0 {
                part1 += 1;
            }
            part2 += possible;
        }
    }
    (part1, part2)
}

fn find_possible(design: &String, towels: &Vec<String>) -> i64 {
    let possible_towels: HashSet<&String> = towels.iter().filter(|t| design.contains(*t)).collect();
    let mut passes: HashSet<(usize, usize)> = HashSet::new();
    for i in 0..design.len() {
        possible_towels.iter().for_each(|t| {
            if design[i..].starts_with(*t) {
                passes.insert((i, i + t.len()));
            }
        });
    }
    let mut target_to_ways: HashMap<usize, i64> = HashMap::new();
    target_to_ways.insert(0, 1);
    let mut min: i32 = -1;
    let mut q = BinaryHeap::new();
    q.push(Reverse(0usize));
    while let Some(Reverse(cur)) = q.pop() {
        if cur as i32 <= min {
            continue;
        }
        min = cur as i32;
        // println!("Checking {} from q", cur);
        let ways = target_to_ways.get(&cur).unwrap().clone();
        let mut to_remove = HashSet::new();
        for v in &passes {
            if v.0 == cur {
                // println!("Checking {:?}", v);
                let target = v.1;
                *target_to_ways.entry(target).or_insert(0) += ways;
                q.push(Reverse(target));
                to_remove.insert(v.clone());
            }
        }
        for r in to_remove {
            passes.remove(&r);
        }
    }
    // println!("{} -> {:?}", design.len(), target_to_ways);
    *target_to_ways.get(&design.len()).unwrap_or(&0)
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/{:0>2}/input.txt", DAY);
    let lines = read_file_lines(&full_path);
    println!("Day{:0>2}", DAY);
    let (part1, part2) = solve_part1_and_2(&lines);
    println!("{}", part1);
    println!("{}", part2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part1_and_2(&lines).0, 6);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part1_and_2(&lines).1, 16);
    }
}
