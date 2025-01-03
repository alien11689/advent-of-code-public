use crate::helper::read_file_lines;
use crate::Day;
use std::collections::{BTreeSet, HashSet};

const YEAR: u16 = 2024;
const DAY: u8 = 23;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1_and_2(lines: &[String]) -> (usize, String) {
    let mut edges: Vec<String> = Vec::new();
    let mut connections: HashSet<BTreeSet<String>> = lines
        .iter()
        .map(|line| {
            let parts: BTreeSet<String> = line.split("-").map(|s| s.to_string()).collect();
            parts.iter().for_each(|p| {
                edges.push(p.clone());
            });
            parts
        })
        .collect();
    let mut lans: HashSet<BTreeSet<String>> = HashSet::new();
    edges.iter().for_each(|e1| {
        let mut connected: Vec<&String> = connections
            .iter()
            .filter(|c| c.contains(e1))
            .flatten()
            .filter(|ee| *ee != e1)
            .collect();
        connected.sort();
        for (x, e2) in connected.iter().enumerate() {
            for e3 in connected.iter().skip(x) {
                let mut con: BTreeSet<String> = BTreeSet::new();
                con.insert((*e2).clone());
                con.insert((*e3).clone());
                if connections.contains(&con) {
                    con.insert(e1.clone());
                    lans.insert(con);
                }
            }
        }
    });
    let part1 = lans
        .iter()
        .filter(|triple| triple.iter().any(|s| s.starts_with("t")))
        .count();

    let mut best = 0;
    let mut best_name = String::new();
    while !lans.is_empty() {
        let mut cur = lans.iter().next().unwrap().clone();
        loop {
            let prev_size = cur.len();
            lans.iter().for_each(|l| {
                let intersection: BTreeSet<String> = l.intersection(&cur).cloned().collect();
                if intersection.len() == 2 {
                    let left: Vec<String> = l.difference(&intersection).cloned().collect();
                    let left = left.first().unwrap();
                    if cur.iter().all(|c| {
                        let mut con = BTreeSet::new();
                        con.insert(c.clone());
                        con.insert(left.clone());
                        connections.contains(&con)
                    }) {
                        cur.insert(left.clone());
                    }
                }
            });
            connections = connections
                .iter()
                .filter(|con| {
                    con.intersection(&cur)
                        .cloned()
                        .collect::<Vec<String>>()
                        .is_empty()
                })
                .cloned()
                .collect();
            lans = lans
                .iter()
                .filter(|lan| {
                    lan.intersection(&cur)
                        .cloned()
                        .collect::<Vec<String>>()
                        .is_empty()
                })
                .cloned()
                .collect();
            if prev_size == cur.len() {
                break;
            }
        }
        let size = cur.len();
        if size > best {
            best = size;
            let mut res = Vec::new();
            cur.iter().for_each(|v| res.push(v.clone()));
            res.sort();
            best_name = res.join(",");
        }
    }

    (part1, best_name)
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
        assert_eq!(solve_part1_and_2(&lines).0, 7);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1_and_2(&lines).1, "co,de,ka,ta");
    }
}
