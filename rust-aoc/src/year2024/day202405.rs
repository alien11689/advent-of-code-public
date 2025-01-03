use crate::helper::read_file_lines;
use crate::Day;

const YEAR: u16 = 2024;
const DAY: u8 = 5;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1(lines: &[String]) -> i32 {
    let (rules, updates) = read_input(lines);
    updates
        .iter()
        .filter(|u| match_rules(u, &rules))
        .map(|u| u[(u.len() - 1) / 2])
        .sum()
}

fn read_input(lines: &[String]) -> (Vec<Vec<i32>>, Vec<Vec<i32>>) {
    let mut rules = Vec::new();
    let mut updates = Vec::new();
    for line in lines {
        if line.contains("|") {
            let parts: Vec<i32> = line.split("|").map(|p| p.parse().unwrap()).collect();
            rules.push(parts);
        } else if line.contains(",") {
            let parts: Vec<i32> = line.split(",").map(|p| p.parse().unwrap()).collect();
            updates.push(parts);
        }
    }
    (rules, updates)
}

fn match_rules(update: &[i32], rules: &[Vec<i32>]) -> bool {
    rules.iter().all(|rule| match_rule(rule, update))
}

fn match_rule(rule: &[i32], update: &[i32]) -> bool {
    if let Some(a) = update.iter().position(|x| *x == rule[0]) {
        if let Some(b) = update.iter().position(|x| *x == rule[1]) {
            return a < b;
        }
    }
    true
}

fn solve_part2(lines: &[String]) -> i32 {
    let (rules, updates) = read_input(lines);
    updates
        .iter()
        .filter(|u| !match_rules(u, &rules))
        .map(|u| fix(u, &rules))
        .map(|u| u[(u.len() - 1) / 2])
        .sum()
}

fn fix(update: &[i32], rules: &[Vec<i32>]) -> Vec<i32> {
    let mut update = update.to_owned();
    let interesting_rules: Vec<&Vec<i32>> = rules
        .iter()
        .filter(|r| r.iter().all(|a| update.contains(a)))
        .collect();
    loop {
        let mut changed = false;
        for rule in &interesting_rules {
            if let Some(a) = update.iter().position(|x| *x == rule[0]) {
                if let Some(b) = update.iter().position(|x| *x == rule[1]) {
                    if a > b {
                        update.remove(a);
                        update.insert(b, rule[0]);
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            return update;
        }
    }
}

#[cfg(not(tarpaulin_include))]
fn main() {
    let lines = read_file_lines(YEAR, DAY, "input.txt");
    println!("{}", solve_part1(&lines));
    println!("{}", solve_part2(&lines));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part1(&lines), 143);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(YEAR, DAY, "test1.txt");
        assert_eq!(solve_part2(&lines), 123);
    }
}
