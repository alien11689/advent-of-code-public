use crate::helper::read_file_lines;
use crate::year2024::day202424::RuleType::{And, Or, Xor};
use crate::Day;
use regex::Regex;
use std::collections::{HashMap, HashSet};

const YEAR: u16 = 2024;
const DAY: u8 = 24;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

#[derive(Eq, PartialEq, Hash)]
struct Rule {
    a: String,
    b: String,
    out: String,
    rule_type: RuleType,
}

impl Rule {
    pub(crate) fn apply(&self, inputs: &mut HashMap<String, bool>) -> bool {
        if let Some(va) = inputs.get(&self.a) {
            if let Some(vb) = inputs.get(&self.b) {
                if !inputs.contains_key(&self.out) {
                    inputs.insert(
                        self.out.clone(),
                        match self.rule_type {
                            And => *va && *vb,
                            Or => *va || *vb,
                            Xor => *va != *vb,
                        },
                    );
                    return true;
                }
            }
        }
        false
    }
}

impl Rule {
    fn new(a: String, b: String, out: String, rule_type: String) -> Rule {
        Rule {
            a,
            b,
            out,
            rule_type: match rule_type.as_str() {
                "AND" => And,
                "OR" => Or,
                "XOR" => Xor,
                _ => panic!(),
            },
        }
    }
}

#[derive(Eq, PartialEq, Hash)]
enum RuleType {
    And,
    Or,
    Xor,
}

fn solve_part1(lines: &[String]) -> i64 {
    let (rules, mut inputs) = parse_input(lines);
    loop {
        let mut changed = false;
        for rule in &rules {
            changed = changed || rule.apply(&mut inputs);
        }
        if !changed {
            break;
        }
    }
    let mut res = Vec::new();
    let mut i = 0;
    while let Some(v) = inputs.get(&format!("z{i:02}")) {
        // println!("{i}: {v}");
        res.push(if *v { "1" } else { "0" });
        i += 1;
    }
    // println!("{inputs:?}");
    res.reverse();
    let src = &res.join("");
    i64::from_str_radix(src, 2).unwrap()
}

fn parse_input(lines: &[String]) -> (HashSet<Rule>, HashMap<String, bool>) {
    let mut rules = HashSet::new();
    let mut inputs = HashMap::new();
    let regex = Regex::new(r"[ \->]+").unwrap();
    lines.iter().for_each(|line| {
        if line.contains(":") {
            let parts: Vec<&str> = line.split(": ").collect();
            inputs.insert(parts[0].to_string(), parts[1] == "1");
        } else if line.contains(">") {
            let parts: Vec<&str> = regex.split(line).collect();
            rules.insert(Rule::new(
                parts[0].to_string(),
                parts[2].to_string(),
                parts[3].to_string(),
                parts[1].to_string(),
            ));
        }
    });
    (rules, inputs)
}

fn solve_part2(lines: &[String]) -> String {
    let (rules, _) = parse_input(lines);
    let mut wrong_outputs = HashSet::new();
    let z_out_count = rules.iter().filter(|r| r.out.starts_with("z")).count();
    for rule in &rules {
        // all Z outputs come from XOR
        // OR inputs are ANDs
        // no two AND in a row
        // no three XOR in a row - the middle one is invalid
        // single OR joins partial adders
        if rule.out.starts_with("z")
            && rule.out != format!("z{:02}", z_out_count - 1)
            && rule.rule_type != Xor
        {
            wrong_outputs.insert(rule.out.clone());
        }
        if rule.rule_type == Or {
            let rule_a = rules.iter().find(|ra| ra.out == rule.a).unwrap();
            let rule_b = rules.iter().find(|ra| ra.out == rule.b).unwrap();
            if rule_a.rule_type != And {
                wrong_outputs.insert(rule.a.clone());
            }
            if rule_b.rule_type != And {
                wrong_outputs.insert(rule.b.clone());
            }
        }
        if rule.rule_type == And {
            if let Some(r) = rules.iter().find(|ra| ra.out == rule.a) {
                if r.rule_type == And && !r.a.ends_with("00") && !r.b.ends_with("00") {
                    wrong_outputs.insert(r.out.clone());
                }
            }
            if let Some(r) = rules.iter().find(|r| r.out == rule.b) {
                if r.rule_type == And && !r.a.ends_with("00") && !r.b.ends_with("00") {
                    wrong_outputs.insert(r.out.clone());
                }
            }
        }
        if rule.rule_type == Xor {
            if let Some(r) = rules.iter().find(|r| r.out == rule.a && r.rule_type == Xor) {
                if rules.iter().any(|rn| rn.out == r.a && r.rule_type == Xor) {
                    wrong_outputs.insert(r.out.clone());
                }
                if rules.iter().any(|rn| rn.out == r.b && r.rule_type == Xor) {
                    wrong_outputs.insert(r.out.clone());
                }
            }
            if let Some(r) = rules.iter().find(|r| r.out == rule.b && r.rule_type == Xor) {
                if rules.iter().any(|rn| rn.out == r.a && r.rule_type == Xor) {
                    wrong_outputs.insert(r.out.clone());
                }
                if rules.iter().any(|rn| rn.out == r.b && r.rule_type == Xor) {
                    wrong_outputs.insert(r.out.clone());
                }
            }
        }
    }
    let mut result = Vec::with_capacity(8);
    for o in wrong_outputs {
        result.push(o);
    }
    result.sort();
    result.join(",")
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
        assert_eq!(solve_part1(&lines), 4);
    }

    #[test]
    fn should_part1_pass_test_input2() {
        let lines = read_file_lines(YEAR, DAY, "test2.txt");
        assert_eq!(solve_part1(&lines), 2024);
    }
}
