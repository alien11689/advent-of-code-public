use crate::helper::read_file_lines;
use std::collections::{HashMap, HashSet, LinkedList};

const DAY: u8 = 22;

fn solve(lines: &[String]) -> (i64, i64) {
    let mut map: HashMap<String, i64> = HashMap::new();
    let part1 = lines
        .iter()
        .map(|line| calculate(line, 2000, &mut map))
        .sum();
    let part2 = *map.values().max().unwrap();
    (part1, part2)
}

fn calculate(init: &String, iter: usize, map: &mut HashMap<String, i64>) -> i64 {
    let mut cur: i64 = init.parse().unwrap();
    let mut last = cur % 10;
    let mut q = LinkedList::new();
    let mut seen: HashSet<String> = HashSet::new();
    for _ in 0..iter {
        cur = (cur ^ (cur << 6)) % 16777216;
        cur = (cur ^ (cur >> 5)) % 16777216;
        cur = (cur ^ (cur << 11)) % 16777216;
        let new_last = cur % 10;
        if q.len() == 4 {
            q.pop_front();
        }
        q.push_back(new_last - last);
        let key = format!("{:?}", q);
        if !seen.contains(&key) && q.len() == 4 {
            if let Some(value) = map.get_mut(&key) {
                *value += new_last;
            } else {
                map.insert(key.to_owned(), new_last);
            }
            seen.insert(key);
        }
        last = new_last;
    }
    cur
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/{:0>2}/input.txt", DAY);
    let lines = read_file_lines(&full_path);
    println!("Day{:0>2}", DAY);
    let (part1, part2) = solve(&lines);
    println!("{part1}");
    println!("{part2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input2() {
        assert_eq!(
            calculate(&"123".to_string(), 10, &mut HashMap::new()),
            5908254
        );
    }

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve(&lines).0, 37327623);
    }

    #[test]
    fn should_part2_pass_test_input3() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test3.txt", DAY));
        assert_eq!(solve(&lines).1, 23);
    }
}
