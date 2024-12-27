use crate::helper::read_file_lines;
use std::collections::HashMap;

const DAY: u8 = 11;

struct Item {
    num: i64,
    iteration: usize,
}

impl Item {
    fn new(num: i64, iteration: usize) -> Self {
        Self { num, iteration }
    }

    fn next(&self) -> Vec<Item> {
        if self.num == 0 {
            return vec![Item::new(1, self.iteration + 1)];
        }
        let digits = self.num.ilog(10) + 1;
        if digits % 2 == 0 {
            let pow = 10_i64.pow(digits / 2);
            vec![
                Item::new(self.num / pow, self.iteration + 1),
                Item::new(self.num % pow, self.iteration + 1),
            ]
        } else {
            vec![Item::new(self.num * 2024, self.iteration + 1)]
        }
    }
}

fn solve_part1_and_2(lines: &[String], blinks: usize) -> i64 {
    let mut memory: HashMap<(i64, usize), i64> = HashMap::new();
    lines[0]
        .split(" ")
        .map(|s| count_items(&Item::new(s.parse().unwrap(), 0), blinks, &mut memory))
        .sum()
}

fn count_items(item: &Item, blinks: usize, memory: &mut HashMap<(i64, usize), i64>) -> i64 {
    if item.iteration == blinks {
        return 1;
    }
    let key = (item.num, item.iteration);
    if let Some(value) = memory.get(&key) {
        return *value;
    }
    let sum = item
        .next()
        .iter()
        .map(|n| count_items(n, blinks, memory))
        .sum();
    memory.insert(key, sum);
    sum
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/{:0>2}/input.txt", DAY);
    let lines = read_file_lines(&full_path);
    println!("Day{:0>2}", DAY);
    println!("{}", solve_part1_and_2(&lines, 25));
    println!("{}", solve_part1_and_2(&lines, 75));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part1_and_2(&lines, 25), 55312);
    }
}
