use crate::helper::read_file_lines;
use crate::Day;

const YEAR: u16 = 2015;
const DAY: u8 = 1;

pub fn day() -> Day {
    Day::new(YEAR, DAY, main)
}

fn solve_part1(input: &str) -> i32 {
    let mut pos = 0;
    for cur in input.chars() {
        match cur {
            '(' => pos += 1,
            ')' => pos -= 1,
            _ => {
                unreachable!("Unknown char {cur}")
            }
        }
    }
    pos
}

fn solve_part2(input: &str) -> i32 {
    let mut pos = 0;
    for (idx, cur) in input.chars().enumerate() {
        match cur {
            '(' => pos += 1,
            ')' => pos -= 1,
            _ => {
                unreachable!("Unknown char {cur}")
            }
        }
        if pos < 0 {
            return (idx as i32) + 1;
        }
    }
    unreachable!("No basement for {input}")
}

#[cfg(not(tarpaulin_include))]
fn main() {
    let lines = read_file_lines(YEAR, DAY, "input.txt");
    let input = lines.first().unwrap();
    println!("{}", solve_part1(input));
    println!("{}", solve_part2(input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("(())", 0)]
    #[case("()()", 0)]
    #[case("(((", 3)]
    #[case("(()(()(", 3)]
    #[case("))(((((", 3)]
    #[case ("())", -1)]
    #[case("))(", -1)]
    #[case(")))", -3)]
    #[case(")())())", -3)]
    fn should_part1_pass_test_inputs(#[case] inp: &str, #[case] res: i32) {
        assert_eq!(solve_part1(&String::from(inp)), res);
    }

    #[rstest]
    #[case(")", 1)]
    #[case("()())", 5)]
    fn should_part2_pass_test(#[case] inp: &str, #[case] res: i32) {
        assert_eq!(solve_part2(&String::from(inp)), res);
    }
}
