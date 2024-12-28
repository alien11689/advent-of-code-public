use crate::helper::read_file_lines;

const DAY: u8 = 17;

fn solve_part1(lines: &[String]) -> String {
    let a: u64 = lines[0]
        .split(": ")
        .last()
        .map(|v| v.parse().unwrap())
        .unwrap();
    let b: u64 = lines[1]
        .split(": ")
        .last()
        .map(|v| v.parse().unwrap())
        .unwrap();
    let c: u64 = lines[2]
        .split(": ")
        .last()
        .map(|v| v.parse().unwrap())
        .unwrap();
    let program = read_program(lines);
    let result: Vec<String> = run_program(a, b, c, &program)
        .iter()
        .map(|v| v.to_string())
        .collect();
    result.join(",")
}

fn read_program(lines: &[String]) -> Vec<u64> {
    lines
        .last()
        .unwrap()
        .split(": ")
        .last()
        .unwrap()
        .split(",")
        .map(|v| v.parse().unwrap())
        .collect()
}

fn run_program(a: u64, b: u64, c: u64, program: &[u64]) -> Vec<u64> {
    let mut a = a;
    let mut b = b;
    let mut c = c;
    let mut i = 0;
    let mut result = Vec::new();
    while i < program.len() {
        let cmd = program[i];
        let operand = program[i + 1];
        let real_operand = match operand {
            4 => a,
            5 => b,
            6 => c,
            7 => panic!(),
            other => other,
        };
        match cmd {
            0 => {
                let denom = 2_u64.pow(real_operand as u32);
                a /= denom;
            }
            1 => {
                b ^= operand;
            }
            2 => {
                b = real_operand % 8;
            }
            3 => {
                if a != 0 {
                    i = operand as usize;
                    continue;
                }
            }
            4 => {
                b ^= c;
            }
            5 => {
                result.push(real_operand % 8);
            }
            6 => {
                let denom = 2_u64.pow(real_operand as u32);
                b = a / denom;
            }
            7 => {
                let denom = 2_u64.pow(real_operand as u32);
                c = a / denom;
            }
            _ => panic!(),
        }
        i += 2;
    }
    result
}

fn solve_part2(lines: &[String]) -> u64 {
    let program = read_program(lines);
    let mut a = 0_u64;
    let mut suffix = 4;
    let mut subprogram = &program[(program.len() - suffix)..program.len()];
    loop {
        let result = run_program(a, 0, 0, &program);
        if result == subprogram {
            if result == program {
                break;
            }
            suffix += 1;
            a *= 8;
            a -= 1;
            subprogram = &program[(program.len() - suffix)..program.len()];
        }
        a += 1;
    }
    a
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
        assert_eq!(solve_part1(&lines), "4,6,3,5,6,3,5,2,1,0");
    }

    #[test]
    fn should_part1_pass_test_input2() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test2.txt", DAY));
        assert_eq!(solve_part1(&lines), "5,7,3,0");
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test2.txt", DAY));
        assert_eq!(solve_part2(&lines), 117440);
    }
}
