use crate::helper::read_file_lines;
use regex::Regex;

const DAY: u8 = 13;

fn solve_part1_and_2(lines: &[String], result_increment: i64) -> i64 {
    let mut res = 0_i64;
    let mut i = 0;
    let regex = Regex::new(r"[+,=]").unwrap();
    while i < lines.len() {
        let (ax, ay) = read_line_input(&regex, &lines[i]);
        let (bx, by) = read_line_input(&regex, &lines[i + 1]);
        let (rx, ry) = read_line_input(&regex, &lines[i + 2]);
        let rx = rx + result_increment;
        let ry = ry + result_increment;
        // println!("ax={ax}, ay={ay}, bx={bx}, by={by}, rx={rx}, ry={ry}");
        let mut bmax = (rx / bx).min(ry / by);
        let mut bmin = 0_i64;
        while bmin <= bmax {
            // println!("Checking range {bmin}..{bmax}");
            let diff_for_bmin = calculate_diff(ax, ay, bx, by, rx, ry, bmin);
            let diff_for_bmax = calculate_diff(ax, ay, bx, by, rx, ry, bmax);
            if diff_for_bmin as i64 != 0
                && diff_for_bmax as i64 != 0
                && diff_for_bmax.signum() as i64 == diff_for_bmin.signum() as i64
            {
                // println!("Dropping since for diff b min = {diff_for_bmin}, b max = {diff_for_bmax}; signums {} and {}", diff_for_bmin.signum(), diff_for_bmax.signum());
                break;
            }
            let b = (bmax + bmin) / 2;
            let diff_for_b = calculate_diff(ax, ay, bx, by, rx, ry, b);
            let res_x = rx - b * bx;
            let res_y = ry - b * by;
            let la1 = res_x / ax;
            let la2 = res_y / ay;
            if la1 >= 0 && la1 == la2 && la1 * ax == res_x && la2 * ay == res_y {
                // println!("For b = {b} possible a = {la1}, diff = {diff_for_b}");
                res += la1 * 3 + b;
                break;
            }
            if diff_for_bmin < diff_for_bmax {
                if diff_for_b > 0_f64 {
                    bmax = b - 1;
                } else {
                    bmin = b + 1;
                }
            } else if diff_for_b < 0_f64 {
                bmax = b - 1;
            } else {
                bmin = b + 1;
            }
        }
        i += 4;
    }
    res
}

fn calculate_diff(ax: i64, ay: i64, bx: i64, by: i64, rx: i64, ry: i64, cur: i64) -> f64 {
    let res_x = rx - cur * bx;
    let res_y = ry - cur * by;
    let a1 = res_x as f64 / ax as f64;
    let a2 = res_y as f64 / ay as f64;
    a1 - a2
}

fn read_line_input(regex: &Regex, line: &str) -> (i64, i64) {
    let parts: Vec<&str> = regex.split(line).collect();
    let x: i64 = parts[1].parse().unwrap();
    let y: i64 = parts[3].parse().unwrap();
    (x, y)
}

#[cfg(not(tarpaulin_include))]
pub fn main(path: &String) {
    let full_path = format!("{path}/resources/2024/{:0>2}/input.txt", DAY);
    let lines = read_file_lines(&full_path);
    println!("Day{:0>2}", DAY);
    println!("{}", solve_part1_and_2(&lines, 0));
    println!("{}", solve_part1_and_2(&lines, 10000000000000));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_part1_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part1_and_2(&lines, 0), 480);
    }

    #[test]
    fn should_part2_pass_test_input2() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test2.txt", DAY));
        assert_eq!(solve_part1_and_2(&lines, 10000000000000), 76358113886726);
    }
}
