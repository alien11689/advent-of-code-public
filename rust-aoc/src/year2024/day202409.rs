use crate::helper::read_file_lines;
use std::collections::VecDeque;

const DAY: u8 = 9;

#[derive(Clone)]
struct Elem {
    file_id: i64,
    count: usize,
    empty: bool,
}

fn solve_part1(lines: &[String]) -> i64 {
    let mut q = parse_input(lines);
    let mut checksum = 0_i64;
    let mut position = 0_i64;
    while let Some(elem) = q.pop_front() {
        if elem.empty {
            let last = q.pop_back().unwrap();
            if last.empty {
                q.push_front(elem);
            } else if last.count > elem.count {
                q.push_front(Elem {
                    file_id: last.file_id,
                    count: elem.count,
                    empty: false,
                });
                q.push_back(Elem {
                    file_id: last.file_id,
                    count: last.count - elem.count,
                    empty: false,
                });
            } else if last.count == elem.count {
                q.push_front(Elem {
                    file_id: last.file_id,
                    count: last.count,
                    empty: false,
                });
            } else {
                q.push_front(Elem {
                    file_id: 0,
                    count: elem.count - last.count,
                    empty: true,
                });
                q.push_front(Elem {
                    file_id: last.file_id,
                    count: last.count,
                    empty: false,
                });
            }
        } else {
            for _ in 0..elem.count {
                checksum += elem.file_id * position;
                position += 1;
            }
        }
    }
    checksum
}

fn parse_input(lines: &[String]) -> VecDeque<Elem> {
    let mut q = VecDeque::new();
    for (i, c) in lines[0].chars().enumerate() {
        let count = c.to_digit(10).unwrap() as usize;
        let empty = i % 2 == 1;
        let file_id: i64 = if empty {
            i as i64 / 2 - 1
        } else {
            i as i64 / 2
        };
        if count > 0 {
            q.push_back(Elem {
                count,
                empty,
                file_id,
            })
        }
    }
    q
}

fn solve_part2(lines: &[String]) -> i64 {
    let mut q = parse_input(lines);
    let mut elems = Vec::with_capacity(q.len());
    while let Some(elem) = q.pop_front() {
        elems.push(elem);
    }
    let mut current_file_id = elems.iter().map(|e| e.file_id).max().unwrap();
    let mut lowest_file_id = elems.iter().map(|e| e.file_id).min().unwrap();
    while current_file_id > 0 {
        let current = elems.iter().find(|e| e.file_id == current_file_id).unwrap();
        let current_idx = elems
            .iter()
            .position(|e| e.file_id == current_file_id)
            .unwrap();
        for i in 0..current_idx {
            let checked = &elems[i];
            if checked.empty && checked.count >= current.count {
                lowest_file_id -= 1;
                let mut new_elems = Vec::new();
                for (j, elem) in elems.iter().enumerate() {
                    if j == i {
                        new_elems.push(current.clone());
                        if checked.count > current.count {
                            new_elems.push(Elem {
                                file_id: checked.file_id,
                                count: checked.count - current.count,
                                empty: true,
                            });
                        }
                    } else if j == current_idx {
                        new_elems.push(Elem {
                            file_id: lowest_file_id,
                            count: current.count,
                            empty: true,
                        });
                    } else {
                        new_elems.push(elem.clone());
                    }
                }
                elems = new_elems;
                break;
            }
        }
        current_file_id -= 1;
    }
    let mut checksum = 0_i64;
    let mut position = 0_i64;
    for elem in elems {
        for _ in 0..elem.count {
            if elem.empty {
                position += 1;
            } else {
                checksum += elem.file_id * position;
                position += 1;
            }
        }
    }
    checksum
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
        assert_eq!(solve_part1(&lines), 1928);
    }

    #[test]
    fn should_part2_pass_test_input1() {
        let lines = read_file_lines(&format!("./resources/2024/{:0>2}/test1.txt", DAY));
        assert_eq!(solve_part2(&lines), 2858);
    }
}
