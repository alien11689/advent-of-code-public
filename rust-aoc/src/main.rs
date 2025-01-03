mod helper;
mod year2015;
mod year2023;
mod year2024;

use crate::helper::measure_time;
use std::env;

pub struct Day {
    pub year: u16,
    pub day: u8,
    pub run: fn(),
}

impl Day {
    pub fn new(year: u16, day: u8, run: fn()) -> Self {
        Self { year, day, run }
    }
}

pub struct Year {
    pub year: u16,
    pub days: Vec<Day>,
}

impl Year {
    pub fn new(year: u16, days: Vec<Day>) -> Self {
        Self { year, days }
    }
}

#[cfg(not(tarpaulin_include))]
fn main() {
    let params: Vec<String> = env::args().collect();
    let selected_year: Option<u16> = params.get(1).map(|s| s.parse().unwrap());
    let selected_day: Option<u8> = params.get(2).map(|s| s.parse().unwrap());

    measure_time(String::from("all"), || {
        let years = vec![year2015::year(), year2023::year(), year2024::year()];
        for year_wrapper in years {
            if let Some(selected_year) = selected_year {
                if selected_year != year_wrapper.year {
                    continue;
                }
            }
            println!("Year {}", year_wrapper.year);
            measure_time(format!("{}", year_wrapper.year), || {
                for day_wrapper in &year_wrapper.days {
                    if let Some(selected_day) = selected_day {
                        if selected_day != day_wrapper.day {
                            continue;
                        }
                    }
                    println!("Day{:0>2}", day_wrapper.day);
                    measure_time(
                        format!("{}/{:0>2}", day_wrapper.year, day_wrapper.day),
                        || {
                            (day_wrapper.run)();
                        },
                    )
                }
            });
        }
    });
}
