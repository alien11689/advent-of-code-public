use crate::Year;

mod day202301;
mod day202302;
mod day202303;
mod day202304;

#[cfg(not(tarpaulin_include))]
pub fn year() -> Year {
    let days = vec![
        day202301::day(),
        day202302::day(),
        day202303::day(),
        day202304::day(),
    ];
    Year::new(2023, days)
}
