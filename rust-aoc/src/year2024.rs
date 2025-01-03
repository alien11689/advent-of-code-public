use crate::Year;

mod day202401;
mod day202402;
mod day202403;
mod day202404;
mod day202405;
mod day202406;
mod day202407;
mod day202408;
mod day202409;
mod day202410;
mod day202411;
mod day202412;
mod day202413;
mod day202414;
mod day202415;
mod day202416;
mod day202417;
mod day202418;
mod day202419;
mod day202420;
mod day202421;
mod day202422;
mod day202423;
mod day202424;
mod day202425;

#[cfg(not(tarpaulin_include))]
pub fn year() -> Year {
    let days = vec![
        day202401::day(),
        day202402::day(),
        day202403::day(),
        day202404::day(),
        day202405::day(),
        day202406::day(),
        day202407::day(),
        day202408::day(),
        day202409::day(),
        day202410::day(),
        day202411::day(),
        day202412::day(),
        day202413::day(),
        day202414::day(),
        day202415::day(),
        day202416::day(),
        day202417::day(),
        day202418::day(),
        day202419::day(),
        day202420::day(),
        day202421::day(),
        day202422::day(),
        day202423::day(),
        day202424::day(),
        day202425::day(),
    ];
    Year::new(2024, days)
}
