use crate::helper::measure_time;

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
pub fn main(path_prefix: &String) {
    let days = vec![
        day202401::main,
        day202402::main,
        day202403::main,
        day202404::main,
        day202405::main,
        day202406::main,
        // day202407::main,
        // day202408::main,
        // day202409::main,
        // day202410::main,
        // day202411::main,
        // day202412::main,
        // day202413::main,
        // day202414::main,
        // day202415::main,
        // day202416::main,
        // day202417::main,
        // day202418::main,
        // day202419::main,
        // day202420::main,
        // day202421::main,
        day202422::main,
        // day202423::main,
        // day202424::main,
        // day202425::main,
    ];
    println!("Year 2024");
    for day in days {
        measure_time(|| {
            day(path_prefix);
        });
    }
}
