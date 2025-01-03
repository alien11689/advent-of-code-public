use crate::Year;

mod day201501;

#[cfg(not(tarpaulin_include))]
pub fn year() -> Year {
    Year::new(2015, vec![day201501::day()])
}
