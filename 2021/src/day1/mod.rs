use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::VecDeque;

pub struct Day1{
    depths: Vec<i64>,
}

pub fn setup(_input:&str) -> Result<Day1, Box<dyn Error>>{
    let mut depths = Vec::new();
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        depths.push(line.trim().parse::<i64>()?);
    }
    Ok(Day1{
        depths,
    })
}

impl Problem for Day1{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut prev = i64::MAX;
        let mut total = 0;
        for cur in &self.depths {
            if *cur > prev {
                total += 1;
            }
            prev = *cur;
        }
        Ok(total.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut total : u64 = 0;
        for i in 3..self.depths.len() {
            if self.depths[i] > self.depths[i-3] {
                total += 1;
            }
        }
        Ok(total.to_string())
    }
}
