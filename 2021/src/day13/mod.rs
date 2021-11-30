use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day13{
    placeholder: i64,
}

pub fn setup(_input:&str) -> Result<Day13, Box<dyn Error>>{
    Err(new_err("Unimplemented"))
}

impl Problem for Day13{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok("".to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok("".to_string())
    }
}
