use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day20{
    field: (),
}

pub fn setup(_input:&str) -> Result<Day20, Box<dyn Error>>{
    let mut res = Day20{
        field: (),
    };
    Ok(res)   
}

impl Problem for Day20{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Err(new_err("Unimplemented"))
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Err(new_err("Unimplemented"))
    }
}
