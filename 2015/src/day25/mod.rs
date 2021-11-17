use crate::Problem;
use crate::read_lines;
use std::error::Error;
use std::fs::read_to_string;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::result::Result;
use std::collections::{HashSet, HashMap, VecDeque};
use enum_map::EnumMap;

#[derive(Debug)]
pub struct Day25{
    goal_row: i64,
    goal_col: i64,
}

pub fn setup(_input:&str) -> Result<Day25, Box<dyn Error>>{
    let re = Regex::new(r"row (\d+), column (\d+)\.")?;
    let text = read_to_string(_input)?;
    let cs = re.captures(&text).ok_or(new_err("match failed"))?;
    Ok(Day25{
        goal_row: cs[1].parse()?,
        goal_col: cs[2].parse()?,
    })
}

fn sequence(row: i64, col: i64) -> i64 {
    let diag = row + col - 1;
    let base = (diag - 1) * diag / 2;
    base + col 
}

fn step(code: u64) -> u64 {
    (code * 252533) % 33554393
}

impl Problem for Day25{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut code = 20151125;
        for _ in 0..sequence(self.goal_row, self.goal_col) - 1 {
            code = step(code);
        }
        Ok(code.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok("**".to_string())
    }
}