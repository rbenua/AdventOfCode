use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day5{
    passes: Vec<(String, String)>,
}

fn count(s: &str, one: char) -> usize {
    let mut n = 0;
    for c in s.chars() {
        n = n << 1;
        if c == one {
            n += 1;
        }
    }
    n
}

fn id(row: &str, col: &str) -> usize {
    count(row, 'B') * 8 + count(col, 'R')
}

pub fn setup(_input:&str) -> Result<Day5, Box<dyn Error>>{
    let mut res = Day5{
        passes: Vec::new(),
    };
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        res.passes.push((line[0..7].to_string(), line[7..10].to_string()));
    }
    Ok(res)   
}

impl Problem for Day5{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut max = 0;
        for (row, col) in &self.passes {
            let i = id(row, col);
            if i > max {
                max = i;
            }
        }
        Ok(max.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut ids : Vec<usize> = self.passes.iter().map(|(row, col)|{id(row, col)}).collect();
        ids.sort();
        for i in 0..(ids.len() - 1) {
            if ids[i+1] != ids[i] + 1 {
                return Ok((ids[i] + 1).to_string());
            }
        }
        Err(new_err("not found"))
    }
}
