use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

const WINDOW_SIZE: usize = 25;

pub struct Day9{
    input: Vec<u64>,
}

pub fn setup(_input:&str) -> Result<Day9, Box<dyn Error>>{
    let mut res = Day9{
        input: Vec::new(),
    };
    for line in read_lines(_input) {
        res.input.push(line?.parse()?);
    }
    Ok(res)   
}
impl Day9 {
    fn find_target(&self) -> Result<u64, Box<dyn Error>> {
        for base in 0..(self.input.len() - WINDOW_SIZE - 1) {
            let next = base + WINDOW_SIZE;
            let mut found = false;
            for i in base..next {
                for j in base..next {
                    if self.input[i] + self.input[j] == self.input[next] {
                        found = true;
                    }
                }
            }
            if !found {
                return Ok(self.input[next]);
            }
        }
        Err(new_err("Not found!"))
    }
}

impl Problem for Day9{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(self.find_target()?.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let target = self.find_target()?;
        let mut start = 0;
        let mut end = 2;
        let mut partial = self.input[0] + self.input[1];
        while partial != target {
            if start == end - 2 || partial < target {
                if end >= self.input.len() {
                    return Err(new_err("Not found!"));
                }
                partial += self.input[end];
                end += 1;
            }
            else {
                partial -= self.input[start];
                start += 1;
            }
        }
        let min = self.input[start..end].iter().min().unwrap();
        let max = self.input[start..end].iter().max().unwrap();
        Ok((min + max).to_string())
    }
}
