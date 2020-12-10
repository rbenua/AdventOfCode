use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day10{
    adapters: Vec<u64>,
}

pub fn setup(_input:&str) -> Result<Day10, Box<dyn Error>>{
    let mut res = Day10{
        adapters: Vec::new(),
    };
    res.adapters.push(0);
    for line in read_lines(_input) {
        res.adapters.push(line?.trim().parse()?);
    }
    Ok(res)   
}

impl Problem for Day10{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        self.adapters.sort();
        self.adapters.push(self.adapters[self.adapters.len() - 1] + 3);
        let mut ones = 0;
        let mut threes = 0;
        for i in 1..self.adapters.len() {
            match self.adapters[i] - self.adapters[i-1] {
                1 => ones += 1,
                3 => threes += 1,
                _ => (),
            };
        }
        Ok((ones * threes).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut partials: Vec<u64> = vec![0; self.adapters.len()];
        partials[self.adapters.len() - 1] = 1;
        for i in (0..partials.len() - 1).rev() {
            let mut j = i + 1;
            while j < partials.len() && self.adapters[j] - self.adapters[i] <= 3 {
                partials[i] += partials[j];
                j += 1;
            }
        }
        Ok(partials[0].to_string())
    }
}
