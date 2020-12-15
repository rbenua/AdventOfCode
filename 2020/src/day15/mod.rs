use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashMap;

pub struct Day15{
    start: Vec<u64>,
}

pub fn setup(_input:&str) -> Result<Day15, Box<dyn Error>>{
    let mut res = Day15{
        start: Vec::new(),
    };
    for line in read_lines(_input) {
        for c in line?.split(',') {
            res.start.push(c.parse()?);
        }
    }
    Ok(res)
}

impl Day15 {
    fn run(&mut self, _input:&str, goal: u64) -> Result<String, Box<dyn Error>>{
        let mut lasts: HashMap<u64, u64> = HashMap::new();
        let mut last_prev = 0;
        let mut index = 1;
        let mut ans = 0;
        for si in &self.start {
            lasts.insert(*si, index);
            //println!("{:>4}: Say {} (starting)", index, *si);
            last_prev = index;
            index += 1;
        }
        while index <= goal {
            let to_say = index - 1 - last_prev;
            if index == goal {
                ans = to_say;
            }
            match lasts.insert(to_say, index) {
                Some(prev) => last_prev = prev,
                None => last_prev = index,
            };
            index += 1;
        }
        Ok(ans.to_string())
    }
}

impl Problem for Day15{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        self.run(_input, 2020)
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        self.run(_input, 30000000)
    }
}
