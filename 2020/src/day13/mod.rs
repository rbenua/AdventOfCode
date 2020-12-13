use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::cmp::Reverse;

#[derive(Debug)]
pub struct Day13{
    goal: u64,
    lines: Vec<Option<u64>>,
}

pub fn setup(_input:&str) -> Result<Day13, Box<dyn Error>>{
    let mut res = Day13{
        goal: 0,
        lines: Vec::new(),
    };
    let mut lines = read_lines(_input);
    let first = lines.next().unwrap()?;
    res.goal = first.trim().parse()?;
    let second = lines.next().unwrap()?;
    for num in second.trim().split(',') {
        if num != "x" {
            res.lines.push(Some(num.parse()?));
        }
        else{
            res.lines.push(None);
        }
    }
    Ok(res)   
}

impl Problem for Day13{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let (min_time, min_id) = self.lines.iter().map(|opt|{match opt{
            Some(x) => (((self.goal - 1) / x * x + x) - self.goal, *x),
            None => (9999999, 9999999)
        }}).min().unwrap();
        Ok((min_time * min_id).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut pairs: Vec<(u64, u64)> = Vec::new();
        for (i, line) in self.lines.iter().enumerate() {
            if let Some(x) = line {
                pairs.push((*x, i as u64));
            }
        }
        pairs.sort_by_key(|w| Reverse(*w));

        let (mut interval, i) = pairs.remove(0);
        let mut base = interval - i;
        while pairs.len() > 0 {
            let (target, ti) = pairs.remove(0);
            while (base + ti) % target != 0 {
                base += interval;
            }
            interval *= target;
        }
        Ok(base.to_string())
    }
}
