use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashSet;

pub struct Day6{
    groups: Vec<HashSet<char>>,
    groups_int: Vec<HashSet<char>>,
}

pub fn setup(_input:&str) -> Result<Day6, Box<dyn Error>>{
    let mut res = Day6{
        groups: Vec::new(),
        groups_int: Vec::new(),
    };
    let mut curr_group = HashSet::new();
    let mut curr_int = HashSet::new();
    for line_opt in read_lines(_input) {
        let line_ws = line_opt?;
        let line = line_ws.trim();
        if line.len() == 0 {
            res.groups.push(curr_group);
            res.groups_int.push(curr_int);
            curr_group = HashSet::new();
            curr_int = HashSet::new();
        }
        else {
            let mut curr_line = HashSet::new();
            for c in line.chars() {
                curr_line.insert(c);
            }
            if curr_group.len() == 0 {
                curr_int = curr_line.clone();
            }
            curr_group = curr_group.union(&curr_line).map(|x|{*x}).collect();
            curr_int = curr_int.intersection(&curr_line).map(|x|{*x}).collect();
        }
    }
    if curr_group.len() != 0 {
        res.groups.push(curr_group);
        res.groups_int.push(curr_int);
    }
    Ok(res)   
}

impl Problem for Day6{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(self.groups.iter().map(|g|{g.len()}).sum::<usize>().to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(self.groups_int.iter().map(|g|{g.len()}).sum::<usize>().to_string())
    }
}
