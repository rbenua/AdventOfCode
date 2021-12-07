use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day7{
    positions: Vec<i64>,
}

pub fn setup(_input:&str) -> Result<Day7, Box<dyn Error>>{
    let line = read_lines(_input).next().unwrap()?;
    let positions = line.split(',').map(|x|{x.trim().parse().unwrap()}).collect();
    Ok(Day7{positions})
}

fn score1(test: i64, positions: &Vec<i64>) -> i64 {
    positions.iter().map(|p|{i64::abs(p - test)}).sum()
}

fn score2(test: i64, positions: &Vec<i64>) -> i64 {
    positions.iter().map(|p|{
        let d = i64::abs(p - test);
        (d * (d + 1)) / 2
    }).sum()
}

fn search(positions: &Vec<i64>, score: fn(i64, &Vec<i64>) -> i64) -> i64 {
    let start = *positions.iter().min().unwrap();
    let end = *positions.iter().max().unwrap();
    ((start+1)..end).map(|p|{score(p, positions)}).min().unwrap()
}

impl Problem for Day7{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(search(&self.positions, score1).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(search(&self.positions, score2).to_string())
    }
}
