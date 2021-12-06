use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day6{
    fish: Vec<i64>,
}

pub fn setup(_input:&str) -> Result<Day6, Box<dyn Error>>{
    let line = read_lines(_input).next().unwrap()?;
    let fish = line.split(',').map(|x|{x.trim().parse().unwrap()}).collect();
    let res = Day6{
        fish
    };
    Ok(res)
}
fn step(fish: &mut Vec<i64>) {
    let l = fish.len();
    for i in 0..l {
        if fish[i] == 0 {
            fish[i] = 6;
            fish.push(8);
        }
        else {
            fish[i] -= 1;
        }
    }
}

fn step_buckets(buckets: &[i64; 9]) -> [i64; 9] {
    let mut res = [0; 9];
    for i in 0..8 { 
        res[i] = buckets[i+1];
    }
    res[8] = buckets[0];
    res[6] += buckets[0];
    res
}

fn build_buckets(fish: &Vec<i64>) -> [i64; 9] {
    let mut res = [0; 9];
    for f in fish {
        res[*f as usize] += 1;
    }
    res
}

impl Problem for Day6{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut fish = self.fish.clone();
        for _ in 0..80 {
            step(&mut fish);
        }
        Ok(fish.len().to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut buckets = build_buckets(&self.fish);
        for _ in 0..256 {
            buckets = step_buckets(&buckets);
        }
        Ok(buckets.iter().sum::<i64>().to_string())
    }
}
