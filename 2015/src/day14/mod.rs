use crate::Problem;
use crate::read_lines;
use std::error::Error;
use std::fs::read;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use json::*;
use std::result::Result;
use std::collections::{HashSet, HashMap};
use permutator::k_permutation;

struct Reindeer{
    name: String,
    speed: u64,
    run_time: u64,
    rest_time: u64,
}

pub struct Day14{
    reindeer: Vec<Reindeer>,
}

pub fn setup(_input:&str) -> Result<Day14, Box<dyn Error>>{
    let re = Regex::new(r"^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")?;
    let mut res = Day14{
        reindeer: Vec::new(),
    };
    for line in read_lines(_input){
        let l = line?;
        let cs = re.captures(&l).ok_or(new_err("bad format"))?;
        let r = Reindeer{
            name: cs[1].to_string(),
            speed: u64::from_str_radix(&cs[2], 10)?,
            run_time: u64::from_str_radix(&cs[3], 10)?,
            rest_time: u64::from_str_radix(&cs[4], 10)?,
        };
        res.reindeer.push(r);
    }
    Ok(res)
}

fn position(r: &Reindeer, t: u64) -> u64{
    let cycle_time = r.run_time + r.rest_time;
    let cycle_dist = r.run_time * r.speed;
    let full_cycles = t / cycle_time;
    let rem_time = *([r.run_time, t % cycle_time].iter().min().unwrap());
    let res = full_cycles * cycle_dist + rem_time * r.speed;
    res
}

impl Problem for Day14{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let m = self.reindeer.iter().map(|r|{position(r, 2503)}).max().unwrap();
        Ok(m.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut scores: HashMap<String, u64>= HashMap::new();
        for t in 1..2504 {
            let (_pos, name) = self.reindeer.iter().map(|r|{(position(r, t), &r.name)}).max().unwrap();
            *scores.entry(name.clone()).or_insert(0) += 1;
        }
        Ok(scores.values().max().unwrap().to_string())
    }
}
