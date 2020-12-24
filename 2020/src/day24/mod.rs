use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::{HashSet, HashMap};

pub struct Day24{
    lines: Vec<String>,
    black: HashSet<(i64, i64)>,
}

pub fn setup(_input:&str) -> Result<Day24, Box<dyn Error>>{
    let mut res = Day24{
        lines: Vec::new(),
        black: HashSet::new(),
    };
    for line in read_lines(_input) {
        res.lines.push(line?.trim().to_string());
    }
    for line in &res.lines {
        let c = coords(line);
        if !res.black.remove(&c) {
            res.black.insert(c);
        }
    }
    Ok(res)   
}

fn coords(line: &str) -> (i64, i64) {
    let mut x = 0;
    let mut y = 0;
    for dir in line.split_inclusive(&['e','w'][..]) {
        match dir {
            "e" => x += 1,
            "w" => x -= 1,
            "ne" => {
                x += 1;
                y += 1;
            },
            "se" => y -= 1,
            "nw" => y += 1,
            "sw" => {
                x -= 1;
                y -= 1;
            },
            err => panic!("bad direction {}", err),
        };
    }
    (x, y)
}

fn nbrs(c: (i64, i64)) -> [(i64, i64); 6] {
    let (x, y) = c;
    [(x - 1, y),
     (x + 1, y),
     (x, y - 1),
     (x, y + 1),
     (x - 1, y - 1),
     (x + 1, y + 1)]
}

fn step(black: &HashSet<(i64, i64)>) -> HashSet<(i64, i64)> {
    let mut nbr_counts = HashMap::new();
    for c in black {
        let nbrs = nbrs(*c);
        for nbr in nbrs.iter() {
            *nbr_counts.entry(*nbr).or_insert(0) += 1;
        }
    }
    let mut res = HashSet::new();
    for (c, n) in nbr_counts {
        if black.contains(&c) {
            if n == 1 || n == 2 {
                res.insert(c);
            }
        }
        else {
            if n == 2 {
                res.insert(c);
            }
        }
    }
    res
}

impl Problem for Day24{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(self.black.len().to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut map = step(&self.black);
        for _ in 1..100 {
            map = step(&map);
        }
        Ok(map.len().to_string())
    }
}
