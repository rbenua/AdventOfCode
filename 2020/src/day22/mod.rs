use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashSet;

#[derive(Debug)]
pub struct Day22{
    p1: Vec<u64>,
    p2: Vec<u64>,
}

pub fn setup(_input:&str) -> Result<Day22, Box<dyn Error>>{
    let mut res = Day22{
        p1: Vec::new(),
        p2: Vec::new(),
    };
    let mut lines = read_lines(_input);
    lines.next();
    let mut line = lines.next().unwrap()?;
    while line.len() != 0 {
        res.p1.push(line.parse()?);
        line = lines.next().unwrap()?;
    }
    lines.next();
    for line_opt in lines {
        res.p2.push(line_opt?.parse()?);
    }
    println!("{:?}", res);
    Ok(res)
}

fn step1(p1: &mut Vec<u64>, p2: &mut Vec<u64>) {
    let c1 = p1.remove(0);
    let c2 = p2.remove(0);
    if c1 > c2 {
        p1.push(c1);
        p1.push(c2);
    }
    else {
        p2.push(c2);
        p2.push(c1);
    }
}

fn run2(p1: &[u64], p2: &[u64]) -> (bool, Vec<u64>) {
    let mut v1: Vec<u64> = Vec::from(p1);
    let mut v2: Vec<u64> = Vec::from(p2);
    let mut seen: HashSet<(Vec<u64>, Vec<u64>)> = HashSet::new();
    //println!("Playing a new game with p1: {:?}, p2: {:?}", v1, v2);
    while v1.len() > 0 && v2.len() > 0 { 
        if seen.contains(&(v1.clone(), v2.clone())) {
            return (true, v1);
        }
        seen.insert((v1.clone(), v2.clone()));
        
        let c1 = v1.remove(0);
        let c2 = v2.remove(0);
        let winner: bool;
        if c1 as usize <= v1.len() && c2 as usize <= v2.len() {
            (winner, _) = run2(&v1[0..c1 as usize], &v2[0..c2 as usize]);
        }
        else {
            winner = c1 > c2;
        }
        if winner {
            v1.push(c1);
            v1.push(c2);
        }
        else {
            v2.push(c2);
            v2.push(c1);
        }
    }
    if v1.len() > 0 {
        return (true, v1);
    }
    else {
        return (false, v2);
    }
}

impl Problem for Day22{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut p1 = self.p1.clone();
        let mut p2 = self.p2.clone();
        while p1.len() > 0 && p2.len() > 0 {
            step1(&mut p1, &mut p2);
        }
        let mut score: u64 = 0;
        for (i, c) in p1.iter().rev().chain(p2.iter().rev()).enumerate() {
            score += (i + 1) as u64 * c;
        }
        Ok(score.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let (_, wv) = run2(&self.p1, &self.p2);
        let mut score: u64 = 0;
        for (i, c) in wv.iter().rev().enumerate() {
            score += (i + 1) as u64 * c;
        }
        Ok(score.to_string())
    }
}
