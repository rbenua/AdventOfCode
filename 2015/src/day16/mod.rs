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



pub struct Day16{
    aunts: Vec<HashMap<String, i64>>,
    letter: HashMap<String, i64>,
}

pub fn setup(_input:&str) -> Result<Day16, Box<dyn Error>>{
    let re = Regex::new(r"(\w+): (\d+)")?;
    let mut res = Day16{
        aunts: Vec::new(),
        letter: vec![("children".to_string(), 3),
(String::from("cats"), 7),
(String::from("samoyeds"), 2),
(String::from("pomeranians"), 3),
(String::from("akitas"), 0),
(String::from("vizslas"), 0),
(String::from("goldfish"), 5),
(String::from("trees"), 3),
(String::from("cars"), 2),
(String::from("perfumes"), 1)].into_iter().collect(),
    };
    for line in read_lines(_input){
        let l = line?;
        let css = re.captures_iter(&l);
        let mut m = HashMap::new();
        for cs in css{
            m.insert(cs[1].to_string(), i64::from_str_radix(&cs[2], 10)?);
        }
        res.aunts.push(m);
    }
    Ok(res)
}

fn matches1(m: &HashMap<String, i64>, l: &HashMap<String, i64>) -> bool {
    for (k, v) in m.iter(){
        if l.get(k) != Some(v) {
            return false;
        }
    }
    true
}

fn matches2(m: &HashMap<String, i64>, l: &HashMap<String, i64>) -> bool {
    for (k, v) in m.iter(){
        if k == "goldfish" || k == "pomeranians" {
            match l.get(k){
                None => return false,
                Some(c) => if c <= v {
                    return false;
                },
            };
        }
        else if k == "cats" || k == "trees" {
            match l.get(k){
                None => return false,
                Some(c) => if c >= v {
                    return false;
                },
            };
        }
        else if l.get(k) != Some(v) {
            return false;
        }
    }
    true
}

impl Problem for Day16{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        for (m, i) in self.aunts.iter().zip(0..) {
            if matches1(m, &self.letter) {
                println!("{:#?}", m);
                return Ok((i+1).to_string());
            }
        }
        Err(new_err("not found"))
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        for (m, i) in self.aunts.iter().zip(0..) {
            if matches2(m, &self.letter) {
                println!("{:#?}", m);
                return Ok((i+1).to_string());
            }
        }
        Err(new_err("not found"))
    }
}
