use crate::Problem;
use crate::read_lines;
use std::vec::Vec;
use std::error::Error;
use string_error::new_err;
use regex::Regex;
use std::collections::HashMap;
use lazy_static::lazy_static;

pub struct Day4{
    pps: Vec<HashMap<String, String>>,
}

pub fn setup(input:&str) -> Result<Day4, Box<dyn Error>>{
    let mut res = Day4{
        pps: Vec::new(),
    };
    let re = Regex::new(r"([^: ]+):([^: ]+)")?;
    let mut curr_map = HashMap::new();
    for line_opt in read_lines(input){
        let line = line_opt.unwrap();
        let l = line.trim();
        if l.len() == 0 {
            res.pps.push(curr_map);
            curr_map = HashMap::new();
        }
        else{
            for cap in re.captures_iter(&l){
                curr_map.insert(cap[1].to_string(), cap[2].to_string());
            }
        }
    }
    if curr_map.len() != 0 {
        res.pps.push(curr_map);
    }
    Ok(res)
}

fn validate_yr(inp: &str, lower: u32, upper: u32) -> bool {
    if let Ok(yr) = inp.parse::<u32>() {
        return yr >= lower && yr <= upper;
    }
    false
}

fn validate_hgt(inp: &str) -> bool {
    if inp.len() < 2 {
        return false;
    }
    if &inp[inp.len()-2..] == "in" {
        if let Ok(i) = inp[..inp.len()-2].parse::<u32>() {
            return i >= 59 && i <= 76;
        }
        return false;
    }
    else if &inp[inp.len()-2..] == "cm" {
        if let Ok(i) = inp[..inp.len()-2].parse::<u32>() {
            return i >= 150 && i <= 193;
        }
        return false;
    }
    false
}

fn validate_hcl(inp: &str) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new("^#[0-9a-f]{6}$").unwrap();
    }
    RE.is_match(inp)
}

fn validate_ecl(inp: &str) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new("^(amb|blu|brn|gry|grn|hzl|oth)$").unwrap();
    }
    RE.is_match(inp)
}

fn validate_pid(inp: &str) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^\d{9}$").unwrap();
    }
    RE.is_match(inp)
}

fn all_keys(map: &HashMap<String, String>) -> bool {
    let keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
    for k in &keys {
        if !map.contains_key(*k) {
            return false;
        }
    }
    true
}

fn validate(map: &HashMap<String, String>) -> bool {
    let mut res = true;
    for (k, v) in map {
        res &= match k.as_str() {
            "byr" => validate_yr(v, 1920, 2002),
            "iyr" => validate_yr(v, 2010, 2020),
            "eyr" => validate_yr(v, 2020, 2030),
            "hgt" => validate_hgt(v),
            "hcl" => validate_hcl(v),
            "ecl" => validate_ecl(v),
            "pid" => validate_pid(v),
            "cid" => true,
            _ => false
        }
    }
    res && all_keys(map)
}

impl Problem for Day4{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut n = 0;
        for map in &self.pps {
            if all_keys(map){
                n += 1;
            }
        }
        Ok(n.to_string())
    }

    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut n = 0;
        for map in &self.pps {
            if validate(map) {
                n += 1;
            }
        }
        Ok(n.to_string())
    }
}
