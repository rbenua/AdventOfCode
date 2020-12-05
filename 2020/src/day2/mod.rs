use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

#[derive(Display, FromStr, PartialEq, Debug)]
#[display("{lower}-{upper} {letter}: {password}")]
pub struct Password{
    lower: usize,
    upper: usize,
    letter: char,
    password: String,
}

pub struct Day2{
    rows: Vec<Password>,
}

fn valid1(p: &Password) -> bool{
    let mut n = 0;
    for c in p.password.chars(){
        if c == p.letter{
            n += 1;
        }
    }
    n >= p.lower && n <= p.upper
}

fn valid2(p: &Password) -> bool{
    let b = p.password.as_bytes();
    let l = p.lower <= b.len() && (b[p.lower - 1] as char) == p.letter;
    let u = p.upper <= b.len() && (b[p.upper - 1] as char) == p.letter;
    l ^ u
}

pub fn setup(_input:&str) -> Result<Day2, Box<dyn Error>>{
    let mut res = Day2{
        rows: Vec::new(),
    };
    for line_opt in read_lines(_input){
        res.rows.push(line_opt?.parse()?);
    }
	Ok(res)
}

impl Problem for Day2{
	fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut n = 0;
		for p in &self.rows{
            if valid1(p){
                n += 1;
            }
        }
        Ok(n.to_string())
	}
	fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut n = 0;
		for p in &self.rows{
            if valid2(p){
                n += 1;
            }
        }
        Ok(n.to_string())
	}
}
