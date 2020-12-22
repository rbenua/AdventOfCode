use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

#[derive(Debug, Display, FromStr, Copy, Clone)]
enum Rhs {
    #[display("\"{0}\"")]
    Atom(char),
    #[display("{0}")]
    Single(usize),
    #[display("{0} {1}")]
    Conc2(usize, usize),
    #[display("{0} {1} {2}")]
    Conc3(usize, usize, usize),
    #[display("{0} | {1}")]
    Or2(usize, usize),
    #[display("{0} {1} | {2} {3}")]
    OrConc4(usize, usize, usize, usize),
}
use Rhs::*;

#[derive(Debug, Display, FromStr, Copy, Clone)]
#[display("{id}: {rhs}")]
struct Line {
    id: usize,
    rhs: Rhs,
}

pub struct Day19{
    rules: Vec<Rhs>,
    lines: Vec<String>,
}

pub fn setup(_input:&str) -> Result<Day19, Box<dyn Error>>{
    let mut res = Day19{
        rules: Vec::new(),
        lines: Vec::new(),
    };
    let mut in_lines = read_lines(_input);
    let mut in_line = in_lines.next().unwrap()?;
    while in_line.len() > 0 {
        let l: Line = in_line.parse()?;
        in_line = in_lines.next().unwrap()?;
        if res.rules.len() <= l.id {
            res.rules.resize(l.id + 1, Atom('X'));
        }
        res.rules[l.id] = l.rhs;
    }
    
    for line in in_lines {
        res.lines.push(line?.to_string());
    }
    Ok(res)   
}

impl Day19 {
    fn build_regex(&self, id: usize, memo: &mut Vec<Option<String>>) -> String {
        if let Some(res) = &memo[id] {
            return res.to_string();
        }
        let res = match self.rules[id] {
            Atom(c) => c.to_string(),
            Single(r) => self.build_regex(r, memo),
            Conc2(r1, r2) => format!("({})({})", self.build_regex(r1, memo), self.build_regex(r2, memo)),
            Conc3(r1, r2, r3) => format!("({})({})({})", self.build_regex(r1, memo), self.build_regex(r2, memo), self.build_regex(r3, memo)),
            Or2(r1, r2) => format!("{}|{}", self.build_regex(r1, memo), self.build_regex(r2, memo)),
            OrConc4(r1, r2, r3, r4) => format!("({})({})|({})({})", self.build_regex(r1, memo), self.build_regex(r2, memo), self.build_regex(r3, memo), self.build_regex(r4, memo)),
        };
        memo[id] = Some(res.clone());
        res
    }
}

impl Problem for Day19{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut memo: Vec<Option<String>> = vec![None; self.rules.len()];
        let s = self.build_regex(0, &mut memo);
        //println!("{}", s);
        let re = Regex::new(&format!("^{}$", s))?;
        let count = self.lines.iter().filter(|s|re.is_match(s)).count();
        Ok(count.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut memo: Vec<Option<String>> = vec![None; self.rules.len()];
        let s42 = self.build_regex(42, &mut memo);
        //println!("{}", s42);
        let s31 = self.build_regex(31, &mut memo);
        //println!("{}", s31);
        let re42 = Regex::new(&s42)?;
        let re31 = Regex::new(&s31)?;
        let re = Regex::new(&format!("^(?P<ft>({})+)(?P<to>({})+)$", s42, s31))?;

        let mut count = 0;
        for line in &self.lines {
            if let Some(caps) = re.captures(line) {
                let c42 = re42.find_iter(&caps["ft"]).count();
                let c31 = re31.find_iter(&caps["to"]).count();
                if c42 > c31 {
                    //println!("{} {}|{} ({}, {})", line, &caps["ft"], &caps["to"], c42, c31);
                    count += 1;
                }
            }
        }
        Ok(count.to_string())
    }
}
