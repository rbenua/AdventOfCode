use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashMap;

#[derive(Debug)]
struct Line{
    p1: (i64, i64),
    p2: (i64, i64),
}
pub struct Day5{
    lines: Vec<Line>
}

pub fn setup(_input:&str) -> Result<Day5, Box<dyn Error>>{
    let mut res = Day5{
        lines: Vec::new(),
    };
    let re = Regex::new(r"^(\d+),(\d+) -> (\d+),(\d+)$")?;
    for line_res in read_lines(_input) {
        let line = line_res?;
        let caps = re.captures(&line).unwrap();
        res.lines.push(Line{
            p1: (caps.get(1).unwrap().as_str().parse()?, caps.get(2).unwrap().as_str().parse()?),
            p2: (caps.get(3).unwrap().as_str().parse()?, caps.get(4).unwrap().as_str().parse()?),
        });
    }
    Ok(res)
}

fn is_ortho(line: &Line) -> bool {
    line.p1.0 == line.p2.0 || line.p1.1 == line.p2.1
}

fn walk(line:&Line) -> Box<dyn Iterator<Item=(i64, i64)>> {
    let (x1, y1) = line.p1;
    let (x2, y2) = line.p2;
    if x1 == x2 {
        if y1 < y2 {
            Box::new((y1..y2+1).map(move |y|{(x1, y)}))
        }
        else {
            Box::new((y2..y1+1).map(move |y|{(x1, y)}))
        }
    }
    else if y1 == y2 {
        if x1 < x2 {
            Box::new((x1..x2+1).map(move |x|{(x, y1)}))
        }
        else {
            Box::new((x2..x1+1).map(move |x|{(x, y1)}))
        }
    }
    else {
        if x1 < x2 && y1 < y2 {
            Box::new((0..x2-x1+1).map(move |x|{(x1 + x, y1 + x)}))
        }
        else if x1 < x2 {
            Box::new((0..x2-x1+1).map(move |x|{(x1 + x, y1 - x)}))
        }
        else if y1 < y2 {
            Box::new((0..x1-x2+1).map(move |x|{(x2 + x, y2 - x)}))
        }
        else {
            Box::new((0..x1-x2+1).map(move |x|{(x2 + x, y2 + x)}))
        }
    }
}

impl Problem for Day5{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut counts: HashMap<(i64, i64), i64> = HashMap::new();
        for line in &self.lines {
            if is_ortho(line) {
                for p in walk(line) {
                    *counts.entry(p).or_insert(0) += 1;
                }
            }
        }
        let total = counts.values().filter(|x|{**x > 1}).count();
        Ok(total.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut counts: HashMap<(i64, i64), i64> = HashMap::new();
        for line in &self.lines {
            for p in walk(line) {
                *counts.entry(p).or_insert(0) += 1;
            }
        }
        let total = counts.values().filter(|x|{**x > 1}).count();
        Ok(total.to_string())
    }
}
