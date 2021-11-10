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

pub struct Day12{
    raw: String,
    parsed: json::JsonValue,
}

pub fn setup(_input:&str) -> Result<Day12, Box<dyn Error>>{
    let s = String::from_utf8(read(_input)?)?;
    let p = parse(&s)?;
    Ok(Day12{raw: s, parsed: p})
}

fn total(p: &JsonValue) -> f64 {
    match p {
        JsonValue::Number(n) => (*n).into(),
        JsonValue::Array(_) => p.members().map(total).sum(),
        JsonValue::Object(_) => p.entries().map(|(_k, v)|{total(v)}).sum(),
        _ => 0.0,
    }
}

fn total2(p: &JsonValue) -> f64 {
    match p {
        JsonValue::Number(n) => (*n).into(),
        JsonValue::Array(_) => p.members().map(total2).sum(),
        JsonValue::Object(_) => if p.entries().find(|(_k, v)|{v.as_str() == Some("red")}).is_some() {
            0.0
        }
        else{
            p.entries().map(|(_k, v)|{total2(v)}).sum()
        },
        _ => 0.0,
    }
}

impl Problem for Day12{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(total(&self.parsed).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(total2(&self.parsed).to_string())
    }
}
