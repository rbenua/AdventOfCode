use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use std::str::FromStr;

use nom::{IResult, one_of, delimited, char, alt, character::complete::digit1, fold_many0, pair, map_res, complete};

pub struct Day18{
    lines: Vec<String>,
}

pub fn setup(_input:&str) -> Result<Day18, Box<dyn Error>>{
    let mut res = Day18{ lines: Vec::new(), };
    for line in read_lines(_input) {
        res.lines.push(line?.trim().replace(" ", ""));
    }
    Ok(res)
}

fn term(input: &str) -> IResult<&str, u64> {
    alt!(input, map_res!(digit1, FromStr::from_str) | delimited!(char!('('), eval, char!(')')))
}

fn eval(input: &str) -> IResult<&str, u64> {
    let (rem, init) = term(input)?;
    fold_many0!(rem, complete!(pair!(one_of!("+*"), term)), init, |acc, (op, t)|{
        if op == '+' {
            acc + t
        }
        else {
            acc * t
        }
    })
}

fn term2(input: &str) -> IResult<&str, u64> {
    alt!(input, map_res!(digit1, FromStr::from_str) | delimited!(char!('('), eval2, char!(')')))
}

fn sum(input: &str) -> IResult<&str, u64> {
    let (rem, init) = term2(input)?;
    let res = fold_many0!(rem, complete!(pair!(char!('+'), term2)), init, |acc, (_, t)|{acc + t});
    //println!("sum   {} -> {:?}", input, res);
    res
}

fn eval2(input: &str) -> IResult<&str, u64> {
    let (rem, init) = sum(input)?;
    let res = fold_many0!(rem, complete!(pair!(char!('*'), sum)), init, |acc, (_, t)|{acc * t});
    //println!("eval2 {} -> {:?}", input, res);
    res
}


impl Problem for Day18{
    fn part1<'a>(&'a mut self, _input:&str) -> Result<String, Box<dyn Error + 'a>>{
        let mut total = 0;
        for line in &self.lines {
            let r = eval(line.as_str());
            println!("{} = {:?}", line, r);
            let (_, res) = r?;
            total += res;
        }
        Ok(total.to_string())
    }
    fn part2<'a>(&'a mut self, _input:&str) -> Result<String, Box<dyn Error + 'a>>{
        let mut total = 0;
        for line in &self.lines {
            let r = eval2(line.as_str());
            println!("{} = {:?}", line, r);
            let (_, res) = r?;
            total += res;
        }
        Ok(total.to_string())
    }
}
