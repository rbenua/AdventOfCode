use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day10{
    lines: Vec<String>,
}

pub fn setup(_input:&str) -> Result<Day10, Box<dyn Error>>{
    Ok(Day10{
        lines: read_lines(_input).map(|x|{x.unwrap()}).collect(),
    })
}

fn find_error(line: &mut impl Iterator<Item=char>, stack: &mut Vec<char>) -> Option<char> {
    match line.next() {
        None => None,
        Some(c) => {
            if "[<({".contains(c) {
                stack.push(c);
                find_error(line, stack)
            }
            else {
                if Some(opener(c)) != stack.pop() {
                    Some(c)
                }
                else {
                    find_error(line, stack)
                }
            }
        }
    }
}

fn opener(close: char) -> char {
    match close {
        ']' => '[',
        '}' => '{',
        '>' => '<',
        ')' => '(',
        _ => panic!("bad closer {}", close)
    }
}

fn score_missing(close: char) -> i64 {
    match close {
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        ')' => 3,
        _ => panic!("bad closer {}", close)
    }
}

fn score_remainder(close: char) -> i64 {
    match close {
        '[' => 2,
        '{' => 3,
        '<' => 4,
        '(' => 1,
        _ => panic!("bad closer {}", close)
    }
}

fn score_first_error(line: &String) -> i64 {
    if let Some(close) = find_error(&mut line.chars(), &mut Vec::new()) {
        score_missing(close)
    }
    else {
        0
    }
}

fn score_all_remainder(line: &String) -> Option<i64> {
    let mut remainder = Vec::new();
    if find_error(&mut line.chars(), &mut remainder).is_some() {
        return None;
    }
    let mut total = 0;
    for closer in remainder.iter().rev() {
        total *= 5;
        total += score_remainder(*closer);
    }
    Some(total)
}

impl Problem for Day10{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let total: i64 = self.lines.iter().map(|line|{score_first_error(line)}).sum();
        Ok(total.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut scores: Vec<i64> = self.lines.iter().filter_map(|line|{score_all_remainder(line)}).collect();
        //println!("{:?}", scores);
        scores.sort();
        let m = scores.len() / 2;
        Ok(scores[m].to_string())
    }
}
