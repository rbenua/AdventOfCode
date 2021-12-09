use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::{HashSet, HashMap};
use itertools::Itertools;
use permutator::KPermutationIterator;

#[derive(Debug)]
struct Line {
    right: Vec<String>,
    all: HashSet<String>,
}

#[derive(Debug)]
pub struct Day8{
    lines: Vec<Line>,
}

pub fn setup(_input:&str) -> Result<Day8, Box<dyn Error>>{
    let mut res = Day8{
        lines: Vec::new(),
    };
    for line_opt in read_lines(_input){
        let line = line_opt?;
        let mut sp = line.split('|');
        let left = sp.next().unwrap();
        let right = sp.next().unwrap();
        let mut rightvec = Vec::new();
        for digit in right.split_whitespace() {
            rightvec.push(digit.chars().sorted().collect::<String>());
        }
        let mut allmap: HashSet<String> = rightvec.iter().cloned().collect();
        for digit in left.split_whitespace() {
            allmap.insert(digit.chars().sorted().collect::<String>());
        }
        res.lines.push(Line{
            right: rightvec,
            all: allmap,
        });
    }
    //println!("{:?}", res);
    Ok(res)
}

fn apply_permut(s: &str, p: &[&char]) -> String {
    s.chars().map(|c|{
        let i = (c as u32) - ('a' as u32);
        p[i as usize]
    }).sorted().collect()
}

fn matches(line: &Line, p: &[&char]) -> bool {
    line.all.iter().all(|s|{
        digit_map(&apply_permut(s, p)).is_some()
    })
}

fn rhs(line: &Line, p: &[&char]) -> u64 {
    let mut total = 0;
    for s in &line.right {
        let d = digit_map(&apply_permut(s, p)).unwrap();
        total *= 10;
        total += d;
    }
    total
}

fn digit_map(s: &str) -> Option<u64> {
    match s {
        "abcefg" => Some(0),
        "cf" => Some(1),
        "acdeg" => Some(2),
        "acdfg" => Some(3),
        "bcdf" => Some(4),
        "abdfg" => Some(5),
        "abdefg" => Some(6),
        "acf" => Some(7),
        "abcdefg" => Some(8),
        "abcdfg" => Some(9),
        _ => None
    }
}

impl Problem for Day8{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let total: usize = self.lines.iter().map(|line|{
            line.right.iter().filter(|s|{
                if s.len() == 2 || s.len() == 3 || s.len() == 4 || s.len() == 7 {
                    //println!("{}", s);
                    return true;
                }
                false
            }).count()
        }).sum();
        Ok(total.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut total = 0;
        for line in &self.lines {
            for p in KPermutationIterator::new(&['a', 'b', 'c', 'd', 'e', 'f', 'g'], 7) {
                if matches(line, &p[..]) {
                    total += rhs(line, &p[..]);
                    break;
                }
            }
        }
        Ok(total.to_string())
    }
}
