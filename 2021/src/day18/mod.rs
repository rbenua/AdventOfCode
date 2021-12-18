use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

#[derive(Debug, Clone)]
enum Number {
    Int(i64),
    Pair(Box<(Number, Number)>),
}
impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(x) => write!(f, "{}", x),
            Number::Pair(b) => write!(f, "[{},{}]", b.0, b.1),
        }
    }
}

pub struct Day18{
    nums: Vec<Number>,
}

pub fn setup(_input:&str) -> Result<Day18, Box<dyn Error>>{
    let mut nums = Vec::new();
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        let parsed = parse(&line)?;
        nums.push(parsed.0);
    }
    Ok(Day18{nums})
}

fn parse(s: &str) -> Result<(Number, &str), Box<dyn Error>> {
    if let Some(rest) = s.strip_prefix('[') {
        let (first, rem) = parse(rest)?;
        if let Some(ss) = rem.strip_prefix(',') {
            let (second, sss) = parse(ss)?;
            if let Some(fin) = sss.strip_prefix(']') {
                Ok((Number::Pair(Box::new((first, second))), fin))
            }
            else {
                Err(new_err("missing ]"))
            }
        }
        else {
            Err(new_err("missing ,"))
        }
    }
    else {
        let (start, rest) = s.split_at(s.find(|c: char|!c.is_digit(10)).ok_or(new_err("bad digit"))?);
        Ok((Number::Int(start.parse()?), rest))
    }
}

fn add_left(n: &mut Number, x: i64) -> bool {
    match n {
        Number::Int(y) => {
            *n = Number::Int(*y + x);
            true
        },
        Number::Pair(b) => add_left(&mut b.0, x) || add_left(&mut b.1, x)
    }
}

fn add_right(n: &mut Number, x: i64) -> bool {
    match n {
        Number::Int(y) => {
            *n = Number::Int(*y + x);
            true
        },
        Number::Pair(b) => add_right(&mut b.1, x) || add_right(&mut b.0, x)
    }
}

fn explode(n: &mut Number, depth: i64) ->  (Option<i64>, Option<i64>, bool) {
    match n {
        Number::Int(_) => (None, None, false),
        Number::Pair(b) => if depth == 0 {
            match (&b.0, &b.1) {
                (Number::Int(left), Number::Int(right)) => {
                    let res = (Some(*left), Some(*right), true);
                    *n = Number::Int(0);
                    res
                }
                _ => panic!("too deep to explode! {}", n)
            }
        }
        else {
            let (l, mut r, f) = explode(&mut b.0, depth - 1);
            if let Some(x) = r {
                r = None;
                add_left(&mut b.1, x);
                (l, r, f)
            }
            else if f {
                (l, r, f)
            }
            else {
                let (mut l2, r2, f2) = explode(&mut b.1, depth - 1);
                if let Some(x2) = l2 {
                    l2 = None;
                    add_right(&mut b.0, x2);
                }
                (l2, r2, f2)
            }
        },
    }
}

fn split(n: &mut Number) -> bool {
    match n {
        Number::Int(x) => {
            let nx = *x;
            if nx >= 10 {
                let half = nx / 2;
                *n = Number::Pair(Box::new((Number::Int(half), Number::Int(nx - half))));
                true
            } 
            else {
                false
            }
        },
        Number::Pair(b) => split(&mut b.0) || split(&mut b.1)
    }
}

fn resolve(n: &mut Number) {
    while explode(n, 4).2 || split(n) {}
}

fn mag(n: &Number) -> i64 {
    match n {
        Number::Int(x) => *x,
        Number::Pair(b) => (mag(&b.0) * 3) + (mag(&b.1) * 2),
    }
}

fn sum(n: Number, m: Number) -> Number {
    let mut res = Number::Pair(Box::new((n, m)));
    resolve(&mut res);
    res
}

impl Problem for Day18{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut it = self.nums.iter().cloned();
        let mut total = it.next().unwrap();
        for n in it {
            total = sum(total, n);
        }
        Ok(mag(&total).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut max = i64::MIN;
        for first in 0..(self.nums.len()-1) {
            for second in (first+1)..self.nums.len() {
                let mut a = &self.nums[first];
                let mut b = &self.nums[second];
                let res1 = mag(&sum(a.clone(), b.clone()));
                let res2 = mag(&sum(b.clone(), a.clone()));
                if res1 > max {
                    max = res1;
                }
                if res2 > max {
                    max = res2;
                }
            }
        }
        Ok(max.to_string())
    }
}
