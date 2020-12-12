use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day12{
    insns: Vec<Insn>,
}

#[derive(Display, FromStr, PartialEq, Debug, Clone, Copy)]
enum Command {
    #[display("N")]
    North,
    #[display("E")]
    East,
    #[display("S")]
    South,
    #[display("W")]
    West,
    #[display("L")]
    Left,
    #[display("R")]
    Right,
    #[display("F")]
    Forward,
}
use Command::*;

#[derive(Display, FromStr, PartialEq, Debug, Clone, Copy)]
#[display("{cmd}{amt}")]
#[from_str(regex = r"(?P<cmd>\w)(?P<amt>\d+)")]
struct Insn {
    cmd: Command,
    amt: i64,
}

#[derive(Display, FromStr, PartialEq, Debug, Clone, Copy)]
#[display("({x}, {y}) bearing {dir}")]
struct State1 {
    x: i64,
    y: i64,
    dir: i64,
}

fn step1(cur: State1, i: &Insn) -> State1 {
    let mut res = cur;
    match i.cmd {
        North => res.y += i.amt,
        South => res.y -= i.amt,
        East => res.x += i.amt,
        West => res.x -= i.amt,
        Left => res.dir = (res.dir - i.amt).rem_euclid(360),
        Right => res.dir = (res.dir + i.amt).rem_euclid(360),
        Forward => match res.dir {
            0 => res.y += i.amt,
            90 => res.x += i.amt,
            180 => res.y -= i.amt,
            270 => res.x -= i.amt,
            n => panic!("bad direction {}", n),
        },
    };
    res
}

#[derive(Display, FromStr, PartialEq, Debug, Clone, Copy)]
#[display("({x}, {y}) waypoint ({wpx}, {wpy})")]
struct State2 {
    x: i64,
    y: i64,
    wpx: i64,
    wpy: i64,
}

fn rotate(x: i64, y: i64, amt: i64) -> (i64, i64) {
    match amt {
        0 => (x, y),
        90 => (y, -x),
        180 => (-x, -y),
        270 => (-y, x),
        n => panic!("bad angle {}", n),
    }
}

fn step2(cur: State2, i: &Insn) -> State2 {
    let mut res = cur;
    match i.cmd {
        North => res.wpy += i.amt,
        South => res.wpy -= i.amt,
        East => res.wpx += i.amt,
        West => res.wpx -= i.amt,
        Left => (res.wpx, res.wpy) = rotate(res.wpx, res.wpy, 360 - i.amt),
        Right => (res.wpx, res.wpy) = rotate(res.wpx, res.wpy, i.amt),
        Forward => {
            res.x += res.wpx * i.amt;
            res.y += res.wpy * i.amt;
        },
    };
    res
}

pub fn setup(_input:&str) -> Result<Day12, Box<dyn Error>>{
    let mut res = Day12{
        insns: Vec::new(),
    };
    for line in read_lines(_input) {
        res.insns.push(line?.trim().parse()?);
    }
    Ok(res)
}

impl Problem for Day12{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut cur = State1 {
            x: 0,
            y: 0,
            dir: 90,
        };
        for i in &self.insns {
            cur = step1(cur, i);
        }
        Ok((cur.x.abs() + cur.y.abs()).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut cur = State2 {
            x: 0,
            y: 0,
            wpx: 10,
            wpy: 1,
        };
        for i in &self.insns {
            cur = step2(cur, i);
        }
        Ok((cur.x.abs() + cur.y.abs()).to_string())
    }
}
