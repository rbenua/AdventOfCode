use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day16{
    bits: Vec<bool>,
    parsed: Packet,
}

#[derive(Debug)]
struct Packet{
    version: u64,
    ptype: u64,
    contents: Contents,
}

#[derive(Debug)]
enum Contents{
    Int(u64),
    Oper(Vec<Packet>),
}

pub fn setup(_input:&str) -> Result<Day16, Box<dyn Error>>{
    let line = read_lines(_input).next().unwrap().unwrap();
    let mut bits = Vec::new();
    for c in line.trim().chars() {
        let i = c.to_digit(16).unwrap();
        for d in (0..4).rev() {
            bits.push((i & (1 << d)) > 0);
        }
    }
    Ok(Day16{parsed: parse(&bits).0, bits})
}

fn to_int(bits: &[bool]) -> u64 {
    let mut res = 0;
    for b in bits {
        res <<= 1;
        if *b {
            res += 1;
        }
    }
    res
}

fn parse(bits: &[bool]) -> (Packet, usize) {
    let version = to_int(&bits[0..3]);
    let ptype = to_int(&bits[3..6]);
    let mut consumed = 6;
    let contents = if ptype == 4 {
        let mut val = 0;
        while bits[consumed] {
            val <<= 4;
            val += to_int(&bits[consumed+1..consumed+5]);
            consumed += 5;
        }
        val <<= 4;
        val += to_int(&bits[consumed+1..consumed+5]);
        consumed += 5;
        Contents::Int(val)
    }
    else {
        let mut res = Vec::new();
        if !bits[6] {
            let rem_length = to_int(&bits[7..22]) as usize;
            let mut start = 22;
            let end = 22 + rem_length;
            consumed = end;
            while start < end {
                let (t, c) = parse(&bits[start..end]);
                res.push(t);
                start += c;
            }
        }
        else {
            let rem_packets = to_int(&bits[7..18]);
            let mut start = 18;
            for _ in 0..rem_packets {
                let (t, c) = parse(&bits[start..]);
                res.push(t);
                start += c;
            }
            consumed = start;
        }
        Contents::Oper(res)
    };
    (Packet{version, ptype, contents}, consumed)
}

fn sum_versions(packet: &Packet) -> u64 {
    packet.version + match &packet.contents {
        Contents::Int(_) => 0,
        Contents::Oper(v) => v.iter().map(sum_versions).sum(),
    }
}

fn eval(packet: &Packet) -> u64 {
    match &packet.contents {
        Contents::Int(x) => *x,
        Contents::Oper(v) => eval_subpackets(packet.ptype, &mut v.iter().map(eval))
    }
}

fn eval_subpackets(ptype: u64, subpackets: &mut impl Iterator<Item=u64>) -> u64 {
    match ptype {
        0 => subpackets.sum(),
        1 => subpackets.product(),
        2 => subpackets.min().unwrap(),
        3 => subpackets.max().unwrap(),
        5 => if subpackets.next().unwrap() > subpackets.next().unwrap() {1} else {0},
        6 => if subpackets.next().unwrap() < subpackets.next().unwrap() {1} else {0},
        7 => if subpackets.next().unwrap() == subpackets.next().unwrap() {1} else {0},
        _ => panic!("bad packet type {}", ptype),
    }
}

impl Problem for Day16{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(sum_versions(&self.parsed).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(eval(&self.parsed).to_string())
    }
}
