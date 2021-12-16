use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day16{
    bits: Vec<bool>,
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
    Ok(Day16{bits})
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

fn sum_versions(bits: &[bool]) -> (u64, usize) {
    //println!("parsing packet ({}): {:?}", bits.len(), bits);
    let mut total = to_int(&bits[0..3]);
    let mut ptype = to_int(&bits[3..6]);
    let mut consumed = 6;
    if ptype == 4 {
        let mut i = 6;
        while bits[i] {
            i += 5;
            consumed += 5;
        }
        consumed += 5;
        //println!("int of length {}", consumed);
    }
    else {
        if !bits[6] {
            let rem_length = to_int(&bits[7..22]) as usize;
            let mut start = 22;
            let end = 22 + rem_length;
            //println!("mode 0, remaining length {}", rem_length);
            consumed = end;
            while start < end {
                let (t, c) = sum_versions(&bits[start..end]);
                total += t;
                start += c;
            }
        }
        else {
            let rem_packets = to_int(&bits[7..18]);
            //println!("mode 1, remaining packets {}", rem_packets);
            let mut start = 18;
            for _ in 0..rem_packets {
                let (t, c) = sum_versions(&bits[start..]);
                total += t;
                start += c;
            }
            consumed = start;
        }
    }
    //println!("total {}, consumed {}", total, consumed);
    (total, consumed) 
}

fn eval_subpackets(ptype: u64, subpackets: &Vec<u64>) -> u64 {
    //println!("type {}, sub {:?}", ptype, subpackets);
    match ptype {
        0 => subpackets.iter().sum(),
        1 => subpackets.iter().product(),
        2 => *subpackets.iter().min().unwrap(),
        3 => *subpackets.iter().max().unwrap(),
        5 => if subpackets[0] > subpackets[1] {1} else {0},
        6 => if subpackets[0] < subpackets[1] {1} else {0},
        7 => if subpackets[0] == subpackets[1] {1} else {0},
        _ => panic!("bad packet type {}", ptype),
    }
}

fn eval(bits: &[bool]) -> (u64, usize) {
    //println!("parsing packet ({})", bits.len());
    let mut total = 0;
    let mut ptype = to_int(&bits[3..6]);
    let mut consumed = 6;
    if ptype == 4 {
        let mut i = 6;
        while bits[i] {
            total = total << 4;
            total += to_int(&bits[i+1..i+5]);
            //println!("{}", total);
            i += 5;
            consumed += 5;
        }
        total = total << 4;
        total += to_int(&bits[i+1..i+5]);
        consumed += 5;
        //println!("int of length {}", consumed);
    }
    else {
        if !bits[6] {
            let rem_length = to_int(&bits[7..22]) as usize;
            let mut start = 22;
            let end = 22 + rem_length;
            //println!("mode 0, remaining length {}", rem_length);
            consumed = end;
            let mut sub_results = Vec::new();
            while start < end {
                let (t, c) = eval(&bits[start..end]);
                sub_results.push(t);
                start += c;
            }
            total = eval_subpackets(ptype, &sub_results);
        }
        else {
            let rem_packets = to_int(&bits[7..18]);
            //println!("mode 1, remaining packets {}", rem_packets);
            let mut start = 18;
            let mut sub_results = Vec::new();
            for _ in 0..rem_packets {
                let (t, c) = eval(&bits[start..]);
                sub_results.push(t);
                start += c;
            }
            total = eval_subpackets(ptype, &sub_results);
            consumed = start;
        }
    }
    //println!("total {}, consumed {}", total, consumed);
    (total, consumed) 
}

impl Problem for Day16{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(sum_versions(&self.bits).0.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(eval(&self.bits).0.to_string())
    }
}
