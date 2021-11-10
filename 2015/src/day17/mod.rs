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
use std::collections::{HashSet, HashMap};
use permutator::k_permutation;



pub struct Day17{
    cups: Vec<i64>,
}

pub fn setup(_input:&str) -> Result<Day17, Box<dyn Error>>{
    let mut res = Day17{
        cups: Vec::new(),
    };
    for line in read_lines(_input){
        let l = line?;
        res.cups.push(i64::from_str_radix(l.trim(), 10)?);
    }
    Ok(res)
}

fn combos(cups: &[i64], rem: i64) -> i64 {
    if cups.len() == 1{
        if rem == 0 || cups[0] == rem{
            return 1;
        }
        else{
            return 0;
        }
    }
    let mut total = combos(&cups[1..], rem);
    if cups[0] <= rem {
        let c = combos(&cups[1..], rem - cups[0]);
        total += c;
    }
    total

}fn combos2(cups: &[i64], rem: i64, used: i64) -> (i64, i64) {
    //println!("rem {}, used {}, {:?}", rem, used, cups);
    if cups.len() == 1{
        if rem == 0 {
            return (used, 1);
        }
        else if rem == cups[0]{
            return (used + 1, 1);
        }
        else{
            return (999999, 0);
        }
    }
    let (min_used_without, num_at_min_without) = combos2(&cups[1..], rem, used);
    if cups[0] > rem {
        return (min_used_without, num_at_min_without);
    }
    let (min_used_with, num_at_min_with) = combos2(&cups[1..], rem - cups[0], used + 1);
    if min_used_without < min_used_with {
        return (min_used_without, num_at_min_without);
    }
    else if min_used_without > min_used_with {
        return (min_used_with, num_at_min_with);
    }
    else{
        return (min_used_with, num_at_min_with + num_at_min_without);
    }
}

impl Problem for Day17{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(combos(&self.cups, 150).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(combos2(&self.cups, 150, 0).1.to_string())
    }
}