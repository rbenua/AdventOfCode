use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day11{
    starting_pw: Vec<char>,
}

pub fn setup(_input:&str) -> Result<Day11, Box<dyn Error>>{
    Ok(Day11{starting_pw: _input.chars().collect::<Vec<char>>()})
}

fn increment(pw: &mut [char]){
    let last = pw.len() - 1;
    if pw[last] == 'z'{
        pw[last] = 'a';
        increment(&mut pw[0..last]);
    }
    else{
        pw[last] = char::from_u32((pw[last] as u32) + 1).unwrap();
        if pw[last] == 'l' || pw[last] == 'o' || pw[last] == 'i'{
            pw[last] = char::from_u32((pw[last] as u32) + 1).unwrap();
        }
    }
}

fn check(pw: &[char]) -> bool{
    let mut doubles = 0;
    let mut lastdouble: i32 = -2;
    let mut run = false;
    for i in 0..(pw.len() - 1){
        if i < pw.len() - 2 {
            let v = pw[i] as u32;
            if pw[i + 1] as u32 == v + 1 && pw[i + 2] as u32 == v + 2{
                run = true;
            }
        }
        if pw[i + 1] == pw[i] && (i as i32) > lastdouble + 1{
            lastdouble = i as i32;
            doubles += 1;
        }
    }
    run && doubles >= 2
}
fn advance(pw:&mut [char]){
    increment(pw);
    while !check(pw){
        increment(pw);
    }
}

impl Problem for Day11{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        advance(&mut self.starting_pw);
        let res = self.starting_pw.iter().collect::<String>();
        Ok(res)
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        advance(&mut self.starting_pw);
        let res = self.starting_pw.iter().collect::<String>();
        Ok(res)
    }
}
