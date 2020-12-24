use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::VecDeque;

pub struct Day23{
    cups: VecDeque<i64>,
}

pub fn setup(_input:&str) -> Result<Day23, Box<dyn Error>>{
    let mut res = Day23{
        cups: VecDeque::new(),
    };
    for line in read_lines(_input) {
        res.cups = line?.trim().chars().map(|c|c.to_digit(10).unwrap() as i64).collect();
    }

    Ok(res)   
}

fn step(cups: &mut VecDeque<i64>) {
    let num_cups = cups.len() as i64;
    let orig_cup = cups[0];
    cups.rotate_left(1);
    let picked_up = [cups.pop_front().unwrap(), cups.pop_front().unwrap(), cups.pop_front().unwrap()];
    let mut target_cup = (orig_cup - 2).rem_euclid(num_cups) + 1;
    while picked_up.contains(&target_cup) {
        target_cup = (target_cup - 2).rem_euclid(num_cups) + 1;
    }
    //println!("target: {}", target_cup);
    let target_idx = cups.iter().enumerate().find(|(_,c)|**c == target_cup).unwrap().0 + 1;
    cups.insert(target_idx, picked_up[2]);
    cups.insert(target_idx, picked_up[1]);
    cups.insert(target_idx, picked_up[0]);
}

impl Problem for Day23{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut cups = self.cups.clone();
        //println!("{:?}", cups);
        for _ in 0..100 {
            step(&mut cups);
            //println!("{:?}", cups);
        }
        let start_idx = cups.iter().enumerate().find(|(_,c)|**c == 1).unwrap().0;
        cups.rotate_left(start_idx);
        cups.pop_front();
        let res: String = cups.iter().map(|d|char::from_digit(*d as u32, 10).unwrap()).collect();
        //println!("{:?}", cups);
        Ok(res)
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut cups = self.cups.clone();
        for i in (cups.len()+1)..=1000000 {
            cups.push_back(i as i64);
        }
        for i in 0..10000000 {
            if i % 10000 == 0 {
                println!("{}", i);
            }
            step(&mut cups);
        }
        let start_idx = cups.iter().enumerate().find(|(_,c)|**c == 1).unwrap().0;
        cups.rotate_left(start_idx);
        Ok((cups[1]*cups[2]).to_string())
    }
}
