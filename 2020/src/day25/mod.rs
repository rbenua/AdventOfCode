use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day25{
    card_pub: u64,
    door_pub: u64,
}

pub fn setup(_input:&str) -> Result<Day25, Box<dyn Error>>{
    let mut lines = read_lines(_input);
    let card_pub: u64 = lines.next().unwrap()?.trim().parse()?;
    let door_pub: u64 = lines.next().unwrap()?.trim().parse()?;
    let mut res = Day25{
        card_pub: card_pub,
        door_pub: door_pub,
    };
    Ok(res)   
}

fn find_loops(pubkey: u64, subj: u64) -> u64 {
    let mut curr = 1;
    for i in 1.. {
        curr = (curr * subj) % 20201227;
        if curr == pubkey {
            return i;
        }
    }
    0
}    

fn run_loops(loop_count: u64, subj: u64) -> u64 {
    let mut curr = 1;
    for _ in 0..loop_count {
        curr = (curr * subj) % 20201227;
    }
    curr
}


impl Problem for Day25{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let card_loops = find_loops(self.card_pub, 7);
        let door_loops = find_loops(self.door_pub, 7);
        println!("found card loops: {}, door loops: {}", card_loops, door_loops);
        Ok(run_loops(card_loops, self.door_pub).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Err(new_err("not enough stars"))
    }
}
