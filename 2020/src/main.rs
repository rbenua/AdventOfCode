#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(dead_code)]
use std::env;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
use std::error::Error;

pub mod day1;
pub mod day2;
pub mod day3;
pub mod day4;
pub mod day5;
pub mod day6;
pub mod day7;
pub mod day8;
pub mod day9;
pub mod day10;
pub mod day11;
pub mod day12;
pub mod day13;
pub mod day14;
pub mod day15;
pub mod day16;
pub mod day17;
pub mod day18;
pub mod day19;
pub mod day20;
pub mod day21;
pub mod day22;
pub mod day23;
pub mod day24;
pub mod day25;

pub trait Problem {
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>;
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>;
}

pub fn read_lines<P>(filename: P) -> io::Lines<io::BufReader<File>>
where P: AsRef<Path>, {
    let file = File::open(filename).unwrap();
    io::BufReader::new(file).lines()
}

fn main() -> Result<(), Box<dyn Error>>{
    let args: Vec<String> = env::args().collect();
    let mut d : Box<dyn Problem> = match &*args[1]{
        "day1" => Box::new(day1::setup(&args[2])?),
        "day2" => Box::new(day2::setup(&args[2])?),
        "day3" => Box::new(day3::setup(&args[2])?),
        "day4" => Box::new(day4::setup(&args[2])?),
        "day5" => Box::new(day5::setup(&args[2])?),
        "day6" => Box::new(day6::setup(&args[2])?),
        "day7" => Box::new(day7::setup(&args[2])?),
        "day8" => Box::new(day8::setup(&args[2])?),
        "day9" => Box::new(day9::setup(&args[2])?),
        "day10" => Box::new(day10::setup(&args[2])?),
        "day11" => Box::new(day11::setup(&args[2])?),
        "day12" => Box::new(day12::setup(&args[2])?),
        "day13" => Box::new(day13::setup(&args[2])?),
        "day14" => Box::new(day14::setup(&args[2])?),
        "day15" => Box::new(day15::setup(&args[2])?),
        "day16" => Box::new(day16::setup(&args[2])?),
        "day17" => Box::new(day17::setup(&args[2])?),
        "day18" => Box::new(day18::setup(&args[2])?),
        "day19" => Box::new(day19::setup(&args[2])?),
        "day20" => Box::new(day20::setup(&args[2])?),
        "day21" => Box::new(day21::setup(&args[2])?),
        "day22" => Box::new(day22::setup(&args[2])?),
        "day23" => Box::new(day23::setup(&args[2])?),
        "day24" => Box::new(day24::setup(&args[2])?),
        "day25" => Box::new(day25::setup(&args[2])?),
        _ => panic!("nonexistent day"),
    };
    println!("Part 1: {}", d.part1(&args[2])?);
    println!("Part 2: {}", d.part2(&args[2])?);
    Ok(())
}
