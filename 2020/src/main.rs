#![allow(unused_imports)]
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
		_ => panic!("nonexistent day"),
	};
    println!("Part 1: {}", d.part1(&args[2])?);
    println!("Part 2: {}", d.part2(&args[2])?);
    Ok(())
}
