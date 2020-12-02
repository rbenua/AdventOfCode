#![allow(unused_imports)]
use std::env;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
use std::error::Error;

pub mod day1;
pub mod day2;

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
		_ => panic!("nonexistent day"),
	};
    println!("{}", d.part1(&args[2])?);
    println!("{}", d.part2(&args[2])?);
    Ok(())
}
