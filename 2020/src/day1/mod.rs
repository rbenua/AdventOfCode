use crate::Problem;
use crate::read_lines;
use std::fs;
use regex::Regex;
use std::vec::Vec;
use std::error::Error;

pub struct Day1{
	nums: Vec<i32>,
}

pub fn setup(input:&str) -> Result<Day1, Box<dyn Error>>{
	let mut res = Day1{
		nums: Vec::new(),
	};
	for line in read_lines(input){
		res.nums.push(line?.trim().parse()?)
	}
	Ok(res)
}

impl Problem for Day1{
	fn part1(&mut self, _input:&str) -> String{
		for a in &self.nums{
			for b in &self.nums{
				if a + b == 2020{
					return format!("{}", a * b);
				}
			}
		}
		return String::from("none");
	}
	fn part2(&mut self, _input:&str) -> String{
		for a in &self.nums{
			for b in &self.nums{
				for c in &self.nums{
					if a + b + c == 2020{
						return format!("{}", a * b * c);
					}
				}
			}
		}
		return String::from("none");
	}
}
