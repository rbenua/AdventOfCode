use crate::Problem;

pub struct Day2{
	lol: i32,
	lolol: i32,
}

pub fn setup(input:&str) -> Day2{
	Day2{
		lol: 3,
		lolol: 4,
	}
}

impl Problem for Day2{
	fn part1(&mut self, _input:&str) -> String{
		return self.lol.to_string();
	}
	fn part2(&mut self, _input:&str) -> String{
		return self.lolol.to_string();
	}
}
