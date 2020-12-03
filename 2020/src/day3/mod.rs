use crate::Problem;
use crate::read_lines;
use std::vec::Vec;
use std::error::Error;
use string_error::new_err;

pub struct Day3{
	grid: Vec<Vec<bool>>,
}

pub fn setup(input:&str) -> Result<Day3, Box<dyn Error>>{
	let mut res = Day3{
		grid: Vec::new(),
	};
	for line in read_lines(input){
        let mut row: Vec<bool> = Vec::new();
        for c in line?.trim().chars(){
            row.push(c == '#');
        }
        res.grid.push(row);
	}
	Ok(res)
}

fn count(grid: &Vec<Vec<bool>>, dx: usize, dy: usize) -> usize{
    let mut n = 0;
    let mut x = 0;
    let mut y = 0;
    while y < grid.len(){
        if grid[y][x % grid[y].len()]{
            n += 1;
        }
        y += dy;
        x += dx;
    }
    n
}

impl Problem for Day3{
	fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(count(&self.grid, 3, 1).to_string())
	}

	fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
        let mut res = 1;
        for (dx, dy) in &slopes{
            res *= count(&self.grid, *dx, *dy);
        }
        Ok(res.to_string())
	}
}
