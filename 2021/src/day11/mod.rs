use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashSet;

pub struct Day11{
    levels: Vec<Vec<u32>>,
    xmax: usize,
    ymax: usize,
}

pub fn setup(_input:&str) -> Result<Day11, Box<dyn Error>>{
    let mut levels: Vec<Vec<u32>> = Vec::new();
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        let row = line.trim().chars().filter_map(|c|{c.to_digit(10)}).collect();
        levels.push(row);
    }
    let res = Day11{
        xmax: levels[0].len(),
        ymax: levels.len(),
        levels,
    };
    Ok(res)
}

fn nbrs(x: usize, y: usize, xmax: usize, ymax: usize) -> Vec<(usize, usize)> {
    let xi = x as i64;
    let yi = y as i64;
    let xmi = xmax as i64;
    let ymi = ymax as i64;
    let all = [(xi-1, yi-1), (xi, yi-1), (xi+1, yi-1),
               (xi-1, yi),               (xi+1, yi),
               (xi-1, yi+1), (xi, yi+1), (xi+1, yi+1)];
    all.iter().filter(move |(xx, yy)|{*xx >= 0 && *xx < xmi && *yy >= 0 && *yy < ymi}).map(move |(xx,yy)|{(*xx as usize, *yy as usize)}).collect()
}

fn increment(levels: &mut Vec<Vec<u32>>, xmax: usize, ymax: usize) -> Vec<(usize, usize)>{
    let mut todo = Vec::new();
    for y in 0..ymax {
        for x in 0..xmax {
            levels[y][x] += 1;     
            if levels[y][x] > 9 {
                todo.push((x, y));
            }
        }
    }
    todo
}

fn flash(levels: &mut Vec<Vec<u32>>, todo: &mut Vec<(usize, usize)>, xmax: usize, ymax: usize) -> usize {
    let mut flashed = HashSet::new();
    while !todo.is_empty() {
        let (cx, cy) = todo.pop().unwrap();
        if flashed.contains(&(cx, cy)) {
            continue;
        }
        flashed.insert((cx, cy));
        for (nx, ny) in nbrs(cx, cy, xmax, ymax) {
            levels[ny][nx] += 1;
            if levels[ny][nx] > 9 && !flashed.contains(&(nx, ny)) {
                todo.push((nx, ny));
            }
        }
    }
    for (x, y) in &flashed {
        levels[*y][*x] = 0;
    }
    flashed.len()
}

fn step(levels: &mut Vec<Vec<u32>>, xmax: usize, ymax: usize) -> usize {
    let mut todo = increment(levels, xmax, ymax);
    flash(levels, &mut todo, xmax, ymax)
}

impl Problem for Day11{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut levels = self.levels.clone();
        let total: usize = (0..100).map(|_|{step(&mut levels, self.xmax, self.ymax)}).sum();
        Ok(total.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut steps: i64 = 0;
        let mut levels = self.levels.clone();
        while step(&mut levels, self.xmax, self.ymax) != self.xmax * self.ymax {
            steps += 1;
        }
        Ok((steps+1).to_string())
    }
}
