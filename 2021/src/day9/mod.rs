use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::{HashSet, VecDeque};
use itertools::Itertools;

pub struct Day9{
    heights: Vec<Vec<u32>>,
    low_points: Vec<(usize, usize)>,
}

pub fn setup(_input:&str) -> Result<Day9, Box<dyn Error>>{
    let mut res = Day9{
        heights: Vec::new(),
        low_points: Vec::new(),
    };
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        let mut row = Vec::new();
        for c in line.trim().chars() {
            row.push(c.to_digit(10).unwrap())
        }
        res.heights.push(row);
    }
    Ok(res)
}

fn nbrs(x: usize, y: usize, xmax: usize, ymax: usize) -> Vec<(usize, usize)>{
    let mut res = Vec::new();
    if x > 0 {
        res.push((x-1, y));
    }
    if x < (xmax - 1) {
        res.push((x+1, y));
    }
    if y > 0 {
        res.push((x, y-1));
    }
    if y < (ymax - 1) {
        res.push((x, y+1));
    }
    res
}

fn low_risk(x: usize, y: usize, xmax: usize, ymax: usize, heights: &Vec<Vec<u32>>) -> Option<u32> {
    if nbrs(x, y, xmax, ymax).iter().all(|(xx,yy)|{heights[*yy][*xx] > heights[y][x]}) {
        Some(heights[y][x] + 1)
    }
    else{
        None
    }
}

fn bfs(startx: usize, starty: usize, xmax: usize, ymax: usize, heights: &Vec<Vec<u32>>) -> usize {
    let mut visited = HashSet::new();
    let mut to_visit = VecDeque::new();
    to_visit.push_back((startx, starty));
    while !to_visit.is_empty() {
        let (cur_x, cur_y) = to_visit.pop_front().unwrap();
        visited.insert((cur_x, cur_y));
        for (nbr_x, nbr_y) in nbrs(cur_x, cur_y, xmax, ymax) {
            if heights[nbr_y][nbr_x] < 9 && !visited.contains(&(nbr_x, nbr_y)) {
                to_visit.push_back((nbr_x, nbr_y));
            }
        }
    }
    visited.len()
}

impl Problem for Day9{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let xmax = self.heights[0].len();
        let ymax = self.heights.len();
        let mut total = 0;
        for y in 0..ymax {
            for x in 0..xmax {
                if let Some(risk) = low_risk(x, y, xmax, ymax, &self.heights) {
                    //println!("low point {} {}, {}", x, y, risk);
                    total += risk;
                    self.low_points.push((x, y));
                }
            }
        }
        Ok(total.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let xmax = self.heights[0].len();
        let ymax = self.heights.len();
        let mut region_sizes = self.low_points.iter().map(|&(x,y)|{
            bfs(x, y, xmax, ymax, &self.heights)
        }).collect::<Vec<usize>>();
        region_sizes.sort();
        let nreg = region_sizes.len();
        let res = region_sizes[nreg-1] * region_sizes[nreg-2] * region_sizes[nreg - 3];
        Ok(res.to_string())
    }
}
