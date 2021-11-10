use crate::Problem;
use crate::read_lines;
use std::error::Error;
use std::fs::read;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use json::*;
use std::result::Result;
use std::collections::{HashSet, HashMap};

pub struct Day18{
    init_grid: Vec<Vec<bool>>,
}

pub fn setup(_input:&str) -> Result<Day18, Box<dyn Error>>{
    let mut res = Day18{
        init_grid: Vec::new(),
    };
    for line in read_lines(_input){
        let l = line?;
        res.init_grid.push(l.trim().chars().map(|c|{c == '#'}).collect());
    }
    println!("{}x{}", res.init_grid.len(), res.init_grid[0].len());
    Ok(res)
}

fn nbrs(x: usize, y: usize, maxx: usize, maxy: usize) -> Vec<(usize, usize)> {
    let xi = x as i64;
    let yi = y as i64;
    let mxi = maxx as i64;
    let myi = maxy as i64;
    let ns = [(xi-1, yi-1), (xi-1, yi), (xi-1, yi+1),
              (xi, yi-1),               (xi, yi+1),
              (xi+1, yi-1), (xi+1, yi), (xi+1, yi+1)];
    ns.iter().filter(|(x,y)|{*x >= 0 && *y >= 0 && *x < mxi && *y < myi}).map(|(x,y)|{(*x as usize, *y as usize)}).collect()
}

fn step(curr: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    let mut new = Vec::new();
    let maxx = curr[0].len();
    let maxy = curr.len();
    new.reserve(maxy);
    for y in 0..maxy{
        let mut v = Vec::new();
        v.reserve(maxx);
        for x in 0..maxx{
            let ns = nbrs(x, y, maxx, maxy).iter().filter(|(x, y)|{curr[*y][*x]}).count();
            let alive = (ns == 3) || (ns == 2 && curr[y][x]);
            //println!("({}, {}) has {} live neighbors, {}", x, y, ns, alive);
            //println!("{:?}", nbrs(x, y, maxx, maxy));
            v.push(alive);
        }
        new.push(v);
    }
    new
}

fn printgrid(grid: &Vec<Vec<bool>>) {
    for v in grid{
        println!("{}", v.iter().map(|b|{if *b{'#'}else{'.'}}).collect::<String>());
    }
    println!();
}

impl Problem for Day18{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut state = self.init_grid.clone();
        printgrid(&state);
        for _ in 0..100 {
            state = step(&state);
            printgrid(&state);
        }
        let count: usize = state.iter().map(|v|{v.iter().filter(|x|{**x}).count()}).sum();
        Ok(count.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut state = self.init_grid.clone();
        state[0][0] = true;
        state[99][0] = true;
        state[0][99] = true;
        state[99][99] = true;
        printgrid(&state);
        for _ in 0..100 {
            state = step(&state);
            state[0][0] = true;
            state[99][0] = true;
            state[0][99] = true;
            state[99][99] = true;
            printgrid(&state);
        }
        let count: usize = state.iter().map(|v|{v.iter().filter(|x|{**x}).count()}).sum();
        Ok(count.to_string())
    }
}