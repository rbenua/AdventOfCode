use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

#[derive(PartialEq, Debug, Copy, Clone)]
enum Cell{
    Seat,
    Floor,
    Person,
}
use Cell::*;

pub struct Day11{
    grid: Vec<Vec<Cell>>,
}

pub fn setup(_input:&str) -> Result<Day11, Box<dyn Error>>{
    let mut res = Day11{
        grid: Vec::new(),
    };
    for line in read_lines(_input) {
        let mut row = Vec::new();
        for c in line?.trim().chars() {
            row.push(match c {
                'L' => Seat,
                '.' => Floor,
                _ => { return Err(new_err("bad character"));},
            });
        }
        res.grid.push(row);
    }
    Ok(res)   
}

fn nbrs(x: i64, y: i64, _grid: &Vec<Vec<Cell>>) -> [(i64, i64); 8] {
    [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1),
     (x - 1, y),                 (x + 1, y),
     (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
}

fn nbrs_2(x: i64, y: i64, _grid: &Vec<Vec<Cell>>) -> [(i64, i64); 8] {
    let deltas = [(-1, 1),  (0, 1),  (1, 1),
                  (-1, 0),           (1, 0),
                  (-1, -1), (0, -1), (1, -1)];
    let mut res = [(0, 0); 8];
    for (i, d) in deltas.iter().enumerate() {
        let (dx, dy) = *d;
        let mut nx = x + dx;
        let mut ny = y + dy;
        while nx >= 0 && nx < _grid[0].len() as i64 &&
              ny >= 0 && ny < _grid.len() as i64 &&
              _grid[ny as usize][nx as usize] == Floor {
            nx += dx;
            ny += dy;
        }
        res[i] = (nx, ny);
    }
    res
}

fn build_nbr_grid(grid: &Vec<Vec<Cell>>, nfn: fn(x: i64, y: i64, _grid: &Vec<Vec<Cell>>) -> [(i64, i64); 8]) -> Vec<Vec<[Option<(usize, usize)>; 8]>> {
    let mut res = Vec::with_capacity(grid.len());
    for y in 0..grid.len() {
        let mut row = Vec::with_capacity(grid[y].len());
        for x in 0..grid[y].len() {
            let mut ns = [None; 8];
            for (i, (nx, ny)) in nfn(x as i64, y as i64, grid).iter().enumerate() {
                if *nx >= 0 && *ny >= 0 && *nx < grid[0].len() as i64 && *ny < grid.len() as i64 {
                    ns[i] = Some((*nx as usize, *ny as usize));
                }
            }
            row.push(ns);
        }
        res.push(row);
    }
    res
}


fn step_cell(grid: &Vec<Vec<Cell>>, x: usize, y : usize, nbr_grid: &Vec<Vec<[Option<(usize, usize)>; 8]>>, threshold: usize) -> Cell {
    let mut n = 0;
    for nbr in nbr_grid[y][x].iter() {
        if let Some((nx, ny)) = *nbr {
            match grid[ny][nx] {
                Person => n += 1,
                _ => (),
            };
        }
    }
    match (grid[y][x], n) {
        (Seat,  0) => Person,
        (Person, _) => if n >= threshold { Seat } else { Person },
        _ => grid[y][x],
    }
}
        
fn step_grid(grid: &Vec<Vec<Cell>>, nbr_grid: &Vec<Vec<[Option<(usize, usize)>; 8]>>, threshold: usize) -> Vec<Vec<Cell>> {
    let mut new = Vec::with_capacity(grid.len());
    for y in 0..grid.len() {
        let mut nrow = Vec::with_capacity(grid[y].len());
        for x in 0..grid[y].len() {
            nrow.push(step_cell(grid, x, y, nbr_grid, threshold));
        }
        new.push(nrow);
    }
    new
}

fn loop_grid(orig_grid: &Vec<Vec<Cell>>, nbr_grid: &Vec<Vec<[Option<(usize, usize)>; 8]>>, threshold: usize) -> usize {
    let mut grid = orig_grid.clone();
    loop {
        let new = step_grid(&grid, &nbr_grid, threshold);
        if new == grid {
             return new.iter().map(|row|{row.iter().filter(|p|{**p == Person}).count()}).sum();
        }
        grid = new;
    }
}

impl Problem for Day11{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let nbr_grid = build_nbr_grid(&self.grid, nbrs);
        Ok(loop_grid(&self.grid, &nbr_grid, 4).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let nbr_grid = build_nbr_grid(&self.grid, nbrs_2);
        Ok(loop_grid(&self.grid, &nbr_grid, 5).to_string())
    }
}
