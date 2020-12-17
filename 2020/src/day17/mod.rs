use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashSet;

type Grid = HashSet<(i32, i32, i32)>;
type Grid2 = HashSet<(i32, i32, i32, i32)>;

pub struct Day17{
    set: Grid,
}

pub fn setup(_input:&str) -> Result<Day17, Box<dyn Error>>{
    let mut res = Day17{
        set: HashSet::new(),
    };
    for (y, line) in read_lines(_input).enumerate() {
        let row = line?;
        for (x, c) in row.trim().chars().enumerate() {
            match c {
                '#' => res.set.insert((x as i32, y as i32, 0)),
                '.' => false,
                _   => return Err(new_err("invalid character")),
            };
        }
    }
    Ok(res)
}

fn step(st: &Grid) -> Grid {
    let mut to_check = HashSet::new();
    // there is some extremely fancy way of producing this set with a single line of chained
    // iterator functions.
    for (x, y, z) in st {
        for nx in x-1..x+2 {
            for ny in y-1..y+2 {
                for nz in z-1..z+2 {
                    to_check.insert((nx, ny, nz));
                }
            }
        }
    }
    let mut res = HashSet::new();
    for (x, y, z) in to_check {
        let mut count = 0;
        for nx in x-1..x+2 {
            for ny in y-1..y+2 {
                for nz in z-1..z+2 {
                    if st.contains(&(nx, ny, nz)) {
                        count += 1;
                    }
                }
            }
        }
        if st.contains(&(x, y, z)) {
            if count == 3 || count == 4 { // count includes the current cell, not just nbrs
                res.insert((x, y, z));
            }
        }
        else {
            if count == 3 {
                res.insert((x, y, z));
            }
        }
    }
    res
}

fn run(st: &Grid, n: usize) -> Grid {
    let mut cur = step(st);
    for _ in 1..n {
        cur = step(&cur);
    }
    cur
}

fn step2(st: &Grid2) -> Grid2 {
    let mut to_check = HashSet::new();
    for (x, y, z, w) in st {
        for nx in x-1..x+2 {
            for ny in y-1..y+2 {
                for nz in z-1..z+2 {
                    for nw in w-1..w+2 {
                        to_check.insert((nx, ny, nz, nw));
                    }
                }
            }
        }
    }
    let mut res = HashSet::new();
    for (x, y, z, w) in to_check {
        let mut count = 0;
        for nx in x-1..x+2 {
            for ny in y-1..y+2 {
                for nz in z-1..z+2 {
                    for nw in w-1..w+2 {
                        if st.contains(&(nx, ny, nz, nw)) {
                            count += 1;
                        }
                    }
                }
            }
        }
        if st.contains(&(x, y, z, w)) {
            if count == 3 || count == 4 { // count includes the current cell, not just nbrs
                res.insert((x, y, z, w));
            }
        }
        else {
            if count == 3 {
                res.insert((x, y, z, w));
            }
        }
    }
    res
}
fn run2(st: &Grid, n: usize) -> Grid2 {
    let mut cur: Grid2 = st.iter().map(|(x,y,z)|{(*x,*y,*z,0)}).collect();
    for _ in 0..n {
        cur = step2(&cur);
    }
    cur
}

impl Problem for Day17{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(run(&self.set, 6).len().to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(run2(&self.set, 6).len().to_string())
    }
}
