use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::{BinaryHeap, HashSet};
use core::cmp::Reverse;
use std::time::{Duration,Instant};

pub struct Day15{
    risks: Vec<Vec<i64>>,
    xmax: usize,
    ymax: usize,
}

pub fn setup(_input:&str) -> Result<Day15, Box<dyn Error>>{
    let mut res = Day15{
        risks: Vec::new(),
        xmax: 0,
        ymax: 0,
    };
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        let row = line.trim().chars().filter_map(|c|{c.to_digit(10)}).map(|x|{x as i64}).collect();
        res.risks.push(row);
    }
    res.xmax = res.risks[0].len();
    res.ymax = res.risks.len();
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

impl Day15{
    fn risk1(&self, pt: (usize, usize)) -> i64 {
        self.risks[pt.1][pt.0]
    }
    fn risk2(&self, pt: (usize, usize)) -> i64 {
        let (x, y) = pt;
        let reps = (x / self.xmax + y / self.ymax) as i64;
        let base = self.risks[y % self.ymax][x % self.xmax];
        ((base - 1) + reps) % 9 + 1
    }
    fn search(&self, src: (usize, usize), dest: (usize, usize), risk: fn(&Day15, (usize, usize)) -> i64) -> i64 {
        let mut to_visit = BinaryHeap::new();
        let mut visited = HashSet::new();
        to_visit.push(Reverse((0, src)));
        while !to_visit.is_empty() {
            let Reverse((cost, (x, y))) = to_visit.pop().unwrap();
            //println!("({}, {}), {}", x, y, cost);
            if (x, y) == dest {
                return cost;
            }
            if visited.insert((x, y)) {
                for np in nbrs(x, y, dest.0 + 1, dest.1 + 1) {
                    to_visit.push(Reverse((cost + risk(&self, np), np)));
                }
            }
        }
        -1
    }
    fn search_vec(&self, src: (usize, usize), dest: (usize, usize), risk: fn(&Day15, (usize, usize)) -> i64) -> i64 {
        let mut to_visit = Vec::new();
        let mut visited = HashSet::new();
        to_visit.push(Reverse((0, src)));
        while !to_visit.is_empty() {
            let Reverse((cost, (x, y))) = to_visit.pop().unwrap();
            //println!("({}, {}), {}", x, y, cost);
            if (x, y) == dest {
                return cost;
            }
            if visited.insert((x, y)) {
                for np in nbrs(x, y, dest.0 + 1, dest.1 + 1) {
                    let new = Reverse((cost + risk(&self, np), np));
                    match to_visit.binary_search(&new) {
                        Err(idx) => to_visit.insert(idx, new),
                        Ok(_) => (),
                    };
                }
            }
        }
        -1
    }
    fn search_vec_scan(&self, src: (usize, usize), dest: (usize, usize), risk: fn(&Day15, (usize, usize)) -> i64) -> i64 {
        let mut to_visit = Vec::new();
        let mut visited = HashSet::new();
        to_visit.push((0, src));
        while !to_visit.is_empty() {
            let mut min_idx = 0;
            for i in 1..to_visit.len() {
                if to_visit[i] < to_visit[min_idx] {
                    min_idx = i;
                }
            }
            let (cost, (x, y)) = to_visit.remove(min_idx);
            //println!("({}, {}), {}", x, y, cost);
            if (x, y) == dest {
                return cost;
            }
            if visited.insert((x, y)) {
                for np in nbrs(x, y, dest.0 + 1, dest.1 + 1) {
                    let new = (cost + risk(&self, np), np);
                    to_visit.push(new);
                }
            }
        }
        -1
    }
    fn search_vec_nonrev(&self, src: (usize, usize), dest: (usize, usize), risk: fn(&Day15, (usize, usize)) -> i64) -> i64 {
        let mut to_visit = Vec::new();
        let mut visited = HashSet::new();
        to_visit.push((0, src));
        while !to_visit.is_empty() {
            let (cost, (x, y)) = to_visit.remove(0);
            //println!("({}, {}), {}", x, y, cost);
            if (x, y) == dest {
                return cost;
            }
            if visited.insert((x, y)) {
                for np in nbrs(x, y, dest.0 + 1, dest.1 + 1) {
                    let new = (cost + risk(&self, np), np);
                    match to_visit.binary_search(&new) {
                        Err(idx) => to_visit.insert(idx, new),
                        Ok(_) => (),
                    };
                }
            }
        }
        -1
    }
}

fn time_invoke<T>(name: &str, f: T) -> i64 
where T: Fn() -> i64 {
    let start = Instant::now();
    let res = f();
    let end = Instant::now();
    println!("{}: {}", end.duration_since(start).as_secs_f64(), name);
    res
}

impl Problem for Day15{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let res = time_invoke("part 1 min-heap", ||{self.search((0, 0), (self.xmax - 1, self.ymax - 1), Day15::risk1)});
        time_invoke("part 1 vector bsearch reversed", ||{self.search_vec((0, 0), (self.xmax - 1, self.ymax - 1), Day15::risk1)});
        time_invoke("part 1 vector bsearch non-reversed", ||{self.search_vec_nonrev((0, 0), (self.xmax - 1, self.ymax - 1), Day15::risk1)});
        time_invoke("part 1 vector linear scan", ||{self.search_vec_scan((0, 0), (self.xmax - 1, self.ymax - 1), Day15::risk1)});
        Ok(res.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let res = time_invoke("part 2 min-heap", ||{self.search((0, 0), (5 * self.xmax - 1, 5 * self.ymax - 1), Day15::risk2)});
        time_invoke("part 2 vector bsearch reversed", ||{self.search_vec((0, 0), (5 * self.xmax - 1, 5 * self.ymax - 1), Day15::risk2)});
        time_invoke("part 2 vector bsearch non-reversed", ||{self.search_vec_nonrev((0, 0), (5 * self.xmax - 1, 5 * self.ymax - 1), Day15::risk2)});
        time_invoke("part 2 vector linear scan", ||{self.search_vec_scan((0, 0), (5 * self.xmax - 1, 5 * self.ymax - 1), Day15::risk2)});
        Ok(res.to_string())
    }
}
