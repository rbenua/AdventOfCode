use crate::Problem;
use crate::read_lines;
use std::error::Error;
use std::fs::read;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::result::Result;
use std::collections::{HashSet, HashMap};
use std::thread;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use num::integer::Roots;

#[derive(Debug)]
pub struct Day20{
    goal: usize,
    res_ptr: Arc<AtomicUsize>,
    max_seen: Arc<AtomicUsize>,
}

const NUM_THREADS: usize = 10;

pub fn setup(_input:&str) -> Result<Day20, Box<dyn Error>>{
    let res = Day20{
        goal: _input.parse::<usize>()?,
        res_ptr: Arc::new(AtomicUsize::new(usize::MAX)),
        max_seen: Arc::new(AtomicUsize::new(0)),
    };
    Ok(res)
}

fn count1(test: usize) -> usize {
    let mut total = 0;
    for i in 1..test.sqrt()+1 {
        if test % i == 0 {
            total += i;
            total += test / i;
        }
    }
    total
}

fn count2(test: usize) -> usize {
    let mut total = 0;
    //println!("testing {}, s={}", test, s);
    for i in 1..if test > 50 {50} else {test} + 1 {
        if test % i == 0 {
            //println!("adding {}", test / i);
            total += test / i;
        }
    }
    //println!("total {}, {}", total, total * 11);
    total * 11
}

fn scan(base: usize, stride: usize, goal: usize, 
        res_ptr: Arc<AtomicUsize>, max_seen: Arc<AtomicUsize>,
        count: fn(usize) -> usize) {
    let mut i = base;
    while i < res_ptr.load(Ordering::Relaxed) {
        let v = count(i);
        if max_seen.fetch_max(v, Ordering::Relaxed) < v {
            //println!("new max {} at {}", v, i);
        }
        if v >= goal {
            res_ptr.fetch_min(i, Ordering::Release);
            break;
        }
        i += stride;
    }
}

fn spawn_all(goal: usize, count: fn(usize) -> usize,
                res_ptr: Arc<AtomicUsize>, max_seen: Arc<AtomicUsize>) -> usize {
                
    let mut handles = Vec::new();
    for i in 0..NUM_THREADS {
        let v = res_ptr.clone();
        let m = max_seen.clone();
        let goal = goal.clone();
        handles.push(thread::spawn(move ||{
            scan(i+1, NUM_THREADS, goal, v, m, count);
        }));
    }
    for h in handles {
        h.join().unwrap();
    }
    res_ptr.load(Ordering::Acquire)
}

impl Problem for Day20{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(spawn_all(self.goal / 10, count1, self.res_ptr.clone(), self.max_seen.clone()).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        self.res_ptr.store(usize::MAX, Ordering::Release);
        self.max_seen.store(0, Ordering::Release);
        Ok(spawn_all(self.goal, count2, self.res_ptr.clone(), self.max_seen.clone()).to_string())
    }
}