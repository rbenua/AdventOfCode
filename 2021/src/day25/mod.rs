use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashSet;

pub struct Day25{
    init_state: State,
}

pub fn setup(_input:&str) -> Result<Day25, Box<dyn Error>>{
    let mut rights = HashSet::new();
    let mut downs = HashSet::new();
    let mut ysize = 0;
    let mut xsize = 0;
    for (line_opt, y) in read_lines(_input).zip(0..) {
        let line = line_opt?;
        xsize = line.len() as i64;
        ysize = y + 1;
        rights.extend(line.match_indices('>').map(|(x,_)|(x as i64, y)));
        downs.extend(line.match_indices('v').map(|(x,_)|(x as i64, y)));
    }
    Ok(Day25{init_state: State{rights, downs, ysize, xsize}})
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct State{
    rights: HashSet<(i64, i64)>,
    downs: HashSet<(i64, i64)>,
    xsize: i64,
    ysize: i64,
}

impl std::fmt::Display for State{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.ysize {
            writeln!(f, "{}", (0..self.xsize).map(|x|{
                if self.rights.contains(&(x, y)) {
                    '>'
                }
                else if self.downs.contains(&(x, y)) {
                    'v'
                }
                else {
                    '.'
                }
            }).collect::<String>())?;
        }
        Ok(())
    }
}

impl State{
    fn step(&self) -> State {
        let mut rights = HashSet::with_capacity(self.rights.len());
        let mut downs = HashSet::with_capacity(self.downs.len());
        for &p in &self.rights {
            let np = ((p.0 + 1) % self.xsize, p.1);
            if self.rights.contains(&np) || self.downs.contains(&np) {
                rights.insert(p);
            }
            else {
                rights.insert(np);
            }
        }
        for &p in &self.downs {
            let np = (p.0, (p.1 + 1) % self.ysize);
            if rights.contains(&np) || self.downs.contains(&np) {
                downs.insert(p);
            }
            else {
                downs.insert(np);
            }
        }
        State{
            rights,
            downs,
            xsize: self.xsize,
            ysize: self.ysize,
        }
    }
}

impl Problem for Day25{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut curr = self.init_state.clone();
        for i in 1usize.. {
            let new = curr.step();
            if new == curr {
                return Ok(i.to_string());
            }
            curr = new;
        }
        Err(new_err("infinite loop terminated???"))
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok("**".to_string())
    }
}
