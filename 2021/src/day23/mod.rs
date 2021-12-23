use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::{BinaryHeap,HashSet};
use core::cmp::Reverse;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
struct State<T: Alcove>{
    hallway: [Option<usize>; 11],
    alcoves: [T; 4],
}

// this was not the fastest way to accomplish the problem but i wanted to learn how to do it.
trait Alcove 
where Self: std::fmt::Debug + Copy + Clone + Ord + PartialOrd + Eq + PartialEq + std::hash::Hash 
+ IntoIterator<Item=Option<usize>, IntoIter: DoubleEndedIterator + ExactSizeIterator> 
+ std::ops::IndexMut<usize, Output=Option<usize>> {
    
    fn try_place(&mut self, c: usize) -> bool {
        if !self.into_iter().all(|o|match o{
            Some(cc) => c == cc,
            None => true,
        }) {
            return false;
        }
        let rpos = self.into_iter().rev().position(|p|p.is_none()).unwrap();
        let last = self.into_iter().len() - 1;
        self[last - rpos] = Some(c);
        true 
    }

    fn try_remove(&mut self, c: usize) -> Option<usize> {
        let mut res = None;
        let mut all_sat = true;
        let mut rpos = None;
        for (place, pos) in self.into_iter().zip(0..) {
            if res.is_none() && place.is_some() {
                res = place;
                rpos = Some(pos);
            }
            if place.unwrap_or(c) != c {
                all_sat = false;
            }
        }
        if !all_sat {
            let pos = rpos.unwrap();
            self[pos] = None;
            return res;
        }
        None
    }
    fn peek(&self, c: usize) -> Option<usize> {
        let mut res = None;
        let mut all_sat = true;
        for place in self.into_iter() {
            if res.is_none() && place.is_some() {
                res = place;
            }
            if place.unwrap_or(c) != c {
                all_sat = false;
            }
        }
        if !all_sat {
            return res;
        }
        None
    }
    
    fn fixed_cost(&self, c: usize) -> usize {
        let mut total = 0;
        for (p, dist) in self.into_iter().zip(1..) {
            let entry = cost(c) * dist;
            let e = p.unwrap();
            let exit = cost(e) * dist;
            total += entry + exit;
        }
        total
    }

    fn solved(&self, c: usize) -> bool {
        self.into_iter().all(|s|s == Some(c))
    }
} // trait Alcove

type Alcove2 = [Option<usize>; 2];
impl Alcove for Alcove2 {}

type Alcove4 = [Option<usize>; 4];
impl Alcove for Alcove4 {}



const CHARS: [char; 4] = ['A', 'B', 'C', 'D'];
fn i2c(i: &Option<usize>) -> char {
    match i {
        Some(n) => CHARS[*n],
        None => '.',
    }
}
fn c2i(c: char) -> Option<usize> {
    CHARS.iter().position(|cc|c == *cc)
}

impl<T: Alcove> std::fmt::Display for State<T>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "#############")?;
        writeln!(f, "#{}#", self.hallway.iter().map(i2c).collect::<String>())?;
        let mut its = [self.alcoves[0].into_iter(), self.alcoves[1].into_iter(), self.alcoves[2].into_iter(), self.alcoves[3].into_iter()];
        writeln!(f, "###{}#{}#{}#{}###", 
                i2c(&its[0].next().unwrap()), i2c(&its[1].next().unwrap()), i2c(&its[2].next().unwrap()), i2c(&its[3].next().unwrap()))?;
        for _ in 0..(self.alcoves[0].into_iter().len() - 1) {
            writeln!(f, "  #{}#{}#{}#{}#  ", 
                    i2c(&its[0].next().unwrap()), i2c(&its[1].next().unwrap()), i2c(&its[2].next().unwrap()), i2c(&its[3].next().unwrap()))?;
        }
        write!(f, "  #########  ")
    }
}

pub struct Day23{
    init_state: State<Alcove2>,
}

pub fn setup(_input:&str) -> Result<Day23, Box<dyn Error>>{
    let cs = read_lines(_input).filter_map(|line_opt|{
        let res = line_opt.unwrap().chars().filter_map(c2i).collect::<Vec<usize>>();
        if res.len() > 0 {Some(res)} else {None}
    }).collect::<Vec<Vec<usize>>>();
    Ok(Day23{
        init_state: State{
            hallway: [None; 11],
            alcoves: [[Some(cs[0][0]), Some(cs[1][0])],
                      [Some(cs[0][1]), Some(cs[1][1])],
                      [Some(cs[0][2]), Some(cs[1][2])],
                      [Some(cs[0][3]), Some(cs[1][3])]]
        }
    })
}

const ALCOVE_LOCS: [usize; 4] = [2,4,6,8];
const WAIT_LOCS: [usize; 7] = [0,1,3,5,7,9,10];

fn cost(c: usize) -> usize {
    10usize.pow(c as u32)
}
fn dist(a: usize, b: usize) -> usize {
    ((a as i64) - (b as i64)).abs() as usize
}

impl<T: Alcove> State<T>{
    fn blocked(&self, src: usize, dest: usize) -> bool {
        let step = if dest < src {-1} else {1};
        let mut pos = src as i64 + step;
        while (pos as usize) != dest {
            if self.hallway[pos as usize] != None {
                return true;
            }
            pos += step;
        }
        self.hallway[dest] != None
    }
    
    fn quiesce(&mut self) -> usize {
        let mut total = 0;
        let mut found_any = true;
        while found_any {
            found_any = false;
            for i in 0..11 {
                if let Some(c) = self.hallway[i] {
                    if !self.blocked(i, ALCOVE_LOCS[c]) && self.alcoves[c].try_place(c) {
                        found_any = true;
                        total += cost(c) * dist(i, ALCOVE_LOCS[c]);
                        self.hallway[i] = None;
                    }
                }
            }
            for a in 0..4 {
                if let Some(c) = self.alcoves[a].peek(a) {
                    if c != a && !self.blocked(ALCOVE_LOCS[a], ALCOVE_LOCS[c]) && self.alcoves[c].try_place(c) {
                        found_any = true;
                        total += cost(c) * dist(ALCOVE_LOCS[a], ALCOVE_LOCS[c]);
                        self.alcoves[a].try_remove(a);
                    }
                }
            }
        }
        total
    }

    fn nbrs(&self) -> Vec<(State<T>, usize)> {
        let mut res = Vec::new();
        for a in 0..4 {
            let mut base = self.clone();
            if let Some(c) = base.alcoves[a].try_remove(a) {
                let src = ALCOVE_LOCS[a];
                for dst in WAIT_LOCS {
                    if !base.blocked(src, dst) {
                        let mut cs = cost(c) * dist(src, dst);
                        let mut new_st = base.clone();
                        new_st.hallway[dst] = Some(c);
                        cs += new_st.quiesce();
                        res.push((new_st, cs));
                    }
                }
            }
        }
        res
    }

    fn solved(&self) -> bool {
        (0..4).all(|c|self.alcoves[c].solved(c))
    }

    fn fixed_cost(&self) -> usize {
        (0..4).map(|c|self.alcoves[c].fixed_cost(c)).sum()
    }
} // impl<T: Alcove> State<T>

fn search<T: Alcove>(start: &State<T>) -> Option<usize> {
    let mut to_visit = BinaryHeap::new();
    let mut visited = HashSet::new();
    to_visit.push(Reverse((0, start.clone())));
    while let Some(Reverse((cost, curr_state))) = to_visit.pop() {
        if curr_state.solved() {
            return Some(cost);
        }
        if visited.insert(curr_state) {
            for (np, dist) in curr_state.nbrs() {
                if !visited.contains(&np) {
                    to_visit.push(Reverse((cost + dist, np)));
                }
            }
        }
    }
    None
}

const MIDDLE_PARTS: [[usize; 2]; 4] = [[3,3], [2,1], [1,0], [0,2]];

impl Problem for Day23{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        println!("{}", self.init_state);
        let res = search(&self.init_state).unwrap() + self.init_state.fixed_cost();
        Ok(res.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut init = State{
            hallway: [None; 11],
            alcoves: [[None; 4]; 4],
        };
        for i in 0..4 {
            init.alcoves[i][0] = self.init_state.alcoves[i][0];
            init.alcoves[i][1] = Some(MIDDLE_PARTS[i][0]);
            init.alcoves[i][2] = Some(MIDDLE_PARTS[i][1]);
            init.alcoves[i][3] = self.init_state.alcoves[i][1];
        }
        println!("{}", init);
        let res = search(&init).unwrap() + init.fixed_cost();
        Ok(res.to_string())
    }
}
