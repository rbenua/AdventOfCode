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
struct State<const N: usize>{
    hallway: [Option<usize>; 11],
    alcoves: [[Option<usize>; N]; 4],
}

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

impl<const N: usize> std::fmt::Display for State<N>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "#############")?;
        writeln!(f, "#{}#", self.hallway.iter().map(i2c).collect::<String>())?;
        writeln!(f, "###{}#{}#{}#{}###", i2c(&self.alcoves[0][0]), i2c(&self.alcoves[1][0]), 
                 i2c(&self.alcoves[2][0]), i2c(&self.alcoves[3][0]))?;
        for i in 1..N {
            writeln!(f, "  #{}#{}#{}#{}#  ", 
                     i2c(&self.alcoves[0][i]), i2c(&self.alcoves[1][i]),
                     i2c(&self.alcoves[2][i]), i2c(&self.alcoves[3][i]))?;
        }
        write!(f, "  #########  ")
    }
}

pub struct Day23{
    init_state: State<2>,
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

impl<const N: usize> State<N>{
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

    fn try_place(&mut self, c: usize) -> bool {
        let alcove = &mut self.alcoves[c];
        if !alcove.iter().all(|o|match o{
            Some(cc) => c == *cc,
            None => true,
        }) {
            return false;
        }
        for i in (0..N).rev() {
            if alcove[i].is_none(){
                alcove[i] = Some(c);
                break;
            }
        }
        true 
    }

    fn peek(&self, c: usize) -> Option<(usize, usize)> {
        let mut res = None;
        let mut all_sat = true;
        for pos in 0..N {
            if let Some(place) = self.alcoves[c][pos] {
                if res.is_none() {
                    res = Some((place, pos));
                }
                if place != c {
                    all_sat = false;
                }
            }
        }
        if !all_sat {
            return res;
        }
        None
    }
    
    fn alcove_cost(&self, c: usize) -> usize {
        let alcove = &self.alcoves[c];
        let mut total = 0;
        for (p, dist) in alcove.iter().zip(1..) {
            if alcove[(dist-1)..].iter().all(|cc|*cc == Some(c)) {
                continue;
            }
            let entry = cost(c) * dist;
            let e = p.unwrap();
            let exit = cost(e) * dist;
            total += entry + exit;
        }
        total
    }

    fn quiesce(&mut self) -> usize {
        let mut total = 0;
        let mut found_any = true;
        while found_any {
            found_any = false;
            for i in 0..11 {
                if let Some(c) = self.hallway[i] {
                    if !self.blocked(i, ALCOVE_LOCS[c]) && self.try_place(c) {
                        found_any = true;
                        total += cost(c) * dist(i, ALCOVE_LOCS[c]);
                        self.hallway[i] = None;
                    }
                }
            }
            for a in 0..4 {
                if let Some((c, d)) = self.peek(a) {
                    if c != a && !self.blocked(ALCOVE_LOCS[a], ALCOVE_LOCS[c]) && self.try_place(c) {
                        found_any = true;
                        total += cost(c) * dist(ALCOVE_LOCS[a], ALCOVE_LOCS[c]);
                        self.alcoves[a][d] = None;
                    }
                }
            }
        }
        total
    }

    fn nbrs(&self) -> Vec<(State<N>, usize)> {
        let mut res = Vec::new();
        for a in 0..4 {
            if let Some((c, d)) = self.peek(a) {
                let src = ALCOVE_LOCS[a];
                for dst in WAIT_LOCS {
                    if !self.blocked(src, dst) {
                        let mut cs = cost(c) * dist(src, dst);
                        let mut new_st = self.clone();
                        new_st.hallway[dst] = Some(c);
                        new_st.alcoves[a][d] = None;
                        cs += new_st.quiesce();
                        res.push((new_st, cs));
                    }
                }
            }
        }
        res
    }

    fn solved(&self) -> bool {
        (0..4).all(|c|self.alcoves[c].iter().all(|o|*o == Some(c)))
    }

    fn fixed_cost(&self) -> usize {
        (0..4).map(|c|self.alcove_cost(c)).sum()
    }

    fn search(&self) -> Option<usize> {
        let mut to_visit = BinaryHeap::new();
        let mut visited = HashSet::new();
        to_visit.push(Reverse((0, self.clone())));
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
} // impl State<N>


const MIDDLE_PARTS: [[usize; 2]; 4] = [[3,3], [2,1], [1,0], [0,2]];

impl Problem for Day23{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        println!("{}", self.init_state);
        let res = self.init_state.search().unwrap() + self.init_state.fixed_cost();
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
        let res = init.search().unwrap() + init.fixed_cost();
        Ok(res.to_string())
    }
}
