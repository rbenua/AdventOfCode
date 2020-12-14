use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashMap;

pub struct Day14{
    prog: Vec<Insn>,
}

#[derive(Debug, Copy, Clone)]
enum Insn {
    Mem(u64, u64),
    Mask(u64, u64, u64),
}
use Insn::*;

pub fn setup(_input:&str) -> Result<Day14, Box<dyn Error>>{
    let mut res = Day14{
        prog: Vec::new(),
    };
    let re = Regex::new(r"(?P<lhs>mask|mem\[(?P<addr>\d+)\]) = (?P<rhs>[0-9X]+)")?;
    for line in read_lines(_input) {
        let ugh = line?;
        let caps = re.captures(&ugh).unwrap();
        if &caps["lhs"] == "mask" {
            let mut set = 0;
            let mut clear = 0;
            let mut x = 0;
            for c in caps["rhs"].chars() {
                set <<= 1;
                clear <<= 1;
                x <<= 1;
                match c {
                    '0' => clear |= 1,
                    '1' => set |= 1,
                    'X' => x |= 1,
                    _   => (),
                };
            }
            res.prog.push(Mask(set, clear, x));
        }
        else {
            res.prog.push(Mem(caps["addr"].parse()?, caps["rhs"].parse()?));
        }
    }
    Ok(res)   
}

fn set_all_mem(mem: &mut HashMap<u64, u64>, addr: u64, data: u64, xs: u64) -> () {
    if xs == 0 {
        mem.insert(addr, data);
    }
    else {
        let place = 1 << xs.trailing_zeros();
        let rem_xs = xs & !place;
        set_all_mem(mem, addr | place, data, rem_xs);
        set_all_mem(mem, addr & !place, data, rem_xs);
    }
}

impl Problem for Day14{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut mem: HashMap<u64, u64> = HashMap::new();
        let mut cur_set = 0;
        let mut cur_clr = 0;
        for i in &self.prog {
            match *i {
                Mask(set, clr, _) => {
                    cur_set = set;
                    cur_clr = clr;
                },
                Mem(addr, data) => {
                    let res = (data | cur_set) & !cur_clr;
                    mem.insert(addr, res);
                },
            };
        }
        let answer = mem.values().sum::<u64>();
        Ok(answer.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut mem: HashMap<u64, u64> = HashMap::new();
        let mut cur_set = 0;
        let mut cur_x = 0;
        for i in &self.prog {
            match *i {
                Mask(set, _, x) => {
                    cur_set = set;
                    cur_x = x;
                },
                Mem(addr, data) => {
                    set_all_mem(&mut mem, addr | cur_set, data, cur_x);
                }
            };
        }

        let answer = mem.values().sum::<u64>();
        Ok(answer.to_string())
    }
}
