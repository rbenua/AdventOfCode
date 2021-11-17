use crate::Problem;
use crate::read_lines;
use std::error::Error;
use std::fs::read;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::result::Result;
use std::collections::{HashSet, HashMap, VecDeque};
use enum_map::EnumMap;

#[derive(Debug)]
pub struct Day23{
    program: Vec<Insn>,
}

#[derive(Debug, enum_map::Enum, Clone, Copy)]
enum Reg {
    A,
    B
}
#[derive(Debug)]
enum Insn {
    Hlf(Reg),
    Tpl(Reg),
    Inc(Reg),
    Jmp(i64),
    Jie(Reg, i64),
    Jio(Reg, i64),
}

#[derive(Debug, Clone)]
struct State{
    pc: i64,
    regs: EnumMap<Reg, i64>,
}

fn reg(id: &str) -> Result<Reg, Box<dyn Error>>  {
    if id == "a" {
        Ok(Reg::A)
    }
    else if id == "b" {
        Ok(Reg::B)
    }
    else {
        Err(new_err("bad register name"))
    }
}

pub fn setup(_input:&str) -> Result<Day23, Box<dyn Error>>{
    let re = Regex::new(r"^(\w{3}) (\S+)(?:, (\S+))?$")?;
    let mut res = Day23{
        program: Vec::new(),
    };
    for line in read_lines(_input){
        let l = line?;
        let cs = re.captures(&l).ok_or(new_err("bad format"))?;
        let i = match &cs[1] {
            "hlf" => Insn::Hlf(reg(&cs[2])?),
            "tpl" => Insn::Tpl(reg(&cs[2])?),
            "inc" => Insn::Inc(reg(&cs[2])?),
            "jmp" => Insn::Jmp(cs[2].parse::<i64>()?),
            "jie" => Insn::Jie(reg(&cs[2])?, cs[3].parse::<i64>()?),
            "jio" => Insn::Jio(reg(&cs[2])?, cs[3].parse::<i64>()?),
            _ => return Err(new_err("bad instruction type"))
        };
        res.program.push(i);
    }
    Ok(res)
}

fn step(st: &mut State, program: &Vec<Insn>) -> bool {
    match program[st.pc as usize] {
        Insn::Hlf(r) => {st.regs[r] = st.regs[r] / 2; st.pc += 1},
        Insn::Tpl(r) => {st.regs[r] = st.regs[r] * 3; st.pc += 1},
        Insn::Inc(r) => {st.regs[r] = st.regs[r] + 1; st.pc += 1},
        Insn::Jmp(off) => st.pc += off,
        Insn::Jie(r, off) => if st.regs[r] % 2 == 0 {
            st.pc += off;
        } else { st.pc += 1 },
        Insn::Jio(r, off) => if st.regs[r] == 1 {
            st.pc += off;
        } else { st.pc += 1 },
    };
    st.pc >= 0 && st.pc < (program.len() as i64)
}

fn run(init: &State, program: &Vec<Insn>) -> State {
    let mut state = init.clone();
    while step(&mut state, program) {}
    state
}

impl Problem for Day23{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let init = State{
            pc: 0,
            regs: EnumMap::default(),
        };
        let end = run(&init, &self.program);
        Ok(end.regs[Reg::B].to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let init = State{
            pc: 0,
            regs: EnumMap::from_array([1, 0]),
        };
        let end = run(&init, &self.program);
        Ok(end.regs[Reg::B].to_string())
    }
}