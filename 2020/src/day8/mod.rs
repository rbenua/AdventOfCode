use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

#[derive(Display, FromStr, PartialEq, Debug, Copy, Clone)]
enum Insn{
    #[display("jmp {0}")]
    Jmp(#[display("{:+}")] i32),
    #[display("acc {0}")]
    Acc(#[display("{:+}")] i32),
    #[display("nop {0}")]
    Nop(#[display("{:+}")] i32),
}
use Insn::*;


pub struct Day8{
    program: Vec<Insn>,
}

fn step(i: Insn, pc: i32, acc: i32) -> (i32, i32) {
    match i {
        Jmp(j) => (pc + j, acc),
        Acc(j) => (pc + 1, acc + j),
        Nop(_) => (pc + 1, acc),
    }
}
fn run(program: &Vec<Insn>) -> (i32, i32) {
    let mut visited = vec![false; program.len()];
    let mut pc: i32 = 0;
    let mut acc = 0;
    while pc >= 0 && (pc as usize) < program.len() && !visited[pc as usize] {
        visited[pc as usize] = true;
        (pc, acc) = step(program[pc as usize], pc, acc);
    }
    (pc, acc)
}

fn swap(i: Insn) -> Insn {
    match i {
        Jmp(j) => Nop(j),
        Acc(j) => Acc(j),
        Nop(j) => Jmp(j),
    }
}

pub fn setup(_input:&str) -> Result<Day8, Box<dyn Error>>{
    let mut res = Day8{
        program: Vec::new(),
    };
    for line in read_lines(_input) {
        res.program.push(line?.parse()?);
    }
    Ok(res)   
}

impl Problem for Day8{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let (_, acc) = run(&self.program);
        Ok(acc.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        for i in 0..self.program.len() {
            self.program[i] = swap(self.program[i]);
            let (pc, acc) = run(&self.program);
            if (pc as usize) == self.program.len() {
                return Ok(acc.to_string());
            }
            self.program[i] = swap(self.program[i]);
        }
        Err(new_err("Not found"))
    }
}
