use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use string_error::static_err;
use parse_display::{Display, FromStr};
use enum_map::EnumMap;
use enum_map::Enum;
use std::collections::VecDeque;
use std::thread;
use std::sync::mpsc::channel;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::io::{stdout, Write};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum Arg{
    Reg(Register),
    Lit(i64),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Enum)]
enum Register{
    W,
    X,
    Y,
    Z,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum Insn{
    Inp(Register),
    Add(Register, Arg),
    Mul(Register, Arg),
    Div(Register, Arg),
    Mod(Register, Arg),
    Eql(Register, Arg),
}
use Insn::*;
use Arg::*;
use Register::*;

impl std::str::FromStr for Register {
    type Err = Box<dyn Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "w" => Ok(W),
            "x" => Ok(X),
            "y" => Ok(Y),
            "z" => Ok(Z),
            _ => Err(new_err(&format!("bad register {}", s))),
        }
    }
}

impl std::str::FromStr for Arg {
    type Err = Box<dyn Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(val) = s.parse::<i64>() {
            Ok(Lit(val))
        }
        else {
            Ok(Reg(s.parse()?))
        }
    }
}

fn from_arg(a: Option<&&str>) -> Result<Arg, Box<dyn Error>> {
    a.ok_or(static_err("not enough arguments"))?.parse()
}
fn from_reg(a: Option<&&str>) -> Result<Register, Box<dyn Error>> {
    a.ok_or(static_err("not enough arguments"))?.parse()
}

impl std::str::FromStr for Insn {
    type Err = Box<dyn Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let subs = s.split_whitespace().collect::<Vec<&str>>();
        match subs[0] {
            "inp" => Ok(Inp(from_reg(subs.get(1))?)),
            "add" => Ok(Add(from_reg(subs.get(1))?, from_arg(subs.get(2))?)),
            "mul" => Ok(Mul(from_reg(subs.get(1))?, from_arg(subs.get(2))?)),
            "div" => Ok(Div(from_reg(subs.get(1))?, from_arg(subs.get(2))?)),
            "mod" => Ok(Mod(from_reg(subs.get(1))?, from_arg(subs.get(2))?)),
            "eql" => Ok(Eql(from_reg(subs.get(1))?, from_arg(subs.get(2))?)),
            _ => Err(new_err(&format!("bad instruction {}", s)))
        }
    }
}

pub struct Day24{
    program: Vec<Insn>,
}

pub fn setup(_input:&str) -> Result<Day24, Box<dyn Error>>{
    let mut program = Vec::new();
    for line_opt in read_lines(_input) {
        program.push(line_opt?.parse()?);
    }
    Ok(Day24{program})
}

#[derive(Debug, Clone)]
struct Machine<'a>{
    program: &'a Vec<Insn>,
    regs: EnumMap<Register, i64>,
    pc: usize,
    input: VecDeque<i64>,
}

impl<'a> std::fmt::Display for Machine<'a>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Program length {}, w={} x={} y={} z={}, pc={}{}, rem input {:?}", 
               self.program.len(), self.regs[W], self.regs[X], 
               self.regs[Y], self.regs[Z], self.pc, 
               if self.pc >= self.program.len() {" (done)"} else {""}, self.input)
    }
}

impl<'a> Machine<'a>{
    fn arg(&self, a: &Arg) -> i64 {
        match a {
            Reg(r) => self.regs[*r],
            Lit(l) => *l,
        }
    }

    fn step(&mut self) -> bool {
        if self.pc >= self.program.len() {
            return false;
        }

        let insn = &self.program[self.pc];
        match insn {
            Inp(r) => self.regs[*r] = {
                println!("{}", self);
                match self.input.pop_front() {
                    None => return false,
                    Some(i) => i,
                }
            },
            Add(r, a) => self.regs[*r] += self.arg(a),
            Mul(r, a) => self.regs[*r] *= self.arg(a),
            Div(r, a) => self.regs[*r] /= self.arg(a),
            Mod(r, a) => self.regs[*r] %= self.arg(a),
            Eql(r, a) => self.regs[*r] = if self.regs[*r] == self.arg(a) {1} else {0},
        }
        self.pc += 1;
        true
    }
    
    fn execute(&mut self) {
        while self.step() {}
    }
}
fn new_machine<'a>(program: &'a Vec<Insn>, input: VecDeque<i64>) -> Machine<'a> {
    Machine{
        program,
        input,
        regs: Default::default(),
        pc: 0,
    }
}

fn make_deque(mut i: i64) -> VecDeque<i64> {
    let mut d = VecDeque::new();
    while i > 0 {
        d.push_front(i % 10);
        i = i / 10;
    }
    while d.len() < 14 {
        d.push_front(0);
    }
    d
}

fn process(idx: usize, mut z: i64, w: i64) -> i64 {
    const XOFFS: [i64; 14] = [14, 12, 11, -4, 10, 10, 15, -9, -9, 12, -15, -7, -10, 0];
    const ZOFFS: [i64; 14] = [7, 4, 8, 1, 5, 14, 12, 10, 5, 7, 6, 8, 4, 6];
    let x = z % 26 + XOFFS[idx];
    if [3, 7, 8, 10, 11, 12, 13].contains(&idx) {
        z = z / 26;
    }
    if w != x {
        z *= 26;
        z += w + ZOFFS[idx];
    }
    z
}
fn increment(mut i: i64, nthreads: i64) -> i64 {
    for _c in 0..nthreads {
        i += 1;
        let mut c = 0;
        let mut p = 10;
        while (i % p) == 0 {
            c += p / 10;
            p *= 10;
        }
        i += c;
    }
    i
}

impl Problem for Day24{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        /*
        let mut i = 29599469991739;
        println!("{}", "93719996499592".chars().rev().collect::<String>());
        let mut z = 0;
        let mut power = 10i64.pow(13);
        for idx in 0..14 {
            let w = (i / power) % 10;
            let nz = process(idx, z, w);
            println!("process({}, {}, {}) = {}", idx, z, w, nz);
            z = nz;
            power /= 10;
        }
        let d = make_deque(i);
        let mut machine = new_machine(&self.program, d);
        machine.execute();
        println!("{}", machine);
        */
        /*
        Ok(machine.regs[Z].to_string())
        for first_i in (0..10000000).rev() {
            if first_i % 100000 == 0 {
                print!(".");
                std::io::stdout().flush()?;
            }
            let mut first_half = make_deque(first_i);
            let mut z = 0;
            for idx in 0..7 {
                z = process(idx, z, first_half.pop_front().unwrap());
            }
            let mut total = first_i;
            for idx in 7..14 {
                let (nz, i) = (1..10).map(|i|{
                    (process(idx, z, i), i)
                }).min().unwrap();
                total *= 10;
                total += i;
                z = nz;
                println!("digit {} best value is {}, z={}", idx, total, z);
            }
            if z == 0 {
                println!();
                return Ok(total.to_string());
            }
        }
        */
        const NTHREADS: i64 = 10;
        let mut handles = Vec::new();
        let (tx, rx) = channel::<i64>();
        let flag = Arc::new(AtomicBool::new(true));
        for tid in 0..NTHREADS {
            let t = tx.clone();
            let f = flag.clone();
            let handle = thread::spawn(move ||{
                // let mut i = 11111111111111 + tid;
                let mut i = 11438311111111 + tid;
                while i >= 0 && f.load(Ordering::Relaxed) {
                    let mut z = 0;
                    let mut power = 10i64.pow(13);
                    for idx in 0..14 {
                        z = process(idx, z, (i / power) % 10);
                        power /= 10;
                    }
                    if z == 0 {
                        println!("Thread {} found match {}", tid, i);
                        let d = make_deque(i);
                        if d.contains(&0) {
                            println!("contains zeroes, continuing");
                            i = increment(i, NTHREADS);
                            continue;
                        }
                        t.send(i).unwrap();
                        return;
                    }
                    i = increment(i, NTHREADS);
                    if i % 100000000 == 11111111 {
                        println!("Thread {} completed up to {}", tid, i);
                    }
                }
            });
            handles.push(handle);
        }
        let res = rx.recv()?;
        flag.store(false, Ordering::Relaxed);
        for handle in handles {
            handle.join().unwrap();
        }
        Ok(res.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok("".to_string())
    }
}
