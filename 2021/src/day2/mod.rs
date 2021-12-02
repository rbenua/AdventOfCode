use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

enum Command{
    Forward(i64),
    Up(i64),
    Down(i64),
}

struct State{
    hpos: i64,
    depth: i64,
    aim: i64,
}

pub struct Day2{
    commands: Vec<Command>,
}

pub fn setup(_input:&str) -> Result<Day2, Box<dyn Error>>{
    let mut res = Day2{
        commands: Vec::new(),
    };
    for line in read_lines(_input){
        let line_opt = line?;
        let mut sl = line_opt.split(" ");
        let cmd = sl.next().unwrap();
        let amt = sl.next().unwrap().parse::<i64>()?;
        res.commands.push(match cmd {
            "forward" => Command::Forward(amt),
            "up" => Command::Up(amt),
            "down" => Command::Down(amt),
            _ => return Err(new_err("bad command")),
        });
    }
    Ok(res)
}

fn step1(cmd: &Command, st: &mut State) {
    match cmd {
        Command::Forward(amt) => st.hpos += amt,
        Command::Up(amt) => st.depth -= amt,
        Command::Down(amt) => st.depth += amt,
    }
}

fn step2(cmd: &Command, st: &mut State) {
    match cmd {
        Command::Forward(amt) => {
            st.hpos += amt;
            st.depth += amt * st.aim;
        },
        Command::Up(amt) => st.aim -= amt,
        Command::Down(amt) => st.aim += amt,
    }
}
impl Problem for Day2{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut st = State{
            hpos: 0,
            depth: 0,
            aim: 0,
        };
        for cmd in &self.commands {
            step1(&cmd, &mut st);
        }
        Ok((st.hpos * st.depth).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut st = State{
            hpos: 0,
            depth: 0,
            aim: 0,
        };
        for cmd in &self.commands {
            step2(&cmd, &mut st);
        }
        Ok((st.hpos * st.depth).to_string())
    }
}
