use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashMap;

pub struct Day21{
    p1start: i32,
    p2start: i32,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct GameState{
    p1_score: i32,
    p2_score: i32,
    p1_pos: i32,
    p2_pos: i32,
    p2_turn: bool,
    won: bool,
}

pub fn setup(_input:&str) -> Result<Day21, Box<dyn Error>>{
    let re = Regex::new(r"starting position: (\d+)")?;
    let mut lines = read_lines(_input);
    let first_line = lines.next().unwrap()?;
    let p1start = re.captures(&first_line).unwrap()[1].parse::<i32>()? - 1;
    let second_line = lines.next().unwrap()?;
    let p2start = re.captures(&second_line).unwrap()[1].parse::<i32>()? - 1;
    Ok(Day21{p1start, p2start})
}

fn roll3(val: i32) -> (i32, i32) {
    let roll = 3 * val + 6;
    let die_val = (val + 3) % 100;
    (roll, die_val)
}

fn step(st: &GameState, roll: i32, win: i32) -> GameState  {
    if st.p2_turn {
        let p2_turn = false;
        let p2_pos = (st.p2_pos + roll) % 10;
        let p2_score = st.p2_score + p2_pos + 1;
        let won = p2_score >= win;
        GameState{
            p2_pos,
            p2_score,
            p2_turn,
            won,
            ..*st
        }
    }
    else {
        let p2_turn = true;
        let p1_pos = (st.p1_pos + roll) % 10;
        let p1_score = st.p1_score + p1_pos + 1;
        let won = p1_score >= win;
        GameState{
            p1_pos,
            p1_score,
            p2_turn,
            won,
            ..*st
        }
    }
}

fn simulate(p1start: i32, p2start: i32) -> (GameState, i32) {
    let mut state = GameState{
        p1_score: 0,
        p2_score: 0,
        p1_pos: p1start,
        p2_pos: p2start,
        p2_turn: false,
        won: false,
    };
    let mut die_val = 0;
    let mut die_rolls = 0;
    let mut roll;
    while !state.won {
        (roll, die_val) = roll3(die_val);
        die_rolls += 3;
        state = step(&state, roll, 1000);
    }
    (state, die_rolls)
}

const ROLL_VALUES: [i32; 27] = [3, 4, 5, 4, 5, 6, 5, 6, 7, 4, 5, 6, 5, 6, 7, 6, 7, 8, 5, 6, 7, 6, 7, 8, 7, 8, 9];

fn qsim(p1start: i32, p2start: i32) -> (u64, u64) {
    let mut wins1 = 0;
    let mut wins2 = 0;
    let mut cont_states = HashMap::new();
    let init_state = GameState{
        p1_score: 0,
        p2_score: 0,
        p1_pos: p1start,
        p2_pos: p2start,
        p2_turn: false,
        won: false,
    };
    cont_states.insert(init_state, 1);

    while !cont_states.is_empty() {
        let (&curr_state, &curr_count) = cont_states.iter().min_by_key(|(k, _v)|k.p1_score + k.p2_score).unwrap();
        cont_states.remove(&curr_state);
        for roll in ROLL_VALUES {
            let new_state = step(&curr_state, roll, 21);
            if new_state.won {
                if new_state.p1_score > new_state.p2_score {
                    wins1 += curr_count;
                }
                else {
                    wins2 += curr_count;
                }
            }
            else {
                *cont_states.entry(new_state).or_insert(0) += curr_count;
            }
        }
    }
    (wins1, wins2) 
}

impl Problem for Day21{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let (wonstate, die_rolls) = simulate(self.p1start, self.p2start);
        let min_score = if wonstate.p2_turn {wonstate.p2_score} else {wonstate.p1_score};
        let res = min_score * die_rolls; 
        Ok(res.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let (wins1, wins2) = qsim(self.p1start, self.p2start);
        let max = if wins1 > wins2 {wins1} else {wins2};
        Ok(max.to_string())
    }
}
