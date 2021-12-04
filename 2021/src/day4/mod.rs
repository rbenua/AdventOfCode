use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashSet;

#[derive(Debug)]
pub struct Day4{
    draws: Vec<i64>,
    boards: Vec<Vec<Vec<i64>>>,
}

pub fn setup(_input:&str) -> Result<Day4, Box<dyn Error>>{
    let mut lines = read_lines(_input);
    let first_line = lines.next().unwrap()?; 
    let draws = first_line.split(',').map(|s|{s.trim().parse().unwrap()}).collect::<Vec<i64>>();
    lines.next();
    let mut res = Day4{
        draws,
        boards: Vec::new(),
    };
    let mut cur_board = Vec::new();
    for line in lines {
        let line_opt = line?;
        if line_opt.trim() == "" {
            res.boards.push(cur_board);
            cur_board = Vec::new();
            continue; // suck it, joshua_
        }
        let cur_row = line_opt.split(' ').filter(|s|{*s != ""}).map(|s|{s.trim().parse().unwrap()}).collect::<Vec<i64>>();
        cur_board.push(cur_row);
    }
    res.boards.push(cur_board);
    Ok(res)
}

fn wins(board: &Vec<Vec<i64>>, drawn: &HashSet<i64>) -> bool {
    let row_wins = board.iter().any(|row|{
        row.iter().all(|i|{drawn.contains(i)})
    });
    let col_wins = (0..board[0].len()).any(|col|{
        board.iter().all(|row|{drawn.contains(&row[col])})
    });
    row_wins || col_wins
}

fn score(board: &Vec<Vec<i64>>, drawn: &HashSet<i64>, last: i64) -> i64 {
    let unmarked: i64 = board.iter().flatten().filter(|x|{!drawn.contains(x)}).sum();
    unmarked * last
}

impl Problem for Day4{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut drawn = HashSet::new();
        for d in &self.draws {
            drawn.insert(*d);
            if let Some(winner) = self.boards.iter().find(|board|{wins(board, &drawn)}) {
                return Ok(score(winner, &drawn, *d).to_string());
            }
        }
        Err(new_err("nobody won"))
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut drawn = HashSet::new();
        let mut non_winners = self.boards.clone();
        for d in &self.draws {
            drawn.insert(*d);
            if non_winners.len() > 1 {
                non_winners = non_winners.drain(..).filter(|board|{!wins(board, &drawn)}).collect();
            }
            else {
                if wins(&non_winners[0], &drawn) {
                    return Ok(score(&non_winners[0], &drawn, *d).to_string());
                }
            }
        }
        Err(new_err("not everybody won"))
    }
}
