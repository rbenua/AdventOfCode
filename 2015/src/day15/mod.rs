use crate::Problem;
use crate::read_lines;
use std::error::Error;
use std::fs::read;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use json::*;
use std::result::Result;
use std::collections::{HashSet, HashMap};
use permutator::k_permutation;

struct Ingredient{
    name: String,
    cap: i64,
    dur: i64,
    fla: i64,
    tex: i64,
    cal: i64,
}

pub struct Day15{
    ingredients: Vec<Ingredient>,
}

pub fn setup(_input:&str) -> Result<Day15, Box<dyn Error>>{
    let re = Regex::new(r"^(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)")?;
    let mut res = Day15{
        ingredients: Vec::new(),
    };
    for line in read_lines(_input){
        let l = line?;
        let cs = re.captures(&l).ok_or(new_err("bad format"))?;
        let mut i = Ingredient{
            name: cs[1].to_string(),
            cap: i64::from_str_radix(&cs[2], 10)?,
            dur: i64::from_str_radix(&cs[3], 10)?,
            fla: i64::from_str_radix(&cs[4], 10)?,
            tex: i64::from_str_radix(&cs[5], 10)?,
            cal: i64::from_str_radix(&cs[6], 10)?,
        };
        res.ingredients.push(i);
    }
    Ok(res)
}

fn score(ings: &[Ingredient], wts: &[i64], rcal: bool) -> i64{
    let mut tcap = 0;
    let mut tdur = 0;
    let mut tfla = 0;
    let mut ttex = 0;
    let mut tcal = 0;
    for (i, wt) in ings.iter().zip(wts){
        tcap += wt * i.cap;
        tdur += wt * i.dur;
        tfla += wt * i.fla;
        ttex += wt * i.tex;
        tcal += wt * i.cal;
    }
    if tcap < 0 || tdur < 0 || tfla < 0 || ttex < 0 || (rcal && tcal != 500) {
        return 0;
    }
    tcap * tdur * tfla * ttex
}

fn fnmax(ings: &[Ingredient], wts: &mut [i64], start: usize, rem: i64, rcal: bool) -> i64 {
    if start == wts.len() - 1 {
        wts[start] = rem;
        return score(ings, wts, rcal);
    }
    (0..rem+1).map(|i|{
        wts[start] = i;
        fnmax(ings, wts, start + 1, rem - i, rcal)
    }).max().unwrap()
}

impl Problem for Day15{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut weights: Vec<i64> = vec![0; self.ingredients.len()];
        let m = fnmax(&self.ingredients, &mut weights, 0, 100, false);
        Ok(m.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut weights: Vec<i64> = vec![0; self.ingredients.len()];
        let m = fnmax(&self.ingredients, &mut weights, 0, 100, true);
        Ok(m.to_string())
    }
}
