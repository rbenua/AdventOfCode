use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

pub struct Day3{
    numbers: Vec<u64>,
    bitlen: u64,
}

pub fn setup(_input:&str) -> Result<Day3, Box<dyn Error>>{
    let mut res = Day3{
        numbers: Vec::new(),
        bitlen: 0,
    };
    for line in read_lines(_input) {
        let line_opt = line?;
        res.numbers.push(u64::from_str_radix(line_opt.trim(), 2)?);
        let cur_len = line_opt.trim().len() as u64;
        if cur_len > res.bitlen {
            res.bitlen = cur_len;
        }
    }
    Ok(res)
}

fn gamma_epsilon(vals: &[u64], bitlen: u64) -> (u64, u64) {
    let mut gamma = 0u64;
    for bit in 0..bitlen {
        let mask = 1u64 << bit;
        let total: u64 = vals.iter().map(|x|{(x & mask) as u64}).sum::<u64>() >> bit;
        if (total as usize) >= vals.len() / 2 {
            gamma |= mask;
        }
    }
    let epsilon = !gamma & (1 << (bitlen)) - 1;
    (gamma, epsilon)
}

impl Problem for Day3{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let (gamma, epsilon) = gamma_epsilon(&self.numbers, self.bitlen);
        println!("gamma: {}, epsilon: {}", gamma, epsilon);
        Ok((epsilon * gamma).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut gamma_remaining = self.numbers.clone();
        let mut epsilon_remaining = self.numbers.clone();
        for start in (0..self.bitlen).rev() {
            let (gamma, _epsilon) = gamma_epsilon(&gamma_remaining, self.bitlen);
            let mask = 1 << start;
            gamma_remaining = gamma_remaining.drain(..).filter(|x|{x & mask == gamma & mask}).collect();
            if gamma_remaining.len() == 1 {
                break;
            }
        }

        for start in (0..self.bitlen).rev() {
            let (_gamma, epsilon) = gamma_epsilon(&epsilon_remaining, self.bitlen);
            let mask = 1 << start;
            epsilon_remaining = epsilon_remaining.drain(..).filter(|x|{x & mask == epsilon & mask}).collect();
            if epsilon_remaining.len() == 1 {
                break;
            }
        }
        Ok((gamma_remaining[0] * epsilon_remaining[0]).to_string())
    }
}
