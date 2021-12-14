use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Day14{
    start_string: Vec<char>,
    rules: HashMap<(char, char), char>,
}

pub fn setup(_input:&str) -> Result<Day14, Box<dyn Error>>{
    let re = Regex::new(r"(\w)(\w) -> (\w)")?;
    let mut lines = read_lines(_input);
    let start_string = lines.next().unwrap().unwrap().chars().collect();
    let mut rules = HashMap::new();
    lines.next();
    for line_opt in lines {
        let line = line_opt?;
        let cs = re.captures(&line).unwrap();
        let c1 = cs[1].chars().next().unwrap();
        let c2 = cs[2].chars().next().unwrap();
        let c3 = cs[3].chars().next().unwrap();
        rules.insert((c1, c2), c3);
    }
    Ok(Day14{
        start_string,
        rules,
    })
}

fn apply(string: &mut Vec<char>, rules: &HashMap<(char, char), char>) {
    let mut i = 0;
    while i < string.len() - 1 {
        let pat = (string[i], string[i+1]);
        if let Some(ins) = rules.get(&pat) {
            i += 1;
            string.insert(i, *ins);
        }
        i += 1;
    }
}

fn apply2(pair_counts: &HashMap<(char, char), u64>, 
          freqs: &mut HashMap<char, u64>,
          rules: &HashMap<(char, char), char>) -> HashMap<(char, char), u64> {
    let mut res = HashMap::new();
    for ((a, b), int) in rules {
        if let Some(count) = pair_counts.get(&(*a, *b)) {
            *freqs.entry(*int).or_insert(0) += count;
            *res.entry((*a, *int)).or_insert(0) += count;
            *res.entry((*int, *b)).or_insert(0) += count;
        }
    }
    res
}

impl Problem for Day14{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut string = self.start_string.clone();
        println!("{}", string.iter().collect::<String>());
        for _i in 0..10 {
            apply(&mut string, &self.rules);
            //println!("{}: {}", _i, string.iter().collect::<String>());
        }
        let mut freqs = HashMap::new();
        for c in &string {
            *freqs.entry(*c).or_insert(0) += 1;
        }
        let min = freqs.iter().min_by_key(|&(_, v)|v).unwrap();
        let max = freqs.iter().max_by_key(|&(_, v)|v).unwrap();
        println!("min: {:?}, max: {:?}", min, max);
        Ok((max.1 - min.1).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut freqs = HashMap::new();
        for c in &self.start_string {
            *freqs.entry(*c).or_insert(0) += 1;
        }
        let mut pair_counts = HashMap::new();
        for i in 0..(self.start_string.len() - 1) {
            *pair_counts.entry((self.start_string[i], self.start_string[i+1])).or_insert(0) +=1 ;
        }

        for _i in 0..40 {
            pair_counts = apply2(&pair_counts, &mut freqs, &self.rules);
        }

        let min = freqs.iter().min_by_key(|&(_, v)|v).unwrap();
        let max = freqs.iter().max_by_key(|&(_, v)|v).unwrap();
        println!("min: {:?}, max: {:?}", min, max);
        Ok((max.1 - min.1).to_string())
    }
}
