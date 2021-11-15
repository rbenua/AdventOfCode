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

#[derive(Debug)]
pub struct Day19{
    rules: Vec<(String, String)>,
    init_string: String,
}

pub fn setup(_input:&str) -> Result<Day19, Box<dyn Error>>{
    let re = Regex::new(r"(\w+) => (\w+)")?;
    let mut res = Day19{
        rules: Vec::new(),
        init_string: "".to_string(),
    };
    for line in read_lines(_input){
        let l = line?;
        if let Some(cs) = re.captures(&l){
            res.rules.push((cs[1].to_string(), cs[2].to_string()));
        }
        else if l.trim().len() > 0 {
            res.init_string = l.trim().to_string();
        }
    }
    Ok(res)
}
fn scan(init: &String, rules: &[(String, String)]) -> HashSet<String> {
    let mut res = HashSet::new();
    for (src, dest) in rules {
        for (i, _) in init.match_indices(src){
            let mut new = init.clone();
            new.replace_range(i..i+src.len(), &dest); 
            res.insert(new);
        }
    }
    res
}

fn dfs(curr: &String, target: &str, curr_depth: i64, rules: &[(String, String)]) -> Option<i64> {
    if curr == target {
        return Some(curr_depth);
    }
    let s = scan(curr, rules);
    println!("Generation {}, curr {}, next {}", curr_depth, curr, s.len());
    for next in s {
        let r = dfs(&next, target, curr_depth + 1, rules);
        if r.is_some() {
            return r;
        }
    }
    None
}

impl Problem for Day19{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok(scan(&self.init_string, &self.rules).len().to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let rev_rules = self.rules.iter().map(|(x,y)|{(y.clone(),x.clone())}).collect::<Vec<(String,String)>>();
        let res = dfs(&self.init_string, &"e", 0, &rev_rules);
        Ok(format!("{:?}", res).to_string())
    }
}