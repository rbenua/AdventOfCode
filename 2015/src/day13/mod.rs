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

pub struct Day13{
    people: Vec<String>,
    rels: HashMap<String, HashMap<String, i64>>,    
}

pub fn setup(_input:&str) -> Result<Day13, Box<dyn Error>>{
    let re = Regex::new(r"^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.")?;
    let mut rels = HashMap::new();
    let mut people = HashSet::new();
    for line in read_lines(_input){
        let l = line?;
        let cs = re.captures(&l).ok_or(new_err("bad format"))?;
        let src = cs[1].to_string();
        let dst = cs[4].to_string();
        let neg = &cs[2] == "lose";
        let mut amt = i64::from_str_radix(&cs[3], 10)?;
        if neg {
            amt *= -1;
        }
        people.insert(src.clone());
        rels.entry(src).or_insert(HashMap::new()).insert(dst, amt);
    }
    let res = Day13{
        people: people.drain().collect(),
        rels: rels,
    };
    Ok(res)
}

fn calculate(perm: &[&String], rels: &HashMap<String, HashMap<String, i64>>) -> i64 {
    let l = perm.len();
    let mut total = 0;
    for i in 0..l {
        total += rels[perm[i]][perm[(i+1)%l]];
        total += rels[perm[i]][perm[(i+l-1)%l]];
    }
    total
}


impl Problem for Day13{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut max_value: i64 = 0;
        k_permutation(&self.people, self.people.len(), |perm|{
            let t = calculate(perm, &self.rels);
            if t > max_value {
                max_value = t;
            }
        });
        Ok(max_value.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut me_map = HashMap::new();
        for p in &self.people {
            self.rels.get_mut(p).unwrap().insert("Me".to_string(), 0);
            me_map.insert(p.clone(), 0);
        }
        self.rels.insert("Me".to_string(), me_map);
        self.people.push("Me".to_string());
        let mut max_value: i64 = 0;
        k_permutation(&self.people, self.people.len(), |perm|{
            let t = calculate(perm, &self.rels);
            if t > max_value {
                max_value = t;
            }
        });
        Ok(max_value.to_string())
    }
}
