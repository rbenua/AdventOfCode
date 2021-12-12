use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::{HashMap, HashSet};

pub struct Day12{
    paths: HashMap<String, Vec<String>>,
}

pub fn setup(_input:&str) -> Result<Day12, Box<dyn Error>>{
    let mut paths = HashMap::new();
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        let mut it = line.trim().split('-');
        let fst = it.next().unwrap().to_string();
        let snd = it.next().unwrap().to_string();
        paths.entry(fst.clone()).or_insert(Vec::new()).push(snd.clone());
        paths.entry(snd).or_insert(Vec::new()).push(fst);
    }
    println!("{:?}", paths);
    Ok(Day12{paths})
}

fn is_lower(s: &str) -> bool {
    s.chars().next().unwrap().is_lowercase() && s != "end"
}

impl Problem for Day12{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut total: i64 = 0;
        let mut to_visit = Vec::new();
        to_visit.push(("start".to_string(), HashSet::new()));
        while !to_visit.is_empty() {
            let (curr, mut visited_lower) = to_visit.pop().unwrap();
            if is_lower(&curr) {
                visited_lower.insert(curr.to_string()); 
            }
            if &curr == "end" {
                total += 1;
            }
            else if let Some(v) = self.paths.get(&curr) {
                for next in v {
                    if !visited_lower.contains(next) {
                        to_visit.push((next.clone(), visited_lower.clone()));
                    }
                }
            }
            //println!("visited {}, to_visit {:?}", curr, to_visit);
        }
        Ok(total.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut total: i64 = 0;
        let mut to_visit = Vec::new();
        let mut init_visited = HashSet::new();
        init_visited.insert("start".to_string());
        to_visit.push(("start".to_string(), init_visited, false));
        while !to_visit.is_empty() {
            let (curr, mut visited_lower, dup_lower) = to_visit.pop().unwrap();
            if is_lower(&curr) {
                visited_lower.insert(curr.to_string()); 
            }
            if &curr == "end" {
                total += 1;
            }
            else if let Some(v) = self.paths.get(&curr) {
                for next in v {
                    if next == "start" {
                        continue;
                    }
                    let next_vis = visited_lower.contains(next);
                    if next_vis && !dup_lower {
                        to_visit.push((next.clone(), visited_lower.clone(), true));
                    }
                    else if !next_vis {
                        to_visit.push((next.clone(), visited_lower.clone(), dup_lower));
                    }
                }
            }
            //println!("visited {}, to_visit {:?}", curr, to_visit);
        }
        Ok(total.to_string())
    }
}
