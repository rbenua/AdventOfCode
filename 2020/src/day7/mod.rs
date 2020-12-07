use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashMap;
use std::collections::HashSet;

pub struct Day7{
    forward: HashMap<String, Vec<(String, usize)>>,
    reverse: HashMap<String, Vec<(String, usize)>>
}

pub fn setup(_input:&str) -> Result<Day7, Box<dyn Error>>{
    let mut res = Day7{
        forward: HashMap::new(),
        reverse: HashMap::new(),
    };
    let re = Regex::new(r"(?:^|(?P<count>\d+ ))(?P<color>[a-z ]+?) bag")?;
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        let mut captures = re.captures_iter(&line);
        let container = captures.next().unwrap()["color"].to_string();
        let mut forward_vec = Vec::new();
        for bag_type in captures {
            println!("count {}, color {}", &bag_type["count"], &bag_type["color"]);
            let num = bag_type["count"].trim().parse()?;
            let color = &bag_type["color"];
            forward_vec.push((color.to_string(), num));
            res.reverse.entry(color.to_string()).or_insert(Vec::new()).push((container.to_string(), num));
            res.reverse.entry(container.to_string()).or_insert(Vec::new());
        }
        res.forward.insert(container.to_string(), forward_vec);
    }
    println!("Forward: {:#?}", res.forward);
    println!("Reverse: {:#?}", res.reverse);
    Ok(res)   
}

fn p2_rec(start: &String, forward: &HashMap<String, Vec<(String, usize)>>) -> usize {
    let mut result = 1;
    for (inner, count) in &forward[start] {
        result += count * p2_rec(&inner, forward);
    }
    result
}


impl Problem for Day7{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut goals: HashSet<String> = HashSet::new();
        let mut checked: HashSet<String> = HashSet::new();
        goals.insert("shiny gold".to_string());
        let mut to_check = goals.difference(&checked).next();
        while to_check != None {
            let to_check_str = to_check.unwrap().to_string();
            for (goal, _) in &self.reverse[&to_check_str] {
                goals.insert(goal.to_string());
            }
            checked.insert(to_check_str);
            to_check = goals.difference(&checked).next();
        }
        Ok((checked.len() - 1).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        Ok((p2_rec(&"shiny gold".to_string(), &self.forward) - 1).to_string())
    }
}
