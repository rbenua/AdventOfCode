use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};

#[derive(Display, FromStr, Debug, PartialEq, Clone)]
#[display("{name}: {min1}-{max1} or {min2}-{max2}")]
struct Field {
    name: String,
    min1: u64,
    max1: u64,
    min2: u64,
    max2: u64,
}

impl Field {
    fn valid(&self, val: u64) -> bool {
        (val >= self.min1 && val <= self.max1) || (val >= self.min2 && val <= self.max2)
    }
}

#[derive(Debug)]
pub struct Day16{
    fields: Vec<Field>,
    my_ticket: Vec<u64>,
    other_tickets: Vec<Vec<u64>>,
}

pub fn setup(_input:&str) -> Result<Day16, Box<dyn Error>>{
    let mut res = Day16{
        fields: Vec::new(),
        my_ticket: Vec::new(),
        other_tickets: Vec::new(),
    };
    let mut lines = read_lines(_input);
    loop {
        let line = lines.next().unwrap()?;
        if line.trim().len() == 0 {
            break;
        }
        res.fields.push(line.trim().parse()?);
    }
    lines.next();
    let my_line = lines.next().unwrap()?;
    res.my_ticket = my_line.trim().split(',').map(|x|{x.parse().unwrap()}).collect();

    lines.next();
    lines.next();

    for other_line in lines {
        res.other_tickets.push(other_line?.trim().split(',').map(|x|{x.parse().unwrap()}).collect());
    }
    Ok(res)
}

impl Problem for Day16{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut total = 0;
        for ticket in &self.other_tickets {
            for val in ticket {
                let mut valid = false;
                for field in &self.fields {
                    if field.valid(*val) {
                        valid = true;
                        break;
                    }
                }
                if !valid {
                    total += *val;
                }
            }
        }
        Ok(total.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut i = 0;
        while i < self.other_tickets.len() {
            let ticket = &self.other_tickets[i];
            let mut valid = false;
            for val in ticket {
                valid = false;
                for field in &self.fields {
                    if field.valid(*val) {
                        valid = true;
                        break;
                    }
                }
                if !valid {
                    break;
                }
            }
            if !valid {
                self.other_tickets.remove(i);
            }
            else {
                i += 1;
            }
        }
        self.other_tickets.push(self.my_ticket.clone());

        let mut possible:Vec<Vec<Field>> = Vec::new();
        for i in 0..self.my_ticket.len() {
            let v = self.fields.iter().filter(|f|{
                self.other_tickets.iter().all(|ticket|{
                    f.valid(ticket[i])
                })
            }).map(|v|{v.clone()}).collect();
            possible.push(v);
        }

        while possible.iter().map(|v|{v.len()}).max() > Some(1) {
            let size = possible.len();
            for i in 0..size {
                if possible[i].len() == 1 {
                    let name = possible[i][0].name.clone();
                    for j in 0..size {
                        if j != i {
                            if let Some(p) = possible[j].iter().position(|f|{f.name == name}) {
                                possible[j].remove(p);
                            }
                        }
                    }
                }
            }
        }
        let mut result = 1;
        for (i, _) in possible.iter().enumerate().filter(|(_, fs)|{fs[0].name.starts_with("departure")}) {
            result *= self.my_ticket[i];
        }
        Ok(result.to_string())
    }
}
