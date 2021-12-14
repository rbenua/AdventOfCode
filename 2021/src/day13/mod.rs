use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashSet;

#[derive(Debug, Clone)]
enum Fold{
    X(i64),
    Y(i64),
}

#[derive(Debug)]
pub struct Day13{
    points: HashSet<(i64, i64)>,
    folds: Vec<Fold>,
}

pub fn setup(_input:&str) -> Result<Day13, Box<dyn Error>>{
    let mut res = Day13{
        points: HashSet::new(),
        folds: Vec::new(),
    };
    let fold_re = Regex::new(r"fold along ([xy])=(\d+)")?;
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        if let Some(cs) = fold_re.captures(&line) {
            let pos = cs.get(2).unwrap().as_str().parse()?;
            if "x" == cs.get(1).unwrap().as_str() {
                res.folds.push(Fold::X(pos));
            }
            else {
                res.folds.push(Fold::Y(pos));
            }
        }
        else if line.len() > 1 {
            let mut it = line.split(',');
            let x = it.next().unwrap().parse()?;
            let y = it.next().unwrap().parse()?;
            res.points.insert((x, y));
        }
    }
    //println!("{:?}", res);
    Ok(res)
}

fn fold_outside(p: &(i64, i64), fold: &Fold) -> bool {
    match fold {
        Fold::X(x) => p.0 > *x,
        Fold::Y(y) => p.1 > *y,
    }
}

fn fold(points: &mut HashSet<(i64, i64)>, fold: &Fold){
    let mut newpts = Vec::new();
    for (x, y) in points.drain_filter(|p|{fold_outside(p, &fold)}) {
        let new = match fold {
            Fold::X(fx) => (fx - (x - fx), y),
            Fold::Y(fy) => (x, fy - (y - fy))
        };
        newpts.push(new);
    }
    points.extend(newpts);
}

fn print_grid(points: &HashSet<(i64, i64)>) {
    let ymax = *points.iter().map(|(_,y)|{y}).max().unwrap();
    let ymin = *points.iter().map(|(_,y)|{y}).min().unwrap();
    let xmax = *points.iter().map(|(x,_)|{x}).max().unwrap();
    let xmin = *points.iter().map(|(x,_)|{x}).min().unwrap();
    for y in ymin..(ymax+1) {
        println!("{}", (xmin..(xmax+1)).map(|x|{
            if points.contains(&(x, y)) {
                '#'
            }
            else {
                ' '
            }
        }).collect::<String>())
    }
}

impl Problem for Day13{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        fold(&mut self.points, &self.folds[0]);
        Ok(self.points.len().to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        for f in &self.folds {
            fold(&mut self.points, f);
        }
        print_grid(&self.points);
        Ok("".to_string())
    }
}
