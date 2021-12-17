use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashSet;

pub struct Day17{
    target_x: (i64, i64),
    target_y: (i64, i64),
}

pub fn setup(_input:&str) -> Result<Day17, Box<dyn Error>>{
    let re = Regex::new(r"x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")?;
    let line = read_lines(_input).next().unwrap().unwrap();
    let cs = re.captures(&line).unwrap();
    let minx = cs[1].parse()?;
    let maxx = cs[2].parse()?;
    let miny = cs[3].parse()?;
    let maxy = cs[4].parse()?;
    Ok(Day17{
        target_x: (minx, maxx),
        target_y: (miny, maxy),
    })
}

fn find_height(v: i64) -> i64 {
    (1..v+1).sum()
}

fn y_times(yvel: i64, (miny, maxy): (i64, i64)) -> impl Iterator<Item=i64> {
    let (start_t, start_vel) = if yvel > 0 {
        (2 * yvel + 1, -(yvel + 1))
    }
    else {
        (0, yvel)
    };
    let mut min_t = None;
    let mut p = 0;
    let mut v = start_vel;
    let mut t = start_t;
    while p >= miny {
        if p <= maxy && min_t == None{
            min_t = Some(t);
        }
        p += v;
        v -= 1;
        t += 1;
    }
    match min_t {
        None => 0..0,
        Some(mt) => {
            //println!("y_times({}, ({}, {})) from {} to {}", yvel, miny, maxy, mt, t - 1);   
            mt..t
        },
    }
}

fn xpos(start_vel: i64, t: i64) -> i64 {
    let res = (1..(start_vel+1)).rev().take(t as usize).sum();
    //println!("xpos({}, {}) = {}", start_vel, t, res);
    res
}

fn x_vels(time: i64, (minx, maxx): (i64, i64)) -> impl Iterator<Item=i64> {
    let mut min = None;
    let mut max = 0;
    for start_vel in 0..(maxx+2) {
        let p = xpos(start_vel, time);
        if p > maxx {
            max = start_vel;
            break;
        }
        else if min == None && p >= minx {
            min = Some(start_vel);
        }
    }
    match min {
        None => {
            //println!("x_vels({}, ({}, {})) impossible", time, minx, maxx);
            0..0
        },
        Some(mt) => {
            //println!("x_vels({}, ({}, {})) from {} to {}: {}", time, minx, maxx, mt, max, max - mt + 1);
            mt..max
        },
    }
}

impl Problem for Day17{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let v = -(self.target_y.0 + 1);
        println!("{}", v);
        Ok(find_height(v).to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let miny = self.target_y.0;
        //let res: i64 = (miny..(-miny+1)).map(|y|y_times(y, self.target_y).map(|t|x_vels(t, self.target_x))).flatten().sum();
        let mut res = 0;
        for y in miny..(-miny+1) {
            let ts = y_times(y, self.target_y).collect::<Vec<i64>>();
            let xs = ts.iter().map(|t|x_vels(*t, self.target_x)).flatten().collect::<HashSet<i64>>();
            println!("y = {}, t = {:?}, x = {:?}", y, ts, xs);
            res += xs.len();
        }
        Ok(res.to_string())
    }
}
