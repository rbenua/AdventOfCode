use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::HashSet;

#[derive(Debug, Copy, Clone)]
struct Region{
    x: (i64, i64),
    y: (i64, i64),
    z: (i64, i64),
    on: bool    
}

impl std::fmt::Display for Region{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} x={:?},y={:?},z={:?}", if self.on {"on"} else {"off"}, self.x, self.y, self.z)
    }
}

pub struct Day22{
    regions: Vec<Region>,
}

pub fn setup(_input:&str) -> Result<Day22, Box<dyn Error>>{
    let re = Regex::new(r"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)$")?;
    let mut regions = Vec::new();
    for line_opt in read_lines(_input) {
        let line = line_opt?;
        let cs = re.captures(&line).ok_or(new_err(&format!("regex didn't match {}", line)))?;
        let on = &cs[1] == "on";
        let x = (cs[2].parse()?, cs[3].parse()?);
        let y = (cs[4].parse()?, cs[5].parse()?);
        let z = (cs[6].parse()?, cs[7].parse()?);
        regions.push(Region{on, x, y, z});
    }
    Ok(Day22{regions})
}

fn clamp(min: i64, val: i64, max: i64) -> i64 {
    if min > val {
        min
    }
    else if max < val {
        max
    }
    else {
        val
    }
}

fn out_of_bounds(reg: &Region) -> bool {
       reg.x.0 > 50 || reg.x.1 < -50
    || reg.y.0 > 50 || reg.y.1 < -50
    || reg.z.0 > 50 || reg.z.1 < -50
}

fn step(reg: &Region, map: &mut HashSet<(i64, i64, i64)>) {
    if out_of_bounds(reg) {
        return;
    }
    for x in clamp(-50, reg.x.0, 50)..(clamp(-50, reg.x.1, 50)+1) {
        for y in clamp(-50, reg.y.0, 50)..(clamp(-50, reg.y.1, 50)+1) {
            for z in clamp(-50, reg.z.0, 50)..(clamp(-50, reg.z.1, 50)+1) {
                if reg.on {
                    map.insert((x, y, z));
                }
                else {
                    map.remove(&(x, y, z));
                }
            }
        }
    }
}

fn contains_region(r1: &Region, r2: &Region) -> bool {
       r1.x.0 <= r2.x.0 && r1.x.1 >= r2.x.1 
    && r1.y.0 <= r2.y.0 && r1.y.1 >= r2.y.1 
    && r1.z.0 <= r2.z.0 && r1.z.1 >= r2.z.1 
}

fn max(a: i64, b: i64) -> i64 {
    if a > b {a} else {b}
}
fn min(a: i64, b: i64) -> i64 {
    if a < b {a} else {b}
}
fn does_intersect(r1: &Region, r2: &Region) -> bool {
       (r1.x.1 >= r2.x.0 && r1.x.0 <= r2.x.1)
    && (r1.y.1 >= r2.y.0 && r1.y.0 <= r2.y.1)
    && (r1.z.1 >= r2.z.0 && r1.z.0 <= r2.z.1)
}
fn intersection(r1: &Region, r2: &Region) -> Option<Region> {
    if does_intersect(r1, r2) {
        Some(Region{
            x: (max(r1.x.0, r2.x.0), min(r1.x.1, r2.x.1)),
            y: (max(r1.y.0, r2.y.0), min(r1.y.1, r2.y.1)),
            z: (max(r1.z.0, r2.z.0), min(r1.z.1, r2.z.1)),
            on: !r1.on
        })
    }
    else {
        None
    }
}

fn vol(r: &Region) -> i64 {
    let res = 
    (r.x.1 - r.x.0 + 1) * 
    (r.y.1 - r.y.0 + 1) * 
    (r.z.1 - r.z.0 + 1) *
    if r.on {1} else {-1};
    //println!("vol({}) = {}", r, res);
    res
}

fn add_region(r: Region, vols: &mut Vec<Region>) {
    //println!("adding region {}", r);
    let mut new_vols = Vec::new();
    for other in vols.iter() {
        if let Some(int) = intersection(other, &r) {
            //println!("intersection with {}: {} ({})", other, int, vol(&int));
            new_vols.push(int);
        }
    }
    vols.extend(new_vols.drain(..));
    if r.on {
        vols.push(r);
    }
}


impl Problem for Day22{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut map = HashSet::new();
        for reg in &self.regions {
            step(reg, &mut map);
        }
        Ok(map.len().to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut vols = Vec::new();
        for region in &self.regions {
            add_region(*region, &mut vols);
        }
        let total = vols.iter().map(vol).sum::<i64>();
        Ok(total.to_string())
    }
}
