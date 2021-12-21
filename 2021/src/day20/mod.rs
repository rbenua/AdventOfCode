use crate::Problem;
use crate::read_lines;
use std::error::Error;
use regex::Regex;
extern crate string_error;
use string_error::new_err;
use parse_display::{Display, FromStr};
use std::collections::{HashMap, HashSet};


pub struct Day20{
    conv: Vec<bool>,
    input_img: Image,
}

#[derive(Debug, Clone)]
struct Image{
    known: HashMap<(i64, i64), bool>,
    default: bool,
}

fn nbrs((x, y): (i64, i64)) -> [(i64, i64); 9] {
    [(x-1, y-1), (x, y-1), (x+1, y-1),
     (x-1, y),   (x, y),   (x+1, y),
     (x-1, y+1), (x, y+1), (x+1, y+1)]
}

pub fn setup(_input:&str) -> Result<Day20, Box<dyn Error>>{
    let mut lines = read_lines(_input);
    let first_line = lines.next().unwrap()?;
    let conv = first_line.chars().map(|c|c == '#').collect();
    lines.next();
    let mut input_img = Image{
        known: HashMap::new(),
        default: false,
    };
    let mut y = 0;
    for line_opt in lines {
        let mut x = 0;
        for c in line_opt?.chars() {
            input_img.known.insert((x, y), c == '#');
            x += 1;
        }
        y += 1;
    }
    Ok(Day20{conv, input_img})
}

fn step(conv: &Vec<bool>, input_img: &Image) -> Image {
    let mut res = HashMap::new();
    let new_default = conv[if input_img.default {511} else {0}];

    for p in input_img.known.keys().map(|pp|nbrs(*pp)).flatten().collect::<HashSet<(i64,i64)>>() {
        let mut idx = 0;
        for n in nbrs(p) {
            idx <<= 1;
            idx += if *input_img.known.get(&n).unwrap_or(&input_img.default) {1} else {0};
        }
        if (idx == 0 && !input_img.default) || (idx == 511 && input_img.default) {
            continue;
        }
        res.insert(p, conv[idx as usize]);
    }

    Image{
        known: res,
        default: new_default,
    }
}

fn ch(i: bool) -> char {
    if i {'#'} else {'.'}
}
fn printgrid(img: &Image) {
    let mut ymin = 0;
    let mut ymax = 0;
    let mut xmin = 0;
    let mut xmax = 0;
    for (x, y) in img.known.keys() {
        if *x > xmax {
            xmax = *x;
        }
        if *x < xmin {
            xmin = *x;
        }
        if *y > ymax {
            ymax = *y;
        }
        if *y < ymin {
            ymin = *y;
        }
    } 
    let hash_size = img.known.len();
    let grid_size = (xmax - xmin) * (ymax - ymin);
    println!("default: {}, x={}..{}, y={}..{}, hash {} grid {}", ch(img.default), xmin, xmax, ymin, ymax, hash_size, grid_size);

    /*
    for y in ymin..(ymax+1) {
        println!("{}", (xmin..(xmax+1)).map(|x|{
            ch(*img.known.get(&(x, y)).unwrap_or(&img.default))
        }).collect::<String>());
    }
    */
}

impl Problem for Day20{
    fn part1(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut img = self.input_img.clone();
        //printgrid(&img);
        for _ in 0..2 {
            img = step(&self.conv, &img);
            //printgrid(&img);
        }
        if img.default {
            return Err(new_err("infinite result"));
        }
        let res = img.known.values().filter(|x|**x).count();
        Ok(res.to_string())
    }
    fn part2(&mut self, _input:&str) -> Result<String, Box<dyn Error>>{
        let mut img = self.input_img.clone();
        //printgrid(&img);
        for _i in 0..50 {
            img = step(&self.conv, &img);
            printgrid(&img);
        }
        if img.default {
            return Err(new_err("infinite result"));
        }
        let res = img.known.values().filter(|x|**x).count();
        Ok(res.to_string())
    }
}
